/**
 * Purity analysis for Simple Java. Implemented as described in
 * 'A Combined Pointer and Purity Analysis for Java Programs'. Any
 * Page references in the source file are references to that paper.
 *
 * @author Mads Hartmann Jensen
 */

package dk.itu.sdg.analysis

import dk.itu.sdg.analysis.CallGraph.{ Invocation }
import dk.itu.sdg.javaparser._

import scala.collection.immutable.{HashSet, HashMap}
import scala.collection.mutable.{Queue}

object Purity {

  /*
   *  public interface
   *  ====================================
   */

  def isPure(className: String, invokable: SJInvokable): Boolean =
    modifiedAbstractFields(className, invokable).isEmpty

  def modifiedAbstractFields(className: String, invokable: SJInvokable): Set[AbstractField] =
    analysis(className, invokable).modifiedFields

  // for testing and debug purposes
  def getState(className: String, invokable: SJInvokable): Result =
    analysis(className, invokable)

  // For testing and debug purposes. Analyzes a single method w/o analyzing called funcions.
  def analyzePartial(className: String, invokable: SJInvokable): Result =
    analysisPartial(className, invokable)

  /*
   *  implementation details
   *  ====================================
   */

  /*
   *  Data structures and types
   */

  type Mapping = Node => Set[Node]

  private def empty[B] = HashSet[B]()

  // TODO: Fix when I generate unique labels for each statement.
  // name of special variable that points to the nodes returned
  // form a method. See page page 9 before section 5.3
  val RETURN_LABEL = "RETURN"

  trait Node { val lb: String }
  case class InsideNode(lb: String) extends Node
  case class LoadNode(lb: String) extends Node
  case class EscapedNode(lb: String) extends Node
  case class ParameterNode(lb: String) extends Node

  trait Edge {
    val n1: Node
    val field: String
    val n2: Node
  }

  case class InsideEdge(n1: Node, field: String, n2: Node) extends Edge
  case class OutsideEdge(n1: Node, field: String, n2: Node) extends Edge

  case class AbstractField(node: Node, field: String)

  case class PTGraph(
    insideEdges         : Set[InsideEdge],
    outsideEdges        : Set[OutsideEdge],
    stateOflocalVars    : Map[String, Set[_ <: Node]],
    globallyEscapedNodes: Set[EscapedNode]) {

    // Using BFS for reachability. Has to check taking each of the nodes as the root
    def isReachable(node: Node, from: Node): Boolean = {
      val edges = this.insideEdges ++ this.outsideEdges
      var queue = Queue(from)
      var visisted = from :: Nil
      var v: Node = null
      while (!queue.isEmpty) {
        v = queue.dequeue
        if (v == node) return true
        edges.collect {
          case InsideEdge(vertex1,_,vertex2) if vertex1 == v && !visisted.contains(vertex2) => vertex2
          case OutsideEdge(vertex1,_,vertex2) if vertex1 == v && !visisted.contains(vertex2) => vertex2
        }.foreach { w => // for each edge e incident on v in PTGraph:
          visisted = w :: visisted
          queue.enqueue(w)
        }
      }
      return false
    }

    // Returns a set with all of the nodes in the graph.
    def nodes: Set[Node] = {

      val inEdges: Set[Node] =
        (for { InsideEdge(n1,_,n2)  <- insideEdges  } yield HashSet(n1,n2)).flatten ++
        (for { OutsideEdge(n1,_,n2) <- outsideEdges } yield HashSet(n1,n2)).flatten

      val variables: Set[Node] = stateOflocalVars.map{ case (k,v) => v }.foldLeft(HashSet[Node]()){ _ ++ _ }

      variables ++ inEdges

    }

  }

  case class Result(
    pointsToGraph : PTGraph,
    modifiedFields: Set[AbstractField]
  ) {
    def prettyPrint() = {
      println("NODES")
      println(this.pointsToGraph.nodes.mkString("\n"))
      println("EDGES")
      println(this.pointsToGraph.outsideEdges.mkString("\n"))
      println(this.pointsToGraph.insideEdges.mkString("\n"))
      println("VARIABLES")
      println(this.pointsToGraph.stateOflocalVars)
      println("MODIFIED")
      println(modifiedFields)
    }
  }

  /*
   *  Methods
   */

  /**
   * Main entry point for the analysis.
   *
   * @param className The name of the class that contains the method to analyze. At
   *                  some points this should be moved into SJMethodDefinition
   * @param method    The method to analyze.
   * @return          A points-to graph and set of abstract fields that are modified by the
   *                  the method.
   */
  def analysis(className: String, invokable: SJInvokable): Result = {

    val callGraph  = AST.extractCallGraph(className, invokable)
    val components = CallGraph.components(callGraph)

    // Keep track of the result of analyzing the method
    var analyzedMethods = HashMap[Invocation, Result]()

    components.foreach { component =>

      // Keep track of the methods that still needs to be analyzed
      var worklist: List[Vertex[Invocation]] = component

      while (worklist.nonEmpty) {

        val invocation = worklist.head.item
        val invoked = getInvokable(invocation)

        val oldPTState = analyzedMethods.getOrElse(invocation, initialStateOfMethod(invoked))
        val newPTState = intraProcedural(invoked, analyzedMethods)

        if (oldPTState != newPTState) {
          for(Edge(from,`invocation`,_) <- callGraph.edges) { worklist = from :: worklist }
          analyzedMethods = analyzedMethods.updated(invocation, newPTState)
        }

        worklist = worklist.tail
      }
    }

    analyzedMethods(callGraph.start.item)
  }

  def analysisPartial(className: String, invokable: SJInvokable): Result = {
    val invocation = (className, invokable.id)
    val analyzedMethods = HashMap( (invocation -> initialStateOfMethod(invokable)))
    intraProcedural(invokable, analyzedMethods)
  }

  /**
   * Constructs the node mapping. Explained on page 9
   *
   * @param g           The Points-to graph of the method that is currently being analyzed
   * @param gCallee     The Points-to graph of the invoked method
   * @param parameters  The name of the parameters of the invoked method
   * @param arguments   The names of the variables passed as arguments to the method
   * @param recievers   The set of nodes that 'this' should map to.
   * @return A mapping from nodes in 'gCallee' to nodes in 'g'
   */
  private def mapping(g: PTGraph,
                      gCallee: PTGraph,
                      invokable: SJInvokable,
                      arguments: List[String],
                      recievers: Set[Node]): Node => Set[Node] = {

    def mergeMap(m1: Map[Node, Set[Node]], m2: Map[Node, Set[Node]]): Map[Node, Set[Node]] = {
      // For the keys where previous values exist we merge the values, for the keys
      // where no previous values exist we simply add them to the map using '++'
      val (oldMap,newMap) = m2.partition { case (k,v) => m1.contains(k) }
      m1.map { case (k,v) => ( k -> (v ++ oldMap.getOrElse(k,empty)) ) } ++ newMap
    }

    var map: Map[Node, Set[Node]] = HashMap()

    // Mapping 1
    // The parameter nodes of gCallee should map to the nodes pointed to by the
    // arguments used to invoke callee.
    val mapping1Map: Map[Node, Set[Node]] = {
      // the set of nodes each each parameter points to
      val parameterNodes: List[Set[Node]] =
        (for { SJArgument(id, _) <- invokable.parameters } yield gCallee.stateOflocalVars(id)).asInstanceOf[List[Set[Node]]]
      // the set of nodes that each argument points to
      val args: List[Set[Node]] =
        (for { id <- arguments } yield g.stateOflocalVars(id)).asInstanceOf[List[Set[Node]]]

      val xxx: List[Pair[Node,Set[Node]]] = {
        parameterNodes.flatMap { ps: Set[Node] =>
          args.flatMap { ars: Set[Node] =>
            ps.map { p: Node => (p -> ars) }
          }
        }
      }

      val tuples = xxx ++ List((ParameterNode("%s:this".format(invokable.id)),recievers))
      HashMap(tuples :_*)
    }

    map = mergeMap(map,mapping1Map)

    // Mapping 2
    // All outside nodes of gCallee that point to inside nodes of G should be
    // mapped to those nodes.
    val mapping2Map: Map[Node, Set[Node]] = {
      (for {
        OutsideEdge(n1, f, n2) <- gCallee.outsideEdges
        InsideEdge(n3, `f`, n4)  <- g.insideEdges if map.getOrElse(n1, empty).contains(n3)
      } yield (n2 -> n4)).groupBy( _._1 ).map {
          case (key,value) => { (key -> value.map( _._2 ).toSet ) }
      }
    }

    map = mergeMap(map,mapping2Map)

    // Mapping 3
    // This constraint deals with the aliasing present in the calling context
    val mapping3Map: Map[Node, Set[Node]] = {
      val loadNodes = for { OutsideEdge(n1, f, n2) <- g.outsideEdges if n2.isInstanceOf[LoadNode]} yield n2

      (for {
        OutsideEdge(n1, f, n2) <- gCallee.outsideEdges
        InsideEdge(n3, `f`, n4)  <- gCallee.insideEdges if n1 != n3 &&
                                                         n1.isInstanceOf[LoadNode] &&
                                                         (map.getOrElse(n1,empty) + n1).intersect( (map.getOrElse(n3,empty) + n3)).nonEmpty
        extra = if (n4.isInstanceOf[ParameterNode]) empty else HashSet(n4)
      } yield (n2 -> (map.getOrElse(n4,empty) ++ extra))).groupBy( _._1 ).map { case (key, value) =>
          (key -> value.map( _._2 ).toSet.flatten)
      }
    }

    map = mergeMap(map, mapping3Map)

    // Mapping 4
    // Each non-parameter node should map to itself
    val mapping4Map: Map[Node,Set[Node]] = {
      val nonParameterNodes = gCallee.nodes.filter( !_.isInstanceOf[ParameterNode])
      HashMap( nonParameterNodes.zip(nonParameterNodes.map( HashSet(_) )).toList :_* )
    }

    map = mergeMap(map, mapping4Map)

    // continue mapping until a fixed-point has been reached
    (n: Node) => {
      var workList: List[Node] = n :: Nil
      var result: List[Node] = Nil
      while(!workList.isEmpty) {
        val node = workList.head
        val mapped = map.getOrElse(node, empty).toList
        if (!mapped.isEmpty) {
          if (result.contains(node)) {
            workList = workList.tail
          } else if (mapped == List(node)) { // Mapped to itself
            workList = workList.tail
            result = result ++ mapped
          } else {
            workList = workList.tail ++ mapped
            result = (result.filterNot (_ == node)) ++ mapped // the node we just mapped  shouldn't be part of the result anymore
          }
        } else {
          workList = workList.tail
        }
      }
      result.toSet
    }
  }

  /**
   * Combines two points-to-graphs. Explained on page 10.
   *
   * @param g       The Points-to graph of the currently analyzed method
   * @param gCallee The Points-to graph of the called method
   * @param mapping A mapping from nodes in gCallee to nodes in g
   * @param varName The name of the variable the result of calling the function
   *                should be stored in
   *
   * @return The result of combining 'g' and 'gCallee'
   */
  private def combine(g: PTGraph,
                      gCallee: PTGraph,
                      mapping: Mapping,
                      varName: Option[SJVariableAccess] ): PTGraph = {

    val insideEdges = for {
      InsideEdge(n1,f,n2) <- gCallee.insideEdges
      mappedN1            <- mapping(n1)
      mappedN2            <- mapping(n2)
    } yield InsideEdge(mappedN2, f, mappedN1)

    val outsideEdges = for {
      OutsideEdge(n1, f, n2) <- gCallee.outsideEdges
      mappedN1               <- mapping(n1)
    } yield OutsideEdge(mappedN1, f, n2)


    val newStateOfLocalVars = (for {
      SJVariableAccess(name) <- varName
    } yield {
      g.stateOflocalVars.updated(name, for {
        node   <- gCallee.stateOflocalVars.getOrElse(RETURN_LABEL, empty)
        mapped <- mapping(node)
      } yield mapped)
    }).getOrElse(g.stateOflocalVars)

    val globallyEscapedNodes = (for {
      node   <- gCallee.globallyEscapedNodes
      mapped <- mapping(node)
    } yield mapped).asInstanceOf[Set[EscapedNode]] // TODO: Don't know if this is sound yet.

    PTGraph(
      insideEdges          = insideEdges ++ g.insideEdges,
      outsideEdges         = outsideEdges ++ g.outsideEdges,
      stateOflocalVars     = newStateOfLocalVars,
      globallyEscapedNodes = g.globallyEscapedNodes ++ globallyEscapedNodes
    )
  }

  /**
   * Simplifies the points-to-graph by removing all captured load nodes, together with
   * all adjacent edges and all outside edges that start in a captured node. Explained in
   * section "Points-to Graph Simplifications" on page 10.
   *
   * @param graph The graph to simplify.
   * @return The simplified graph.
   *
   * TODO: Implement
   */
  private def simplify(graph: PTGraph): PTGraph = {
    graph
  }

  /**
   * Calculate all of the modified abstract fields that are relevant for the caller of a
   * method (i.e. all nodes that were modified that aren't InsideNodes.)
   *
   * @param calleeModified          The set of abstract fields modified by called method
   * @param nodesInSimplifiedGraph  The nodes in the simplified graph
   * @param mapping                 The mapping used to map nodes from the called points-to graph
   *                                to the callers nodes.
   * @return                        The modified abstract fields mapped to the correct nodes.
   */
  private def mapModifiedAbstractFields(calleeModified: Set[AbstractField],
                                        nodesInSimplifiedGraph: Set[Node],
                                        mapping: Mapping): Set[AbstractField] = {
    for {
      AbstractField(n, f) <- calleeModified
      node <- mapping(n) if !node.isInstanceOf[InsideNode] && nodesInSimplifiedGraph.contains(node)
    } yield AbstractField(node, f)
  }

  /**
   * Analyze a single method.
   *
   * Traverse the body from top to bottom transforming the Points-to-Graph on every
   * statement on the way and (possibly) adding AbstractField to the writes set
   *
   * @param  invokable The method to analyze
   * @param  analyzed  A map of the currently analyzed methods
   * @return The points-to graph and set of modified abstract fields at the
   *         end of the method.
   */
  def intraProcedural(invokable: SJInvokable, analyzed: HashMap[Invocation, Result]): Result = {

    import TransferFunctions._
    import AST.{ foldLeft }

    def transferFunction(stm: SJStatement, state: TFState): TFState = {
      stm match {
        case SJAssignment(SJVariableAccess(v1), SJVariableAccess(v2))    => assignmentTF(stm, v1, v2, state)
        case SJNewExpression(SJVariableAccess(v),typ,arguments)          => newInstanceTF(stm, v, typ, invokable, state, arguments, analyzed)
        case SJFieldWrite(SJVariableAccess(v1), f, SJVariableAccess(v2)) => fieldWriteTF(stm, v1, f, v2, state)
        case SJReturn(SJVariableAccess(v))                               => returnTF(stm, v, state)
        case SJFieldRead(SJVariableAccess(v1), SJVariableAccess(v2), f)  => fieldReadTF(stm, v1,v2,f, invokable, state)
        case SJCall(value, SJVariableAccess(receiver), fun, arguments)   => callTF(stm, value, receiver, fun, arguments, invokable, analyzed, state)
        case _                                                           => state
      }
    }
    foldLeft(invokable.body, TFState(initialStateOfMethod(invokable),0,0), transferFunction).result
  }

  /**
   * Given an Invocation it will return the correct SJMethodDefinition or SJConstructorDefinition
   *
   * @param invocation The invocation of the method you want to get
   * @return The SJMethodDefinition or SJConstructorDefinition of the called method
   */
  private def getInvokable(invocation: Invocation) = {
    (invocation match {
      case (typ, "constructor") => SJTable.getConstructor(typ) // TODO: Better way to deal with constructors
      case (typ, method)        => SJTable.getMethodInClass(typ, method)
    }).get
  }

  /**
   * Produces the initial state of a method (PTGraph and Set of modified abstract fields)
   *
   * @param parameters The parameters of the invoked method.
   * @return The initial state of the analysis of the method.
   */
  private def initialStateOfMethod(invokable: SJInvokable) = {
    // page 7, section 5.21
    Result(
      pointsToGraph  = PTGraph(insideEdges          = empty,
                               outsideEdges         = empty,
                               stateOflocalVars     = (invokable.parameters.map( arg => (arg.id -> HashSet(ParameterNode("%s:%s".format(invokable.id,arg.id)))))
                                                      :+ ( "this" -> HashSet(ParameterNode("%s:this".format(invokable.id))))).toMap,
                               globallyEscapedNodes = empty),
      modifiedFields = empty
    )
  }

  /*
   * Transfer functions for the different commands. Implemented as described on
   * page 8.
   *
   * TODO: This doesn't cover Array assignments/reads, static assignments/reads, thread starts
   */
  private object TransferFunctions {

    private def localVars(state: TFState) = state.result.pointsToGraph.stateOflocalVars
    private def ptGraph(state: TFState) = state.result.pointsToGraph

    case class TFState(result: Result, insideNodeCount: Int, outsideNodeCount: Int)

    /*
     * v = f(...)
     * Get the points-to graph of the called method. Merge the points-to graph before the invocation
     * with the points-to graph at the end of the called function. The merging id done using a specific
     * mapping from nodes in the called PTGraph to nodes in the calling PTGraph. Once the graphs have
     * been merged it is simplified.
     */
    def callTF(stm: SJStatement,
               value: Option[SJVariableAccess],
               receiver: String,
               fun: String,
               arguments: List[SJExpression],
               invokable: SJInvokable,
               analyzed: HashMap[Invocation, Result],
               state: TFState): TFState = {

      val invocation = (invokable.localvariables(receiver), fun)
      val invokedMethod = getInvokable(invocation)
      val stateOfCall = analyzed.getOrElse(invocation, initialStateOfMethod(invokedMethod))
      val args = for { SJVariableAccess(arg) <- arguments } yield arg // TODO: Have to support other args then SJVariableAccess
      val mappingFunc = mapping(state.result.pointsToGraph,
                                stateOfCall.pointsToGraph,
                                invokedMethod,
                                args,
                                state.result.pointsToGraph.stateOflocalVars(receiver).asInstanceOf[Set[Node]])
      val combined = combine(state.result.pointsToGraph, stateOfCall.pointsToGraph, mappingFunc, value)
      val simplified = simplify(combined)

      state.copy(
        result = Result(
          pointsToGraph = simplified,
          modifiedFields = mapModifiedAbstractFields(stateOfCall.modifiedFields, simplified.nodes, mappingFunc) ++ state.result.modifiedFields
        )
      )
    }

    /*
     * v1 = v2.f
     * makes v1 point to all nodes pointed to by f-labeled inside edges starting from v2
     * In the case that any of the nodes pointed to by v2 were LoadNodes we:
     *   Introduce a new load node
     *   Introduce f-labeled outside edges for every escaped node we read from to the new load node
     */
    def fieldReadTF(stm: SJStatement, v1: String, v2: String, f: String, invokable: SJInvokable, state: TFState): TFState = {

      // n is escaped iff n is reachable from a node from 'escapedNodes' along a (possibly empty)
      // path of edges from graph.insideEdges ++ graph.outsideEdges.. As by definition 1 on page 7
      def isEscaped(n: Node): Boolean = {

        val escapedNodes = ("this" :: invokable.parameters.map(_.id)).flatMap{ id => localVars(state).getOrElse(id, empty).toList } ++
                           localVars(state).getOrElse(RETURN_LABEL, empty) ++
                           ptGraph(state).globallyEscapedNodes // TODO: Need Ngbl

        escapedNodes.exists(ptGraph(state).isReachable(n, _))
      }

      val nodes = (for {
          v2s <- localVars(state).get(v2)
        } yield for {
          v2 <- v2s
          InsideEdge(`v2`,`f`,n2) <- ptGraph(state).insideEdges
        } yield n2 ).getOrElse( empty )

      val b = for { n <- localVars(state).getOrElse(v2, empty) if isEscaped(n) } yield n

      if (b.isEmpty ) {
        state.copy(
          result = state.result.copy(
            pointsToGraph = ptGraph(state).copy( stateOflocalVars = localVars(state).updated(v1, nodes))
          )
        )
      } else {
        val outsideNode  = LoadNode(invokable.id+":L"+state.outsideNodeCount + " variable: " + v1)


        val outsideEdges = for { n <- b } yield OutsideEdge(n,f,outsideNode)

        state.copy(
          result = state.result.copy(
            pointsToGraph = ptGraph(state).copy( stateOflocalVars = localVars(state).updated(v1, nodes ++ HashSet(outsideNode)),
            outsideEdges  = ptGraph(state).outsideEdges ++ outsideEdges)
          ),
          outsideNodeCount = state.outsideNodeCount + 1
        )
      }
    }

    /*
     * v1 = v2
     * Make v1 point to all of the nodes that v2 pointed to
     */
    def assignmentTF(stm: SJStatement, v1: String, v2: String, state: TFState): TFState = {
      val newStateOfLocalVars = localVars(state).updated(v1, localVars(state).getOrElse(v2,empty))
      state.copy(
        result = state.result.copy(pointsToGraph = ptGraph(state).copy( stateOflocalVars = newStateOfLocalVars ))
      )
    }

    /*
     * v = new C
     * Makes v point to the newly created InsideNode
     */
    def newInstanceTF(stm: SJStatement,
                      v: String,
                      typ: String,
                      invokable: SJInvokable,
                      state: TFState,
                      arguments: List[SJExpression],
                      analyzed: HashMap[Invocation, Result]): TFState = {

      val insideNode = InsideNode(invokable.id+":I"+state.insideNodeCount + " variable: " + v + " type: " + typ)

      // A call to a constructor is still a call, so we need to merge the graphs.
      val invocation = (typ, "constructor")
      val invokedMethod = getInvokable(invocation)
      val stateOfCall = analyzed.getOrElse(invocation, initialStateOfMethod(invokedMethod))
      val args = for { SJVariableAccess(arg) <- arguments } yield arg // TODO: Have to support other args then SJVariableAccess
      val mappingFunc = mapping(state.result.pointsToGraph, stateOfCall.pointsToGraph, invokedMethod, args, HashSet(insideNode))
      val combined = combine(state.result.pointsToGraph, stateOfCall.pointsToGraph, mappingFunc, Some(SJVariableAccess(v)))
      val simplified = simplify(combined)


      val newGraph   = simplified.copy( stateOflocalVars = localVars(state).updated(v, HashSet(insideNode)))
      state.copy(
        result = state.result.copy( pointsToGraph = newGraph ),
        insideNodeCount = state.insideNodeCount + 1
      )
    }
    /*
     * v1.f = v2
     * Introduce an InsideEdge between each node pointed to by v1 and
     * each node pointed to by v2.
     * Also update the writes set to record the mutations on f of all
     * of the non-inside nodes pointed to by v1.
     */
    def fieldWriteTF(stm: SJStatement, v1: String, f: String, v2: String, state: TFState): TFState = {

      val insideEdges = (for {
          v1s <- localVars(state).get(v1)
          v2s <- localVars(state).get(v2)
        } yield for {
          v1 <- v1s
          v2 <- v2s
        } yield InsideEdge(v1,f,v2)).getOrElse( empty )

      val mods = (for {
          v1s <- localVars(state).get(v1)
        } yield for {
          node <- v1s if !node.isInstanceOf[InsideNode]
        } yield AbstractField(node,f)).getOrElse( empty )

      state.copy(
        result = state.result.copy(
          pointsToGraph  = ptGraph(state).copy( insideEdges = ptGraph(state).insideEdges ++ insideEdges ),
          modifiedFields = state.result.modifiedFields ++ mods
        )
      )
    }

    /*
     * return v
     */
    def returnTF(stm: SJStatement, v: String, state: TFState): TFState = {
      val newStateOfLocalVars = localVars(state).updated(RETURN_LABEL, localVars(state).getOrElse(v, empty))
      state.copy(
        result = state.result.copy(
          pointsToGraph = ptGraph(state).copy( stateOflocalVars = newStateOfLocalVars )
        )
      )
    }

  }
}