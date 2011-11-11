/**
 * Purity analysis for Simple Java. Implemented as described in
 * 'A Combined Pointer and Purity Analysis for Java Programs'. Any
 * Page references in the source file are references to that paper.
 *
 * @author Mads Hartmann Jensen
 */

/*
  Random notes and things I still have to consider:

  - v1.f = 10 would currently just be ignored as 10 isn't a var.
  - each node needs to get a proper label
*/

package dk.itu.sdg.analysis

import dk.itu.sdg.analysis.CallGraph.{ Invocation }
import dk.itu.sdg.javaparser._

import scala.collection.immutable.{HashSet, HashMap}

object Purity {

  /*
   *  public interface
   *  ====================================
   */

  def isPure(className: String, method: SJMethodDefinition): Boolean =
    modifiedAbstractFields(className, method).isEmpty

  def modifiedAbstractFields(className: String, method: SJMethodDefinition): Set[AbstractField] =
    analysis(className, method).modifiedFields

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

  case class Graph( //TODO: Rename of PTGraph
    insideEdges         : Set[InsideEdge],
    outsideEdges        : Set[OutsideEdge],
    stateOflocalVars    : Map[String, Set[_ <: Node]],
    globallyEscapedNodes: Set[EscapedNode]
  )

  case class State( // TODO: Rename to Result
    pointsToGraph : Graph,
    modifiedFields: Set[AbstractField]
  )

  /*
   *  Methods
   */

  /**
   * Main entry point for the analysis.
   *
   * @param className The name of the class that contains the method to analyze. At
   *                  some points this should be moved into SJMethodDefinition
   * @param method    The method to analyze.
   * @return A points-to graph and set of abstract fields that are modified by the
   *         the method.
   */
  def analysis(className: String, method: SJMethodDefinition): State = {

    val callGraph  = AST.extractCallGraph(className, method)
    val components = CallGraph.components(callGraph)

    // Keep track of the result of analyzing the method
    var analyzedMethods = HashMap[Invocation, State]()

    components.reverse.foreach { component =>
      // Keep track of the methods that still needs to be analyzed
      var worklist: List[Vertex[Invocation]] = component

      while (worklist.nonEmpty) {

        val invocation = worklist.head.item
        val invoked = getInvokable(invocation)

        val oldPTState = analyzedMethods.getOrElse(invocation, initialStateOfMethod(invoked.parameters))
        val newPTState = intraProcedural(invoked, analyzedMethods)

        if (oldPTState != newPTState) {

          for {
            Edge(from,`invocation`) <- callGraph.edges
          } {
            worklist = from :: worklist
          }

          analyzedMethods = analyzedMethods.updated(invocation, newPTState)
        }

        worklist = worklist.tail
      }
    }

    analyzedMethods(callGraph.start.item)
  }

  /**
   * Constructs the node mapping. Explained on page 9
   *
   * @param g           The Points-to graph of the method that is currently being analyzed
   * @param gCallee     The Points-to graph of the invoked method
   * @param parameters  The name of the parameters of the invoked method
   * @param arguments   The names of the variables passed as arguments to the method
   * @return A mapping from nodes in 'gCallee' to nodes in 'g'
   */
  private def mapping(g: Graph,
                      gCallee: Graph,
                      parameters: List[SJArgument],
                      arguments: List[String]): Node => Set[Node] = {

    val refine: (Mapping => Mapping, Mapping) => Mapping = (f,g) => f(g)

    // Mapping 1
    // The parameter nodes of gCallee should map to the nodes pointed to by the
    // arguments used to invoke callee.
    val parameterToArgument: Mapping = {
      val parameterNodes = for {
        parameter           <- parameters.map( _.id )
        OutsideEdge(_,_,n2) <- gCallee.outsideEdges if n2.lb == parameter
      } yield n2

      val args: List[Set[Node]] = (for { id <- arguments } yield g.stateOflocalVars(id)).asInstanceOf[List[Set[Node]]]

      val map = parameterNodes.zip(args).toMap

      (n: Node) => map(n)
    }

    // Mapping 2
    // All outside nodes of gCallee that point to inside nodes of G should be
    // mapped to those nodes.
    val outsideToInside: Mapping => Mapping = { (mapping: Mapping) =>

      val map = (for {
        OutsideEdge(n1, f, n2) <- gCallee.outsideEdges
        InsideEdge(n3, f, n4)  <- g.insideEdges if mapping(n1).contains(n3)
      } yield (n2 -> n4)).groupBy( _._1 )
                         .map { case (key,value) => (key -> value.map( _._2 ).toSet ) }

      (n: Node) => map(n)
    }

    // Mapping 3
    // This constraint deals with the aliasing present in the calling context
    val aliasInCallingContext: Mapping => Mapping = { (mapping: Mapping) =>

      val loadNodes = for { OutsideEdge(n1, f, n2) <- g.outsideEdges if n2.isInstanceOf[LoadNode]} yield n2

      val map = (for {
        OutsideEdge(n1, f, n2) <- gCallee.outsideEdges
        InsideEdge(n3, f, n4)  <- gCallee.insideEdges if n1 != n3 &&
                                                         n1.isInstanceOf[LoadNode] &&
                                                         (mapping(n1) + n1).intersect( (mapping(n3) + n3)).nonEmpty
        extra = if (n4.isInstanceOf[ParameterNode]) empty else HashSet(n4)
      } yield (n2 -> (mapping(n4) ++ extra) )).groupBy( _._1 )
                                        .map { case (key, value) => (key -> value.map( _._2 ).toSet.flatten) }

      (n: Node) => map(n)
    }

    // Mapping 4
    // Each non-parameter node should map to itself
    val nonParameterToSelf: Mapping => Mapping = { (mapping: Mapping) =>
      val nonParameterNodes =
        (gCallee.insideEdges ++ gCallee.outsideEdges).map( _.n2 ).filter( !_.isInstanceOf[ParameterNode])

      val map = nonParameterNodes.zip(nonParameterNodes.map( HashSet(_) )).toMap

      (n: Node) => mapping(n) ++ map(n)
    }

    refine(nonParameterToSelf, refine(aliasInCallingContext, refine( outsideToInside, parameterToArgument)))
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
  private def combine(g: Graph,
                      gCallee: Graph,
                      mapping: Mapping,
                      varName: Option[SJVariableAccess] ): Graph = {

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
        node   <- gCallee.stateOflocalVars(RETURN_LABEL)
        mapped <- mapping(node)
      } yield mapped)
    }).getOrElse(g.stateOflocalVars)
    
    val globallyEscapedNodes = (for {
      node   <- gCallee.globallyEscapedNodes
      mapped <- mapping(node)
    } yield mapped).asInstanceOf[Set[EscapedNode]] // TODO: Don't know if this is sound yet.

    Graph(
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
  private def simplify(graph: Graph): Graph = {
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
  private def modifiedAbstractFields(calleeModified: Set[AbstractField],
                                     nodesInSimplifiedGraph: Set[Node],
                                     mapping: Mapping): Set[AbstractField] = {
    for {
      AbstractField(n, f) <- calleeModified
      node <- mapping(n) if !node.isInstanceOf[InsideNode] && nodesInSimplifiedGraph.contains(node)
    } yield AbstractField(node, f)
  }

  /**
   * Analyze a single method
   *
   * @param invokable The method to analyze
   * @param analyzed  A map of the currently analyzed methods
   * @return The points-to graph and set of modified abstract fields at the
   *         end of the method.
   */
  def intraProcedural(invokable: SJInvokable,
                      analyzed: HashMap[Invocation, State]): State = {

    val body       = invokable.body
    val parameters = invokable.parameters

    /*
     * Traverse the body from top to bottom transforming the Points-to-Graph on every
     * statement on the way and (possibly) adding AbstractField to the writes set
     */
    def transferFunction(stm: SJStatement, before: State): State = {

      val graph     = before.pointsToGraph
      val localVars = before.pointsToGraph.stateOflocalVars

      stm match {

        /*
         * Transfer functions for the different commands. Implemented as described on
         * page 8.
         *
         * TODO: This doesn't cover Array assignments/reads, static assignments/reads, thread starts
         */

        case SJAssignment(SJVariableAccess(v1), SJVariableAccess(v2)) => {
          /*
           * v1 = v2
           * Make v1 point to all of the nodes that v2 pointed to
           */
          val newStateOfLocalVars = localVars.updated(v1, localVars.getOrElse(v2,empty))
          before.copy( pointsToGraph = graph.copy( stateOflocalVars = newStateOfLocalVars ))
        }

        case SJNewExpression(SJVariableAccess(v),tpy,args) => {
          /*
           * v = new C
           * Makes v point to the newly created InsideNode
           */
          val insideNode = InsideNode("some label")
          val newGraph   = graph.copy( stateOflocalVars = localVars.updated(v, HashSet(insideNode)))
          before.copy( pointsToGraph = newGraph )
        }

        case SJFieldWrite(SJVariableAccess(v1), f, SJVariableAccess(v2)) => {
          /*
           * v1.f = v2
           * Introduce an InsideEdge between each node pointed to by v1 and
           * each node pointed to by v2.
           * Also update the writes set to record the mutations on f of all
           * of the non-inside nodes pointed to by v1.
           */
          val insideEdges = (for {
              v1s <- localVars.get(v1)
              v2s <- localVars.get(v2)
            } yield for {
              v1 <- v1s
              v2 <- v2s
            } yield InsideEdge(v1,f,v2)).getOrElse( empty )

          val mods = (for {
              v1s <- localVars.get(v1)
            } yield for {
              node <- v1s if !node.isInstanceOf[InsideNode]
            } yield AbstractField(node,f)).getOrElse( empty )

          before.copy( pointsToGraph  = graph.copy( insideEdges = graph.insideEdges & insideEdges ),
                       modifiedFields = before.modifiedFields ++ mods)
        }

        case SJFieldRead(SJVariableAccess(v1), SJVariableAccess(v2), f) => {
          /*
           * v1 = v2.f
           * makes v1 point to all nodes pointed to by f-labeled inside edges starting from v2
           * In the case that any of the nodes pointed to by v2 were LoadNodes we:
           *   Introduce a new load node
           *   Introduce f-labeled outside edges for every escaped node we read from to the new load node
           */
          val nodes = (for {
              v2s <- localVars.get(v2)
            } yield for {
              v2 <- v2s
              fLabeledIEdge <- graph.insideEdges if fLabeledIEdge.field == f && fLabeledIEdge.n1 == v2
            } yield fLabeledIEdge.n2 ).getOrElse( empty )

          val escapedNodes = (for {
              v2s <- localVars.get(v2)
            } yield for {
              v2 <- v2s
              fLabeledOEdge <- graph.outsideEdges if fLabeledOEdge == f && fLabeledOEdge.n1 == v2
            } yield fLabeledOEdge.n2 ).getOrElse(empty)

          if (escapedNodes.isEmpty ) {
            before.copy( pointsToGraph = graph.copy( stateOflocalVars = localVars.updated(v1, nodes) ))
          } else {
            val outsideNode  = LoadNode("some label")
            val outsideEdges = for { n <- escapedNodes } yield OutsideEdge(n,f,outsideNode)
            before.copy(pointsToGraph = graph.copy( stateOflocalVars = localVars.updated(v1, nodes ++ HashSet(outsideNode)),
                                                    outsideEdges     = graph.outsideEdges ++ outsideEdges ))
          }
        }

        case SJReturn(SJVariableAccess(v)) => {
          /*
           * return v
           */
          val newStateOfLocalVars = localVars.updated(RETURN_LABEL, localVars.getOrElse(v, empty))
          before.copy( pointsToGraph = graph.copy( stateOflocalVars = newStateOfLocalVars ))
        }

        case SJCall(value, SJVariableAccess(receiver), fun, arguments) => {
          /*
           * v = f(...)
           * Get the points-to graph of the called method. Merge the points-to graph before the invocation
           * with the points-to graph at the end of the called function. The merging id done using a specific
           * mapping from nodes in the called PTGraph to nodes in the calling PTGraph. Once the graphs have
           * been merged it is simplified.
           */
          val invocation = (invokable.localvariables(receiver), fun)
          val invokedMethod = getInvokable(invocation)

          val stateOfCall = analyzed.getOrElse(invocation, initialStateOfMethod(invokedMethod.parameters))
          val args = for { SJVariableAccess(arg) <- arguments } yield arg // TODO: Have to support other args then SJVariableAccess
          val mappingFunc = mapping(before.pointsToGraph, stateOfCall.pointsToGraph, invokedMethod.parameters, args)
          val combined = combine(before.pointsToGraph, stateOfCall.pointsToGraph, mappingFunc, value)
          val simplified = simplify(combined)

          val nodesInSimplifiedGraph = (
            (for {
              InsideEdge(n1,_,n2) <- simplified.insideEdges
            } yield HashSet(n1,n2)).flatten ++
            (for {
              OutsideEdge(n1,_,n2) <- simplified.outsideEdges
            } yield HashSet(n1,n2)).flatten
          ).toSet

          State(
            pointsToGraph = simplified,
            modifiedFields = modifiedAbstractFields(stateOfCall.modifiedFields, nodesInSimplifiedGraph, mappingFunc)
          )
        }

        case _ => before
      }
    }

    AST.foldLeft(body, initialStateOfMethod(parameters), transferFunction)
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
  private def initialStateOfMethod(parameters: List[SJArgument]) = {
    // page 7, section 5.21
    State(
      pointsToGraph  = Graph(insideEdges          = empty,
                             outsideEdges         = empty,
                             stateOflocalVars     = (parameters.map( arg => (arg.id -> HashSet(ParameterNode(arg.id))) )
                                                    :+ ( "this" -> HashSet(ParameterNode("this")))).toMap,
                             globallyEscapedNodes = empty),
      modifiedFields = empty
    )
  }
}
