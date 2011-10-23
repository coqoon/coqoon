/**
 * Purity analysis for Simple Java. Implemented as described in
 * 'A Combined Pointer and Purity Analysis for Java Programs'. Any
 * Page references in the source file are references to that paper.
 *
 * @author Mads Hartmann Jensen
 */

/*
  Random notes and things I still have to consider:

    v1.f = 10 would currently just be ignored as 10 isn't a var.

*/

package dk.itu.sdg.analysis

import dk.itu.sdg.analysis.CallGraph.{ Invocation }
import dk.itu.sdg.javaparser._

import scala.collection.immutable.{HashSet, HashMap}

object Purity {

  private def Ø[B] = HashSet[B]()

  import AST._

  val vRet = "RETURN" // name of special variable that points to the nodes returned
                      // form a method. See page page 9 before section 5.3

  // Page 6 figure 7.
  trait Node { val lb: String }
  case class InsideNode(lb: String) extends Node
  case class LoadNode(lb: String) extends Node
  case class EscapedNode(lb: String) extends Node

  trait Edge {
    val n1: Node
    val field: String
    val n2: Node
  }

  case class InsideEdge(n1: Node, field: String, n2: Node) extends Edge
  case class OutsideEdge(n1: Node, field: String, n2: Node) extends Edge

  case class AbstractField(node: Node, field: String)

  case class Graph(
    insideEdges         : Set[InsideEdge],
    outsideEdges        : Set[OutsideEdge],
    stateOflocalVars    : Map[String, Set[_ <: Node]],
    globallyEscapedNodes: Set[EscapedNode]
  )

  case class State(
    pointsToGraph : Graph,
    modifiedFields: Set[AbstractField]
  )

  def isPure(method: SJMethodDefinition): Boolean = false

  def interProcedural(method: SJMethodDefinition): State = {
    // This is explained on page 10-11. 

    // Extract the call graph from the AST and find the strongly 
    // connected components of the CG. 
    val callGraph  = AST.extractCallGraph("TODO", method)                       // TODO: Use proper class name or find a way to avoid it.
    val components = CallGraph.components(callGraph)

    // Iterate though the work-list till a fixed point is found. 
    def iter(workList: List[Vertex[Invocation]], st: State): State = {

      if (workList.isEmpty) st else {

        val invokale = (workList.head.item match {
          case (typ, "constructor") => SJTable.getConstructor(typ)              // TODO: Better way to deal with constructors
          case (typ, method)        => SJTable.getMethodInClass(typ, method)
        }).get // TODO: For now, assuming that the methods are always present.

        val st2 = intraProcedural(invokale)
        
        if (st2.pointsToGraph != st.pointsToGraph) {
          val callers = for { e <- callGraph.edges if e.to == workList.head } yield e.to
          iter(workList.tail ::: callers,st2)
        } else {
          iter(workList.tail, st2)
        }
      }
    }

    iter(components.head, initialStateOfMethod(method.parameters))              // TODO: Only analyzing the FIRST SCC for now. 
  }

  def intraProcedural(invokable: SJInvokable): State = {

    val body       = invokable.body
    val parameters = invokable.parameters

    /*
      Traverse the body from top to bottom transforming the Points-to-Graph on every
      statement on the way and (possibly) adding AbstractField to the writes set
    */
    def transferFunction(stm: SJStatement, before: State): State = {

      val graph     = before.pointsToGraph
      val localVars = before.pointsToGraph.stateOflocalVars

      stm match {

        /*
          Transfer functions for the different commands. Implemented as described on
          page 8.

          TODO: This doesn't cover Array assignments/reads
                                   static assignments/reads
                                   thread starts
        */

        case SJAssignment(SJVariableAccess(v1), SJVariableAccess(v2)) => {
          /*
            v1 = v2
            Make v1 point to all of the nodes that v2 pointed to
          */
          val newStateOfLocalVars = localVars.updated(v1, localVars.getOrElse(v2,Ø))
          before.copy( pointsToGraph = graph.copy( stateOflocalVars = newStateOfLocalVars ))
        }

        case SJNewExpression(SJVariableAccess(v),tpy,args) => {
          /*
            v = new C
            Makes v point to the newly created InsideNode
          */
          val insideNode = InsideNode("some label")
          val newGraph   = graph.copy( stateOflocalVars = localVars.updated(v, HashSet(insideNode)))
          before.copy( pointsToGraph = newGraph )
        }

        case SJFieldWrite(SJVariableAccess(v1), f, SJVariableAccess(v2)) => {
          /*
            v1.f = v2
            Introduce an InsideEdge between each node pointed to by v1 and
            each node pointed to by v2.
            Also update the writes set to record the mutations on f of all
            of the non-inside nodes pointed to by v1.
          */
          val insideEdges = (for {
              v1s <- localVars.get(v1)
              v2s <- localVars.get(v2)
            } yield for {
              v1 <- v1s
              v2 <- v2s
            } yield InsideEdge(v1,f,v2)).getOrElse( Ø )

          val mods = (for {
              v1s <- localVars.get(v1)
            } yield for {
              node <- v1s if !node.isInstanceOf[InsideNode]
            } yield AbstractField(node,f)).getOrElse( Ø )

          before.copy( pointsToGraph  = graph.copy( insideEdges = graph.insideEdges & insideEdges ),
                       modifiedFields = before.modifiedFields ++ mods)
        }

        case SJFieldRead(SJVariableAccess(v1), SJVariableAccess(v2), f) => {
          /*
            v1 = v2.f
            makes v1 point to all nodes pointed to by f-labeled inside edges starting from v2
            In the case that any of the nodes pointed to by v2 were LoadNodes we:
              Introduce a new load node
              Introduce f-labeled outside edges for every escaped node we read from to the new load node
          */
          val nodes = (for {
              v2s <- localVars.get(v2)
            } yield for {
              v2 <- v2s
              fLabeledIEdge <- graph.insideEdges if fLabeledIEdge.field == f && fLabeledIEdge.n1 == v2
            } yield fLabeledIEdge.n2 ).getOrElse( Ø )

          val escapedNodes = (for {
              v2s <- localVars.get(v2)
            } yield for {
              v2 <- v2s
              fLabeledOEdge <- graph.outsideEdges if fLabeledOEdge == f && fLabeledOEdge.n1 == v2
            } yield fLabeledOEdge.n2 ).getOrElse(Ø)

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
            return v
          */
          val newStateOfLocalVars = localVars.updated(vRet, localVars.getOrElse(v, Ø))
          before.copy( pointsToGraph = graph.copy( stateOflocalVars = newStateOfLocalVars ))
        }

        // TODO: SJCall. Difference between an analyzable and non analyzable call

        case _ => before
      }
    }

    foldLeft(body, initialStateOfMethod(parameters), transferFunction)
  }

  def initialStateOfMethod(parameters: List[SJArgument]) =
    // page 7, section 5.21
    State(
      pointsToGraph  = Graph(insideEdges          = Ø,
                             outsideEdges         = Ø,
                             stateOflocalVars     = (parameters.map( arg => (arg.id -> HashSet(LoadNode(arg.id))) )
                                                    :+ ( "this" -> HashSet(LoadNode("this")))).toMap,
                             globallyEscapedNodes = Ø),
      modifiedFields = Ø
    )

}