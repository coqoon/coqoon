/*

  This file contains implementations of different graphs
  used in the project.

  @author Mads Hartmann Jensen
*/
package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashMap }
import dk.itu.sdg.javaparser._
import dk.itu.sdg.analysis.Purity._

// Graph data structures.
case class Vertex[+ItemType](item: ItemType)
case class Edge[+ItemType](from: Vertex[ItemType], to: Vertex[ItemType], lb: Option[String] = None)

trait BaseGraph[ItemType] {

  def edges: List[Edge[ItemType]]

  def vertices: List[Vertex[ItemType]]

  protected def adjacent(vertex: Vertex[ItemType]) =
    for { edge <- edges if edge.from == vertex } yield edge.to

}

trait Root[ItemType] {

  this: BaseGraph[ItemType] =>

  def start: Vertex[ItemType]

}

/*

  # Overview

  Implementation of a SCC (Strongly Connected Components) algorithm for our graphs,
  mix this into a BaseGraph to get a 'components' method.

  # Details

  This is implemented following the instructions in "The Design and Analysis of
  Computer Algorithms, AHO Hopcroft Ullman, 1974".

  The implementation uses a DFS to find the strongly connected components (SCCs)
  of a graph. During the DFS the vertices are placed on a stack in the order
  they are visited. Whenever a root is found, all vertices of the corresponding
  SSC are on the top of the stack and are popped.

  A root of a SCC is found by keeping track of the DFN (depth first number) and
  LLN (lowlink number) of each vertex. When a vertex (v) is first encountered
  it is marked as visited and LLN(v) is set to DFN(v). Each adjacent vertex (w)
  is then considered.

    - If w is unvisited recurse with w. set LLN(w) to the smallest of LLN(w)
      & LLN(v).

    - If w is visited AND w was visited before v AND w is on the stack then set
      LLN(v) to the smallest of DFN(w) and LLN(v).

   If, after all the adjacent vertexes have been considered, the LLN(v) is the
   same as DFN(v) then v the root of the current SSC.

   When there are no vertexes un-visited the algorithm is done.

   @author Mads Hartmann Jensen.
*/
trait StronglyConnected[ItemType] {

  this: BaseGraph[ItemType] with Root[ItemType] =>

  type Component = List[Vertex[ItemType]]

  // Return a list with the strongly connected components of
  // this graph.
  def components: List[Component] = {

    var state = search(start, initial)

    while(state.visited.exists( _._2 == false)) {
      state.visited.find(_._2 == false).foreach {
        case (vertex, _) =>
          state = search(vertex, state)
      }
    }

    state.components
  }

  // State used when implementing the SCC algorithm.
  private case class State(
    count     : Int,
    visited   : Map[Vertex[ItemType], Boolean],
    dfNumber  : Map[Vertex[ItemType], Int],
    lowlinks  : Map[Vertex[ItemType],Int],
    stack     : List[Vertex[ItemType]],
    components: List[Component]
  )

  // Initial state for the SCC algorithm.
  private def initial = State (
    count      = 1,
    visited    = vertices.map { (_,false) } toMap,
    dfNumber   = Map(),
    lowlinks   = Map(),
    stack      = Nil,
    components = Nil
  )

  // Search for SCC.
  private def search(vertex: Vertex[ItemType], state: State): State = {

    val newState = state.copy(
      visited  = state.visited.updated(vertex, true),
      dfNumber = state.dfNumber.updated(vertex,state.count),
      count    = state.count + 1,
      lowlinks = state.lowlinks.updated(vertex, state.count),
      stack    = vertex :: state.stack
    )

    def processVertex(st: State, w: Vertex[ItemType]): State = {
      if (!st.visited(w)) {
        val st1 = search(w, st)
        val min = smallest(st1.lowlinks(w),st1.lowlinks(vertex))
        st1.copy( lowlinks = st1.lowlinks.updated(vertex, min) )
      } else {
        if ( (st.dfNumber(w) < st.dfNumber(vertex)) && st.stack.contains(w) ) {
          val min = smallest( st.dfNumber(w), st.lowlinks(vertex) )
          st.copy( lowlinks = st.lowlinks.updated(vertex, min))
        } else st
      }
    }

    val strslt = adjacent(vertex).foldLeft(newState)( processVertex )

    if (strslt.lowlinks(vertex) == strslt.dfNumber(vertex)) {
      val index = strslt.stack.indexOf(vertex)
      val (comp,rest) = strslt.stack.splitAt( index + 1 )
      strslt.copy (
        stack = rest,
        components = strslt.components :+ comp
      )
    } else strslt
  }

  // get the smallest of two numbers.
  private def smallest(x: Int, y: Int): Int = if (x < y) x else y

}

/*

  Implementation of a call graph. use the `fromAST` method in
  the companion object to constrct the CG.

  @author Mads Hartmann Jensen

*/

import CallGraph.Invocation

class CallGraph(
  val start: Vertex[Invocation],
  val vertices: List[Vertex[Invocation]],
  val edges: List[Edge[Invocation]]
) extends BaseGraph[Invocation] with Root[Invocation] with StronglyConnected[Invocation] {

  override def equals(other: Any) = other match {
    case cg: CallGraph => {
      this.start == cg.start &&
      this.vertices == cg.vertices &&
      this.edges == cg.edges
    }
    case _ => false
  }
}

object CallGraph {

  // Invocation is a pair of strings. _1 is the name of the type and
  // _2 is the name of the method invoked on an instance of the type.
  type Invocation = (String, String)

  // Construct a CG from the AST of a method/constructor
  def fromAST(cls: String, invokable: SJInvokable): CallGraph = {

    // State used in the fold.
    case class State(
      vertices: List[Vertex[Invocation]],
      edges: List[Edge[Invocation]],
      visited: Map[Invocation, Boolean]
    )

    // Method that recurses through the AST in a depth first fashion.
    def recurse(vertex: Vertex[Invocation], invokable: SJInvokable, state: State): State = {

      def wasSJCall(st: State, reciever: String, methodName: String) = {
        val recieverType = invokable.localvariables.get(reciever).get
        val invocation   = (recieverType, methodName)
        wasInvocation(st, invocation, SJTable.getMethodInClass(recieverType,methodName).get)
      }

      def wasNexExpression(st: State, typ: String) = {
        val invocation = (typ,"constructor") // TODO: Find a better name than constructor.
        wasInvocation(st, invocation, SJTable.getConstructor(typ).get)
      }

      def wasInvocation(st: State, invocation: Invocation, after: SJInvokable) = {
        if (!st.visited.contains(invocation)) {
          val v = Vertex(invocation)
          val e = Edge(vertex, v)
          val st2 = st.copy(
            vertices = st.vertices :+ v,
            edges    = st.edges    :+ e,
            visited  = st.visited.updated(invocation, true)
          )
          recurse(v, after, st2)
        } else recurse(vertex, after, st)
      }

      AST.foldLeft(invokable.body, state, (stm, st: State) => stm match {
        case SJCall(_,SJVariableAccess(reciever),methodName,_) => wasSJCall(st, reciever, methodName)
        case SJNewExpression(_,typ,_) => wasNexExpression(st, typ)
        case _ => st // Don't care about the rest of the nodes.
      })
    }

    val main = Vertex((cls,invokable.id))
    val initial = State(main :: Nil, Nil, Map())
    val state = recurse(main,invokable,initial)
    new CallGraph(main, state.vertices, state.edges)
  }
}