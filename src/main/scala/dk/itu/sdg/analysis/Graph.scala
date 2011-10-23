/*
   
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

package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashMap }

// Graph data structures.
case class Vertex[ItemType](item: ItemType)
case class Edge[ItemType](from: Vertex[ItemType], to: Vertex[ItemType])
case class G[ItemType](start: Vertex[ItemType], vertices: List[Vertex[ItemType]], edges: List[Edge[ItemType]])

trait Graph {
    
  // ItemType is abstract. Specified by Graph implementations.
  type ItemType
  
  // Simplifying the types a bit.  
  type GVertex   = Vertex[ItemType]
  type GEdge     = Edge[ItemType]
  type GG        = G[ItemType]
  type Component = List[GVertex]
    
  // State used when implementing the SCC algorithm.
  case class State(graph: GG,
                   count: Int,
                   visited: Map[GVertex, Boolean],
                   dfNumber: Map[GVertex, Int],
                   lowlinks: Map[GVertex,Int],
                   stack: List[GVertex],
                   components: List[Component])
  
  // Initial state for the SCC algorithm.
  def initial(g: GG) = State (
    graph      = g,
    count      = 1,
    visited    = g.vertices.map { (_,false) } toMap,
    dfNumber   = Map(),
    lowlinks   = Map(),
    stack      = Nil,
    components = Nil
  )
  
  // Calculate the SCC of a Graph.
  def components(graph: GG): List[Component] = {
    
    var state = search(graph.start, initial(graph))
    
    while(state.visited.exists( _._2 == false)) {
      state.visited.find(_._2 == false).foreach { tuple => 
        val (vertex, _) = tuple
        state = search(vertex, state)
      }
    }
    
    state.components
  }
  
  // Search for SCC.
  private def search(vertex: GVertex, state: State): State = {
    
    val newState = state.copy( visited  = state.visited.updated(vertex, true),
                               dfNumber = state.dfNumber.updated(vertex,state.count),
                               count    = state.count + 1,
                               lowlinks = state.lowlinks.updated(vertex, state.count),
                               stack    = vertex :: state.stack)
    
    def processVertex(st: State, w: GVertex): State = {
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
      
    val strslt = adjacent(vertex, newState).foldLeft(newState)( processVertex )
    
    if (strslt.lowlinks(vertex) == strslt.dfNumber(vertex)) {
      
      val index = strslt.stack.indexOf(vertex)
      val (comp,rest) = strslt.stack.splitAt( index + 1 )
      strslt.copy ( stack = rest, 
                    components = strslt.components :+ comp)
    } else strslt
  }
  
  // get the smallest of two numbers.
  private def smallest(x: Int, y: Int): Int = if (x < y) x else y  
  
  // Get the adjacent vertices of a given vertex.
  private def adjacent(vertex: GVertex, state: State): List[GVertex] = 
    for { edge <- state.graph.edges if edge.from == vertex } yield edge.to 
}

// Call-graph. The vertices carry Invocations 
object CallGraph extends Graph {
  
  // Invocation is a pair of strings. _1 is the name of the type and 
  // _2 is the name of the method invoked on an instance of the type.
  type Invocation = (String, String)
  
  type ItemType = Invocation 
}