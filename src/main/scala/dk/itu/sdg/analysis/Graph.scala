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

object Graph {
    
  type Component = List[Vertex]
  
  // Simple data structure for now. 
  case class Vertex(label: String)
  case class Edge(from: Vertex, to: Vertex)
  case class G(start: Vertex, vertices: List[Vertex], edges: List[Edge])
  
  case class State(graph: G,
                   count: Int, 
                   visited: Map[Vertex, Boolean], 
                   dfNumber: Map[Vertex, Int],
                   lowlinks: Map[Vertex,Int],
                   stack: List[Vertex],
                   components: List[Component])
  
  def initial(g: G) = State (
    graph      = g,
    count      = 1,
    visited    = g.vertices.map { (_,false) } toMap,
    dfNumber   = Map(),
    lowlinks   = Map(),
    stack      = Nil, 
    components = Nil
  )
  
  def components(graph: G): List[Component] = {
            
    var state = search(graph.start, initial(graph))
        
    while(state.visited.exists( _._2 == false)) {
      state.visited.find(_._2 == false).foreach { tuple => 
        val (vertex, _) = tuple
        state = search(vertex, state)
      }
    }
    
    state.components
  }
  
  def search(vertex: Vertex, state: State): State = {
    
    val newState = state.copy( visited  = state.visited.updated(vertex, true),
                               dfNumber = state.dfNumber.updated(vertex,state.count),
                               count    = state.count + 1,
                               lowlinks = state.lowlinks.updated(vertex, state.count),
                               stack    = vertex :: state.stack)
    
    def processVertex(st: State, w: Vertex): State = {
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
  
  def smallest(x: Int, y: Int): Int = if (x < y) x else y  
  
  def adjacent(vertex: Vertex, state: State): List[Vertex] = 
    for { edge <- state.graph.edges if edge.from == vertex } yield edge.to 
}