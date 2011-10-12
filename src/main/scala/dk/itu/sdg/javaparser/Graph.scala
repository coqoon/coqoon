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

package dk.itu.sdg.javaparser

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
  
  /* Playground. */
  def main(args: Array[String]): Unit = {

    // Example taken from page 187, figure 5.13

    val v1 = Vertex("V1")
    val v2 = Vertex("V2")
    val v3 = Vertex("V3")
    val v4 = Vertex("V4")
    val v5 = Vertex("V5")
    val v6 = Vertex("V6")
    val v7 = Vertex("V7")
    val v8 = Vertex("V8")
    
    val vertices = List(v1,v2,v3,v4,v5,v6,v7,v8)
    
    val edges = List(
                  Edge(v1, v2),
                  Edge(v1, v5),
                  Edge(v1, v4),
                  Edge(v2, v3),
                  Edge(v2, v4),
                  Edge(v3, v1),
                  Edge(v4, v3),
                  Edge(v5, v4),
                  Edge(v6, v8),
                  Edge(v6, v7),
                  Edge(v7, v5),
                  Edge(v8, v4),
                  Edge(v8, v6),
                  Edge(v8, v7))
    
    val g = G(v1, vertices, edges)

    
    println("found components: " + components(g).mkString("\n","\n",""))
    
  }
}