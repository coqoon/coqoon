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
  
  import scalaz._
  import Scalaz._
    
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
  
  def components(g: G): List[Component] = {
    
    def run: scalaz.State[State, State] = for {
      next <- next
      cont <- search(next)
      st   <- gets( (s: State) => s )
    } yield if (cont) run ! st else st 
      
    (run ! (search(g.start) ~> initial(g))).components
  }
  
  def search(v: Vertex): scalaz.State[State, Boolean] = for {
    _  <- visited(v)
    _  <- push(v)
    _  <- setDFNumber(v)
    _  <- setLowlink(v)
    _  <- incrementCounter
    _  <- processAdjacents(v)
    _  <- getComponents(v)
    s  <- gets ( (st: State) => st )
  } yield s.visited.exists( _._2 == false)
  
  
  def processAdjacents(v: Vertex) = for {
    adjacents <- adjacent(v)
    _ <- modify ( (st: State) => adjacents.foldLeft(st)( (s: State, w: Vertex) => processVertex(v,w,s) ) )
  } yield Unit
  
  def getComponents(v: Vertex) = for {
    _ <- modify( (s: State) => {
      if (s.lowlinks(v) == s.dfNumber(v)) { 
        val index = s.stack.indexOf(v)
        val (comp,rest) = s.stack.splitAt( index + 1 )
        s.copy ( stack = rest, components = s.components :+ comp)
      } else s
    })
  } yield Unit 
  
  def processVertex(current: Vertex, adjacent: Vertex, st: State): State = (for {
    visited <- isVisited(adjacent)
    _       <- if (visited) wasVisited(current, adjacent) else wasntVisited(current, adjacent)
    s       <- gets ((sta: State) => sta)
  } yield s) ~> st 
  
  def next = for { s <- gets ( (st: State) => st ) } 
             yield s.visited.find( _._2 == false ).map( _._1 ).get
  
  def adjacent(v: Vertex) = for { 
    s <- gets ( (st: State) => st )
    vertices = for { edge <- s.graph.edges if edge.from == v } yield edge.to  
  } yield vertices
    
  def wasntVisited(current: Vertex, adjacent: Vertex) = for {
    _ <- search(adjacent)
    _ <- modify( (s: State) => {
      val min = smallest( s.lowlinks(adjacent), s.lowlinks(current) )
      s.copy( lowlinks = s.lowlinks.updated(current, min))
    })
  } yield Unit
  
  def wasVisited(current: Vertex, adjacent: Vertex) = modify( (s: State) => {
    if (s.dfNumber(adjacent) < s.dfNumber(current) && s.stack.contains(adjacent)) {
      val min = smallest( s.dfNumber(adjacent), s.lowlinks(current) )
      s.copy( lowlinks = s.lowlinks.updated(current, min)) 
    } else s
  })
    
  def isVisited(w: Vertex)   = for { s <- gets ( (st: State) => st ) } yield s.visited(w)
  def visited(v: Vertex)     = modify( (s: State) => s.copy( visited = s.visited.updated(v,true)))
  def push(v: Vertex)        = modify( (s: State) => s.copy( stack = v :: s.stack))
  def setDFNumber(v: Vertex) = modify( (s: State) => s.copy( dfNumber = s.dfNumber.updated(v, s.count)) )
  def setLowlink(v: Vertex)  = modify( (s: State) => s.copy( lowlinks = s.lowlinks.updated(v, s.count)) )
  def incrementCounter       = modify( (s: State) => s.copy( count = s.count + 1 ))
  
  def smallest(x: Int, y: Int): Int = if (x < y) x else y  
    
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