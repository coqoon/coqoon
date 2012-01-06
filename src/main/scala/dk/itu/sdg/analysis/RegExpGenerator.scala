package dk.itu.sdg.analysis

import  dk.itu.sdg.analysis.Purity._

object RegExpGenerator {

  def regularExpressions(analysisResult: Result): List[String] =
    NFA.fromPurityResult(analysisResult).allPaths

  def print(analysisResult: Result): Unit =
    NFA.fromPurityResult(analysisResult).print

}

/*
  Implementation of a non-deterministic finite automata.

  @author Mads Hartmann Jensen
*/
class NFA private (
  val start: Vertex[Node],
  val vertices: List[Vertex[Node]],
  val edges: List[Edge[Node]],
  val accept: Vertex[Node]
) extends BaseGraph[Node] with Root[Node] {

  def allPaths: List[String] = {

    def rec(
      path: List[String],
      current: Vertex[Node],
      visited: List[Vertex[Node]]
    ): List[String] = {
      if (current == accept) {
        List(path.reverse.mkString("."))
      } else {
        (edges.collect {
          case Edge(v1,v2,Some(lb)) if v1 == current && !visited.contains(v2) =>
            rec(lb :: path, v2, current :: visited)
        }).flatten
      }
    }

    rec(Nil,start,Nil)
  }

  def print: Unit = {
    println("start: " + start)
    println("edges: \n" + edges.mkString("\n"))
    println("nodes: \n" + vertices.mkString("\n"))
  }

}

object NFA {

  def fromPurityResult(analysisResult: Result): NFA = {

    val nodes = analysisResult.pointsToGraph.nodes.toList

    val start  = Vertex(InsideNode("START"))
    val accept = Vertex(InsideNode("ACCEPT"))
    val states = start :: accept :: nodes.map(Vertex(_)).toList

    val transitions: List[Edge[Node]] = {

      val startToParameter = for {
        p@ParameterNode(name) <- nodes
      } yield Edge(start,Vertex(p), Some(name))

      val stateToState = for {
        OutsideEdge(n1,f,n2) <- analysisResult.pointsToGraph.outsideEdges
      } yield Edge(Vertex(n1),Vertex(n2),Some(f))

      val stateToAccept = for {
        AbstractField(n,f) <- analysisResult.modifiedFields
      } yield Edge(Vertex(n), accept, Some(f))

      startToParameter ++ stateToState ++ stateToAccept
    }

    new NFA(start, states, transitions, accept)
  }
}
