package dk.itu.sdg.analysis

import  dk.itu.sdg.analysis.Purity._

object NFA extends Graph {

  type ItemType = Node

  def create(analysisResult: Result): (G[_ <: ItemType], Vertex[ItemType]) = {

    val nodes = analysisResult.pointsToGraph.nodes.toList

    val start  = Vertex(InsideNode("START"))
    val accept = Vertex(InsideNode("ACCEPT"))
    val states = start :: accept :: nodes.map(Vertex(_)).toList

    val transitions: List[Edge[ItemType]] = {

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

    (G(start, states, transitions), accept)
  }

  def allPaths(graph: G[ItemType], to: Vertex[ItemType]): List[String] = {

    def rec(path: List[String], current: Vertex[ItemType], visited: List[Vertex[ItemType]]): List[String] = {
      println("pursuing path " + path.reverse.mkString("."))
      if (current == to) {
        List(path.reverse.mkString("."))
      } else {
        (graph.edges.collect {
          case Edge(v1,v2,Some(lb)) if v1 == current && !visited.contains(v2) =>
            rec(lb :: path, v2, current :: visited)
        }).flatten
      }
    }

    rec(Nil,graph.start,Nil)
  }

  def print(graph: G[ItemType]): Unit = {
    println("start: " + graph.start)
    println("edges: \n" + graph.edges.mkString("\n"))
    println("nodes: \n" + graph.vertices.mkString("\n"))
  }

}

object RegExpGenerator {

  def regularExpressions(analysisResult: Result): List[String] = {
    val (graph,accept) = NFA.create(analysisResult)
    NFA.allPaths(graph,accept)
  }

  def print(analysisResult: Result): Unit = {
    val (graph,accept) = NFA.create(analysisResult)
    NFA.print(graph)
  }

}