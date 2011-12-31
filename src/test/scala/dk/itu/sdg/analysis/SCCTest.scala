package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import org.scalatest.Ignore
import dk.itu.sdg.javaparser._
import dk.itu.sdg.analysis._

// Handy when testing. The vertices are simply carrying strings.
class TestGraph(
  val start: Vertex[String],
  val vertices: List[Vertex[String]],
  val edges: List[Edge[String]]
) extends BaseGraph[String] with Root[String] with StronglyConnected[String]

class GraphTest extends FlatSpec with ShouldMatchers {

  "Finding the SCC of example graph" should "find the right components" in {

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

    val g = new TestGraph(v1, vertices, edges)

    g.components should equal (
      List(List(Vertex("V5"), Vertex("V4"), Vertex("V3"), Vertex("V2"), Vertex("V1")),
         List(Vertex("V7")),
         List(Vertex("V8"), Vertex("V6"))))
  }
}