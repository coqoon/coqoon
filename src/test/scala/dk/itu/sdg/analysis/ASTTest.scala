package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import org.scalatest.Ignore
import dk.itu.sdg.javaparser._ 
import dk.itu.sdg.analysis._
import dk.itu.sdg.analysis.AnalysisTestHelpers.{ methodsOf }

class ASTTest extends FlatSpec with ShouldMatchers with ASTSpec {
  
  "Finding the CG of method in PurityAnalysisExample.java" should "find the right CG" in {
    
    val graph = AST.extractCallGraph("PurityAnalysisExample",sumX)
    
    val vertex1 = Vertex(("PurityAnalysisExample","sumX"))
    val vertex2 = Vertex(("List","iterator"))
    val vertex3 = Vertex(("ListItr","constructor"))
    val vertex4 = Vertex(("Iterator","hasNext"))
    val vertex5 = Vertex(("Iterator","next"))
    
    val edge1 = Edge(vertex1,vertex2)
    val edge2 = Edge(vertex2,vertex3)
    val edge3 = Edge(vertex1,vertex4)
    val edge4 = Edge(vertex1,vertex5)
    
    val expected = G(
      vertex1,
      List(vertex1,
           vertex2,
           vertex3,
           vertex4,
           vertex5),
      List(edge1,
           edge2,
           edge3, 
           edge4))
        
    graph should equal (expected)
  }
  
  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  val sumX = methodsOf("PurityAnalysisExample",ast).filter(_.id == "sumX").head
}