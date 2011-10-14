package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import org.scalatest.Ignore
import dk.itu.sdg.javaparser._ 
import dk.itu.sdg.analysis.Graph._
import dk.itu.sdg.analysis.AnalysisTestHelpers.{ methodsOf }


class ASTTest extends FlatSpec with ShouldMatchers with ASTSpec {
  
  "Finding the CG of method in PurityAnalysisExample.java" should "find the right CG" in {
    
    AST.extractCalLGraph("PurityAnalysisExample",sumX)
    
    true should equal (true)    
  }
  
  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  val sumX = methodsOf("PurityAnalysisExample",ast).filter(_.id == "sumX").head
}