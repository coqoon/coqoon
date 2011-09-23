package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import PurityAnalysis._

/*
  Intra-procedural tests. 
*/
class IntraproceduralTests extends FlatSpec with ShouldMatchers with ASTSpec {
  
  "....." should "....." in  {
  
    val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java",
                                       List("src", "test", "resources", "static_analysis", "source"))
    println(ast)
  
    true should equal (true)
  
  } 
      
  
    
}