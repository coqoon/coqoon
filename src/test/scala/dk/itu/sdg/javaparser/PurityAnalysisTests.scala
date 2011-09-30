package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import PurityAnalysis._

/*
  Intra-procedural tests. 
*/
class IntraproceduralTests extends FlatSpec with ShouldMatchers with ASTSpec {
  
  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  
  val listAddMethod = {
    ast.filter(_.id == "List")
       .head
       .body
       .filter(_.isInstanceOf[SJMethodDefinition])
       .map(_.asInstanceOf[SJMethodDefinition])
       .filter(_.id == "add")    
       .head
  }
  
  "....." should "....." in  {
  
    println(intraProcedural(listAddMethod))
  
    true should equal (true)
  
  }
}