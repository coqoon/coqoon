package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import dk.itu.sdg.javaparser._
import Purity._
import AnalysisTestHelpers._


class PurityTestsFromPaper extends FlatSpec with ShouldMatchers with ASTSpec {
  
  /*
    Test results expected from the paper 
  */
    
  "Purity analysis on List.add" should "record a mutation on this.head" in  {
    modifiedAbstractFields("List",listAddMethod) should equal (HashSet(AbstractField(ParameterNode("this"),"head")))
  }
  
  "Purity analysis on ListItr.next" should "record a mutation on this.cell" in {
    modifiedAbstractFields("ListItr", listItrNextMethod) should equal (HashSet(AbstractField(ParameterNode("this"),"cell")))
  }
  
  "Purity analysis on List.iterator" should "not record any mutations" in {
    modifiedAbstractFields("List", listIteratorMethod) should equal (HashSet[String]())
  }
  
  "Purity analysis on List.iterator" should "state that it is pure" in {
    isPure("List", listIteratorMethod) should equal (true)
  }
  
  // Currently only works with methods not constructors 
  // ---
  // "Purity analysis on ListItr constructor" should "record a mutation on this.cell" in {
  //   modifiedAbstractFields(listConstructor) should equal (HashSet(AbstractField(ParameterNode("this"),"cell")))
  // }
    
  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  val listAddMethod = methodsOf("List",ast).filter(_.id == "add").head
  val listItrNextMethod = methodsOf("ListItr",ast).filter(_.id == "next").head
  val listIteratorMethod = methodsOf("List",ast).filter(_.id == "iterator").head
  val listConstructor = constructorOf("ListItr",ast)  
}

class PurityTestsByMads extends FlatSpec with ShouldMatchers with ASTSpec {
  /*
    Tests and results by Mads
  */
  
  "ParameterToArgument.pta" should "be pure" in {
    isPure("ParameterToArgument", pta) should equal (true)
  }
  
  val ast2 = getASTbyParsingFileNamed("PurityAnalysisExample2.java", List("src", "test", "resources", "static_analysis", "source"))
  val pta = methodsOf("ParameterToArgument",ast2).filter(_.id == "pta").head
}