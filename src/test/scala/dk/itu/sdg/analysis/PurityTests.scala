package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import dk.itu.sdg.javaparser._
import Purity._
import AnalysisTestHelpers._

/*
  Intra-procedural tests. 
*/
class IntraproceduralTests extends FlatSpec with ShouldMatchers with ASTSpec {
    
  "Purity analysis on List.add" should "record a mutation on this.head" in  {
    intraProceduralOnMethod(listAddMethod).modifiedFields should equal (HashSet(AbstractField(LoadNode("this"),"head")))
  }
  
  "Purity analysis on ListItr.next" should "record a mutation on this.cell" in {
    intraProceduralOnMethod(listItrNextMethod).modifiedFields should equal (HashSet(AbstractField(LoadNode("this"),"cell")))
  }
  
  "Purity analysis on List.iterator" should "not record any mutations" in {
    intraProceduralOnMethod(listIteratorMethod).modifiedFields should equal (HashSet[String]())
  }
 
  "Purity analysis on ListItr constructor" should "record a mutation on this.cell" in {
    intraProceduralOnConstructor(listConstructor).modifiedFields should equal (HashSet(AbstractField(LoadNode("this"),"cell")))
  }
  
  /*
    Data. 
  */
  
  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  val listAddMethod = methodsOf("List",ast).filter(_.id == "add").head
  val listItrNextMethod = methodsOf("ListItr",ast).filter(_.id == "next").head
  val listIteratorMethod = methodsOf("List",ast).filter(_.id == "iterator").head
  val listConstructor = constructorOf("ListItr",ast)  
}