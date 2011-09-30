package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import PurityAnalysis._

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
  
  def methodsOf(id: String) = 
    ast.filter(_.id == id)
       .head.body
       .filter(_.isInstanceOf[SJMethodDefinition])
       .map(_.asInstanceOf[SJMethodDefinition])
  
  def constructorOf(id: String) = 
    ast.filter(_.id == id)
       .head.body
       .filter(_.isInstanceOf[SJConstructorDefinition])
       .map(_.asInstanceOf[SJConstructorDefinition])
       .head
  
  val listAddMethod = methodsOf("List").filter(_.id == "add").head
  
  val listItrNextMethod = methodsOf("ListItr").filter(_.id == "next").head
  
  val listIteratorMethod = methodsOf("List").filter(_.id == "iterator").head
  
  val listConstructor = constructorOf("ListItr")
 
 
  
}