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

  // ListItr

  "Purity analysis on ListItr constructor" should "record a mutation on this.cell" in {
    modifiedAbstractFields("ListItr", listConstructor) should equal (HashSet(AbstractField(ParameterNode("constructor:this"),"cell")))
  }

  "Purity analysis on ListItr.next" should "record a mutation on this.cell" in {
    modifiedAbstractFields("ListItr", listItrNextMethod) should equal (HashSet(AbstractField(ParameterNode("next:this"),"cell")))
  }

  "Purity analysis on ListItr.hasNext" should "not record any mutations" in {
    isPure("ListItr", listItrHasNext) should equal (true)
  }

  // List

  "Purity analysis on List.add" should "record a mutation on this.head" in  {
    modifiedAbstractFields("List",listAddMethod) should equal (HashSet(AbstractField(ParameterNode("add:this"),"head")))
  }

  "Purity analysis on List.iterator" should "not record any mutations" in {
    modifiedAbstractFields("List", listIteratorMethod) should equal (HashSet[String]())
  }

  // Point

  "Purity analysis on Point constructor" should "record mutations on this.x and this.y" in {
    modifiedAbstractFields("Point", pointConstructor) should equal (HashSet(AbstractField(ParameterNode("constructor:this"),"x"),
                                                                            AbstractField(ParameterNode("constructor:this"),"y")))
  }

  "Purity analysis on point.flip" should "mutate this.x & this.y" in {
    modifiedAbstractFields("Point", pointFlip) should equal (HashSet(AbstractField(ParameterNode("flip:this"),"x"),
                                                                     AbstractField(ParameterNode("flip:this"),"y")))
  }

  // Cell

  "Purity analysis on Cell constructor" should "record mutations on this.data and this.next" in {
    modifiedAbstractFields("Cell", cellConstructor) should equal (HashSet(AbstractField(ParameterNode("constructor:this"),"data"),
                                                                          AbstractField(ParameterNode("constructor:this"),"next")))
  }

  // PurityAnalysisExample

  "Purity analysis on PurityAnalysisExample.sumX" should "record no mutations" in {
    isPure("PurityAnalysisExample",sumx) should equal (true)
  }


  "Purity analysis on PurityAnalysisExample.flipAll" should "record mutations on list.head.next*.data.(x|y)" in {
    modifiedAbstractFields("PurityAnalysisExample", flipAll) should equal (
      Set(AbstractField(LoadNode("next:L1 variable: result"),"y"), AbstractField(LoadNode("next:L1 variable: result"),"x"))
    )
  }


  val ast = getASTbyParsingFileNamed("PurityAnalysisExample.java", List("src", "test", "resources", "static_analysis", "source"))
  val listAddMethod = methodsOf("List",ast).filter(_.id == "add").head
  val listItrNextMethod = methodsOf("ListItr",ast).filter(_.id == "next").head
  val listItrHasNext = methodsOf("ListItr", ast).filter(_.id == "hasNext").head
  val listIteratorMethod = methodsOf("List",ast).filter(_.id == "iterator").head
  val listConstructor = constructorOf("ListItr",ast)
  val pointConstructor = constructorOf("Point", ast)
  val pointFlip = methodsOf("Point",ast).filter(_.id == "flip").head
  val cellConstructor = constructorOf("Cell", ast)
  val sumx = methodsOf("PurityAnalysisExample",ast).filter(_.id == "sumX").head
  val flipAll = methodsOf("PurityAnalysisExample",ast).filter(_.id == "flipAll").head
}

class PurityTestsByMads extends FlatSpec with ShouldMatchers with ASTSpec {
  /*
    Tests and results by Mads
  */

  "PersonModifier.swapNames" should "modify p1.name and p2.name" in {
    modifiedAbstractFields("PersonModifier",swapNames) should equal(HashSet(AbstractField(ParameterNode("swapNames:p1"),"name"),
                                                                            AbstractField(ParameterNode("swapNames:p2"),"name")))
  }

  "PersonModifier.setName" should "modify p.name" in {
    modifiedAbstractFields("PersonModifier", setName) should equal(HashSet(AbstractField(ParameterNode("setName:p"),"name")))
  }

  "ParameterToArgument.existingAndModify" should "modify person.name" in {
    modifiedAbstractFields("ParameterToArgument",existingAndModify) should equal (HashSet(AbstractField(ParameterNode("existingAndModify:person"),"name")))
  }

  val ast2 = getASTbyParsingFileNamed("PurityAnalysisExample2.java", List("src", "test", "resources", "static_analysis", "source"))
  val swapNames = methodsOf("PersonModifier",ast2).filter(_.id == "swapNames").head
  val setName = methodsOf("PersonModifier",ast2).filter(_.id == "setName").head
  val existingAndModify = methodsOf("ParameterToArgument",ast2).filter(_.id == "existingAndModify").head
}