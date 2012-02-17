package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }

import AnalysisTestHelpers._
import dk.itu.sdg.javaparser._
import dk.itu.sdg.analysis._
import dk.itu.sdg.analysis.Purity._


class RegExpGeneratorTest extends FlatSpec with ShouldMatchers with ASTSpec {

  "Mutations on ListItr constructor" should "by the path this.cell" in {
    val result = getState("ListItr", listConstructor)
    RegExpGenerator.regularExpressions(result) should equal (List("constructor:this.cell"))
  }

  "Purity analysis on ListItr.next" should "record a mutation on this.cell" in {
    val result = getState("ListItr", listItrNextMethod)
    RegExpGenerator.regularExpressions(result) should equal (List("next:this.cell"))
  }

  "Purity analysis on List.add" should "record a mutation on this.head" in  {
    val result = getState("List",listAddMethod)
    RegExpGenerator.regularExpressions(result) should equal (List("add:this.head"))
  }

  "Purity analysis on Point constructor" should "record mutations on this.x and this.y" in {
    val result = getState("Point", pointConstructor)
    RegExpGenerator.regularExpressions(result) should equal (List("constructor:this.y","constructor:this.x"))
  }

  "Purity analysis on point.flip" should "mutate this.x & this.y" in {
    val result = getState("Point", pointFlip)
    RegExpGenerator.regularExpressions(result) should equal (List("flip:this.x","flip:this.y"))
  }

  "Purity analysis on Cell constructor" should "record mutations on this.data and this.next" in {
    val result = getState("Cell", cellConstructor)
    RegExpGenerator.regularExpressions(result) should equal (List("constructor:this.data","constructor:this.next"))
  }

  "Purity analysis on PurityAnalysisExample.flipAll (fails at the moment)" should "record mutations on list.head.next*.data.(x|y)" in {
    val result = getState("PurityAnalysisExample", flipAll)
    RegExpGenerator.regularExpressions(result) should equal (List("list.head.next*.data.(x|y)"))
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