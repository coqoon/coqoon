/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.HashMap

class SJTableTestSpec extends FlatSpec with ShouldMatchers {
  //you'd certainly think this doesn't require a test, since it's only
  //a single immutable hashtable and 6 defined methods.
  //but the implementation has been messed up too many times

  "EmptyTable" should "be empty" in {
    SJTable.reset
    SJTable.ct.size should equal(0)
  }

  val m1 = SJMethodDefinition(Set(), "m1", "void", Nil, List(), HashMap("this" -> "Foo"))
  val m2 = SJMethodDefinition(Set(), "m2", "int", Nil, List(), HashMap("this" -> "Foo"))
  val cl1 = SJClassDefinition(Set(), "Foo", "", Nil, List(m1, m2), None, HashMap())

  val c1 = SJConstructorDefinition(Set(Public()), "Bar", List(), List(), HashMap("this" -> "Bar"))
  val m3 = SJMethodDefinition(Set(), "m3", "void", Nil, List(), HashMap("this" -> "Bar"))
  val cl2 = SJClassDefinition(Set(), "Bar", "", Nil, List(m3, c1), None, HashMap())

  def populate () = {
    SJTable.reset //for safety!
    SJTable.addClass(cl1)
  }

  def populate2 () = {
    SJTable.addClass(cl1)
    SJTable.addClass(cl2)
  }

  "Populated Table" should "be populated with 1 class" in {
    populate
    SJTable.ct.size should equal(1)
    SJTable.reset //for safety!
  }

  "Populated Table" should "contain Foo" in {
    populate
    SJTable.getClass("Foo") should equal(cl1)
    SJTable.reset //for safety!
  }

  "Populated Table" should "not contain Bar" in {
    populate
    val thr = intercept[AssertionError] {
      SJTable.getClass("Bar")
    }
    thr.getMessage should equal("assertion failed")
    SJTable.reset //for safety!
  }

  "Populated Table" should "not contain a constructor" in {
    populate
    SJTable.getConstructor("Foo") should equal(None)
    SJTable.reset //for safety!
  }

  "Populated Table" should "not contain m3" in {
    populate
    SJTable.getMethodInClass("Foo", "m3") should equal(None)
    SJTable.reset //for safety!
  }

  "Populated Table" should "contain m1" in {
    populate
    SJTable.getMethodInClass("Foo", "m1") should equal(Some(m1))
    SJTable.reset //for safety!
  }

  "Populated Table" should "contain m2" in {
    populate
    SJTable.getMethodInClass("Foo", "m2") should equal(Some(m2))
    SJTable.reset //for safety!
  }

  "Populated Table" should "return right type for m1" in {
    populate
    SJTable.getMethodTypeOfClass("Foo", "m1") should equal("void")
    SJTable.reset //for safety!
  }

  "Populated Table" should "return right type for m2" in {
    populate
    SJTable.getMethodTypeOfClass("Foo", "m2") should equal("int")
    SJTable.reset //for safety!
  }

  "Populated Table" should "return right type for m3" in {
    //that's actually unpleasant
    populate
    SJTable.getMethodTypeOfClass("Foo", "m3") should equal("")
    SJTable.reset //for safety!
  }

  //lets put bar into it as well
  "Populated Table2" should "contain 2 classes" in {
    SJTable.reset //for safety
    populate2
    SJTable.ct.size should equal(2)
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain Foo" in {
    SJTable.reset //for safety
    populate2
    SJTable.getClass("Foo") should equal(cl1)
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain Bar" in {
    SJTable.reset //for safety
    populate2
    SJTable.getClass("Bar") should equal(cl2)
    SJTable.reset //for safety
  }

  "Populated Table2" should "complain about a class with the same name" in {
    SJTable.reset //for safety
    populate2
    val thr = intercept[AssertionError] {
      populate2
    }
    thr.getMessage should equal("assertion failed")
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain constructor for Bar" in {
    SJTable.reset //for safety
    populate2
    SJTable.getConstructor("Bar") should equal(Some(c1))
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain method Foo::m1" in {
    SJTable.reset //for safety
    populate2
    SJTable.getMethodInClass("Foo", "m1") should equal(Some(m1))
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain method Foo::m2" in {
    SJTable.reset //for safety
    populate2
    SJTable.getMethodInClass("Foo", "m2") should equal(Some(m2))
    SJTable.reset //for safety
  }

  "Populated Table2" should "not contain method Foo::m3" in {
    SJTable.reset //for safety
    populate2
    SJTable.getMethodInClass("Foo", "m3") should equal(None)
    SJTable.reset //for safety
  }

  "Populated Table2" should "contain method Bar::m3" in {
    SJTable.reset //for safety
    populate2
    SJTable.getMethodInClass("Bar", "m3") should equal(Some(m3))
    SJTable.reset //for safety
  }

  "Populated Table2" should "not contain method Bar::m1" in {
    SJTable.reset //for safety
    populate2
    SJTable.getMethodInClass("Bar", "m1") should equal(None)
    SJTable.reset //for safety
  }
}

