/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.HashMap

class SimpleJavaASTSpec extends ASTSpec {
  //coqoutput(<>, false, <>);

  "simple class definition" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""
Definition Foo :=
  Build_Class (SS.empty)
              (SM.empty _).""",
"""
Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }


}
