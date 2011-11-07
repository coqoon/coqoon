/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.HashMap

class CoqOutputterSpec extends ASTSpec {
  "simple class definition" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"Definition Foo := Build_Class (SS.empty) (SM.empty _).",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "simple class definition with a field" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJFieldDefinition(Set(), "a", "Object")), None, HashMap("a" -> "Object")))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition Foo := Build_Class (SS.add "a" (SS.empty)) (SM.empty _).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "simple class definition with two fields" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJFieldDefinition(Set(), "a", "Object"), SJFieldDefinition(Set(), "b", "Object")), None, HashMap("a" -> "Object", "b" -> "Object")))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition Foo := Build_Class (SS.add "a" (SS.add "b" (SS.empty))) (SM.empty _).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "simple class definition with a method" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJMethodDefinition(Set(), "foo", "int", List(), List(SJAssignment(SJVariableAccess("x"), SJLiteral("20")), SJReturn(SJVariableAccess("x"))), HashMap("x" -> "int"))), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition foo_body := (cassign (var_expr "x") (20:expr)).""",
"""Definition fooM := Build_Method ("this" :: nil) foo_body (var_expr "x").""",
"""Definition Foo := Build_Class (SS.empty) (SM.add "foo" fooM (SM.empty _)).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "simple class definition with a constructor" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJConstructorDefinition(Set(), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition Foo_new := Build_Method (nil) (calloc "this" "Foo") (var_expr "this").""",
"""Definition Foo := Build_Class (SS.empty) (SM.add "Foo_new" Foo_new (SM.empty _)).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }
/*
  "cell class" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Cell", "", Nil, List(SJFieldDefinition(Set(), "value", "Object"), SJMethodDefinition(Set(), "foo", "int", List(), List(SJAssignment(SJVariableAccess("x"), SJLiteral("20")), SJReturn(SJVariableAccess("x"))), HashMap("x" -> "int"))), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition foo_body := (cassign (var_expr "x") (20:expr)).""",
"""Definition fooM := Build_Method ("this" :: nil) foo_body (var_expr "x").""",
"""Definition Foo := Build_Class (SS.empty) (SM.add "foo" fooM (SM.empty _)).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }
*/
}
