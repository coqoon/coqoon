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
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJMethodDefinition(Set(), "foo", "int", List(), List(SJAssignment(SJVariableAccess("x"), SJLiteral("20")), SJReturn(SJVariableAccess("x"))), HashMap("this" -> "Foo", "x" -> "int"))), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition foo_body := (cassign (var_expr "x") (20:expr)).""",
"""Definition fooM := Build_Method ("this" :: nil) foo_body (var_expr "x").""",
"""Definition Foo := Build_Class (SS.empty) (SM.add "foo" fooM (SM.empty _)).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "simple class definition with a conditional" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJMethodDefinition(Set(), "foo", "int", List(), List(SJConditional(SJBinaryExpression("<", SJLiteral("10"), SJLiteral("20")), List(SJAssignment(SJVariableAccess("x"), SJLiteral("100"))), List(SJAssignment(SJVariableAccess("x"), SJLiteral("200")))), SJReturn(SJVariableAccess("x"))), HashMap("x" -> "int", "this" -> "Foo"))), None, HashMap()))
    val expected = List("Module Foo <: PROGRAM.",
"""Definition foo_body := (cif (elt (10:expr) (20:expr)) (cassign (var_expr "x") (100:expr)) (cassign (var_expr "x") (200:expr))).""",
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
"""Definition Foo := Build_Class (SS.empty) (SM.add "new" Foo_new (SM.empty _)).""",
"""Definition Prog := Build_Program (SM.add "Foo" Foo (SM.empty _)).""",
"End Foo.", "")
    FinishAST.coqoutput(tst, false, "Foo") should equal(expected)
  }

  "cell class" should "produce some coq definition" in {
    val tst = List(SJClassDefinition(Set(), "Cell", "", Nil, List(SJFieldDefinition(Set(), "value", "Object"), SJConstructorDefinition(Set(), "Cell", List(), List(), HashMap("this" -> "Cell")), SJMethodDefinition(Set(), "get", "Object", List(), List(SJFieldRead(SJVariableAccess("x"), SJVariableAccess("this"), "value"), SJReturn(SJVariableAccess("x"))), HashMap("this" -> "Cell", "x" -> "Object")), SJMethodDefinition(Set(), "set", "Object", List(SJArgument("x", "Object")), List(SJFieldWrite(SJVariableAccess("this"), "value", SJVariableAccess("x"))), HashMap("this" -> "Cell", "x" -> "Object"))), None, HashMap("value" -> "Object")))
    val expected = List("Module Cell <: PROGRAM.",
"""Definition Cell_new := Build_Method (nil) (calloc "this" "Cell") (var_expr "this").""",
"""Definition get_body := (cread (var_expr "x") (var_expr "this") "value").""",
"""Definition getM := Build_Method ("this" :: nil) get_body (var_expr "x").""",
"""Definition set_body := (cwrite (var_expr "this") "value" (var_expr "x")).""",
"""Definition setM := Build_Method ("this" :: "x" :: nil) set_body 0.""",
"""Definition Cell := Build_Class (SS.add "value" (SS.empty)) (SM.add "new" Cell_new (SM.add "get" getM (SM.add "set" setM (SM.empty _)))).""",
"""Definition Prog := Build_Program (SM.add "Cell" Cell (SM.empty _)).""",
"End Cell.", "")
    FinishAST.coqoutput(tst, false, "Cell") should equal(expected)
  }

  "recell class" should "produce some coq definition" in {
    val cell = SJClassDefinition(Set(), "Cell", "", Nil, List(SJFieldDefinition(Set(), "value", "Object"), SJConstructorDefinition(Set(), "Cell", List(), List(), HashMap("this" -> "Cell")), SJMethodDefinition(Set(), "get", "Object", List(), List(SJFieldRead(SJVariableAccess("x"), SJVariableAccess("this"), "value"), SJReturn(SJVariableAccess("x"))), HashMap("this" -> "Cell", "x" -> "Object")), SJMethodDefinition(Set(), "set", "Object", List(SJArgument("x", "Object")), List(SJFieldWrite(SJVariableAccess("this"), "value", SJVariableAccess("x"))), HashMap("this" -> "Cell", "x" -> "Object"))), None, HashMap("value" -> "Object"))
    val recell = SJClassDefinition(Set(), "Recell", "", Nil, List(SJFieldDefinition(Set(), "cell", "Cell"), SJFieldDefinition(Set(), "backup", "Object"), SJConstructorDefinition(Set(), "Recell", List(), List(SJCall(Some(SJVariableAccess("tmp_1")), SJLiteral("Cell"), "new", List()), SJFieldWrite(SJVariableAccess("this"), "cell", SJVariableAccess("tmp_1"))), HashMap("this" -> "Recell", "tmp_1" -> "Cell")), SJMethodDefinition(Set(), "get", "Object", List(), List(SJFieldRead(SJVariableAccess("cell"), SJVariableAccess("this"), "cell"), SJCall(Some(SJVariableAccess("x")), SJVariableAccess("cell"), "get", List()), SJReturn(SJVariableAccess("x"))), HashMap("this" -> "Recell", "cell" -> "Cell", "x" -> "Object")), SJMethodDefinition(Set(), "set", "Object", List(SJArgument("x", "Object")), List(SJFieldRead(SJVariableAccess("cell"), SJVariableAccess("this"), "cell"), SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("cell"), "get", List()), SJFieldWrite(SJVariableAccess("this"), "cell", SJVariableAccess("tmp_1")), SJCall(None, SJVariableAccess("cell"), "set", List(SJVariableAccess("x")))), HashMap("this" -> "Recell", "x" -> "Object", "tmp_1" -> "Object", "cell" -> "Cell"))), None, HashMap("cell" -> "Cell", "backup" -> "Object"))
    SJTable.addClass(cell)
    SJTable.addClass(recell)
    val tst = List(cell, recell)
    val expected = List("Module Cell <: PROGRAM.",
"""Definition Cell_new := Build_Method (nil) (calloc "this" "Cell") (var_expr "this").""", """Definition get_body := (cread (var_expr "x") (var_expr "this") "value").""",
"""Definition getM := Build_Method ("this" :: nil) get_body (var_expr "x").""",
"""Definition set_body := (cwrite (var_expr "this") "value" (var_expr "x")).""",
"""Definition setM := Build_Method ("this" :: "x" :: nil) set_body 0.""",
"""Definition Cell := Build_Class (SS.add "value" (SS.empty)) (SM.add "new" Cell_new (SM.add "get" getM (SM.add "set" setM (SM.empty _)))).""",
"""Module Cell <: PROGRAM.""",
"""Definition Recell_new := Build_Method (nil) (cseq (calloc "this" "Recell") (cseq (cscall (var_expr "tmp_1") Cell "new" nil) (cwrite (var_expr "this") "cell" (var_expr "tmp_1")))) (var_expr "this").""",
"""Definition get_body := (cseq (cread (var_expr "cell") (var_expr "this") "cell") (cdcall (var_expr "x") (var_expr "cell") "get" nil)).""",
"""Definition getM := Build_Method ("this" :: nil) get_body (var_expr "x").""",
"""Definition set_body := (cseq (cread (var_expr "cell") (var_expr "this") "cell") (cseq (cdcall (var_expr "tmp_1") (var_expr "cell") "get" nil) (cseq (cwrite (var_expr "this") "cell" (var_expr "tmp_1")) (cdcall  (var_expr "cell") "set" (var_expr "x") :: nil)))).""",
"""Definition setM := Build_Method ("this" :: "x" :: nil) set_body 0.""",
"""Definition Recell := Build_Class (SS.add "backup" (SS.add "cell" (SS.empty))) (SM.add "new" Recell_new (SM.add "get" getM (SM.add "set" setM (SM.empty _)))).""",
"""Definition Prog := Build_Program (SM.add "Recell" Recell (SM.add "Cell" Cell (SM.empty _))).""", "End Cell.", "")
    FinishAST.coqoutput(tst, false, "Cell") should equal(expected)
  }
}
