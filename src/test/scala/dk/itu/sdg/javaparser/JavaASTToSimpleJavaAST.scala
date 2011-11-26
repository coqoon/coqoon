/*
  This tests the conversion from JavaAST to SimpleJavaAST. It parses each file
  in src/test/resources/javaparser/source and tests that the expected
  SimpleJavaAST AST is generated.

  @author Mads Hartman Jensen
*/

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.HashMap

class JavaASTSpec extends ASTSpec {

  /*
     NOTE:
     The easiest way to do this is to run the Main method in sbt with the
     given file and then in Emacs: M-x query-replace-regexp RET \([a-z0-9_+-*]+\) RET "\1"
   */

  "Parsing Postfix1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), (SJLiteral("0"))),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("+", SJVariableAccess("a"), SJLiteral("1")))),
                        HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Postfix1.txt") should equal(expected)
  }

  "Parsing Postfix2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("0")),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("-", SJVariableAccess("a"), SJLiteral("1")))),
                      HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Postfix2.txt") should equal(expected)
  }

  "Parsing Postfix3.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "a", "int"),
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "a"),
        SJFieldWrite(SJVariableAccess("this"), "a", SJBinaryExpression("+", SJVariableAccess("tmp_1"), SJLiteral("1")))),
                       HashMap("tmp_1" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("a" -> "int")))
    getASTbyParsingFileNamed("Postfix3.txt") should equal(expected)
  }

  "Parsing Conditional1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", List(SJArgument("a", "int")), List(
        SJConditional(SJBinaryExpression("==", SJVariableAccess("a"), SJLiteral("10")),
                      List(SJAssignment(SJVariableAccess("b"), SJLiteral("20"))),
                      List(SJAssignment(SJVariableAccess("b"), SJLiteral("30"))))),
                       HashMap("b" -> "int", "a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Conditional1.txt") should equal(expected)
  }

  "Parsing Conditional2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "c", "int"),
      SJMethodDefinition(Set(), "bar", "void", List(SJArgument("a", "int")), List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "c"),
        SJConditional(SJBinaryExpression("==", SJVariableAccess("a"), SJVariableAccess("tmp_1")),
                      List(SJAssignment(SJVariableAccess("b"), SJLiteral("20"))),
                      List(SJAssignment(SJVariableAccess("b"), SJLiteral("30"))))),
                       HashMap("tmp_1" -> "int", "b" -> "int", "a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))),
                                          None, HashMap("c" -> "int")))
    getASTbyParsingFileNamed("Conditional2.txt") should equal(expected)
  }

  "Parsing NestedField1.txt" should "produce the correct AST" in {
    val expected =
      List(
        SJClassDefinition(Set(), "Foo", "", Nil, List(
          SJFieldDefinition(Set(), "a", "int"),
          SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("a" -> "int")),
        SJClassDefinition(Set(), "Bar", "", Nil, List(
          SJFieldDefinition(Set(), "f", "Foo"),
          SJMethodDefinition(Set(), "bar", "void", Nil, List(
            SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
            SJFieldRead(SJVariableAccess("b"), SJVariableAccess("tmp_1"), "a")),
                             HashMap("tmp_1" -> "Foo", "b" -> "int", "this" -> "Bar")),
          SJConstructorDefinition(Set(Public()), "Bar", List(), List(), HashMap("this" -> "Bar"))), None, HashMap("f" -> "Foo")))
    getASTbyParsingFileNamed("NestedField1.txt") should equal(expected)
  }

  "Parsing NestedField2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", List(), List(
      SJFieldDefinition(Set(), "f", "Foo"),
      SJFieldDefinition(Set(), "a", "int"),
      SJMethodDefinition(Set(), "bar", "int", List(), List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
        SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("tmp_1"), "f"),
        SJFieldRead(SJVariableAccess("tmp_3"), SJVariableAccess("tmp_2"), "f"),
        SJFieldRead(SJVariableAccess("tmp_4"), SJVariableAccess("tmp_3"), "f"),
        SJFieldRead(SJVariableAccess("tmp_5"), SJVariableAccess("tmp_4"), "f"),
        SJFieldRead(SJVariableAccess("tmp_6"), SJVariableAccess("tmp_5"), "a"),
        SJReturn(SJVariableAccess("tmp_6"))),
                       HashMap("tmp_1" -> "Foo", "tmp_2" -> "Foo", "tmp_3" -> "Foo",
                               "tmp_4" -> "Foo", "tmp_5" -> "Foo", "tmp_6" -> "int",
                             "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None,
                                        HashMap("a" -> "int", "f" -> "Foo")))
    getASTbyParsingFileNamed("NestedField2.txt") should equal(expected)
  }


  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJMethodDefinition(Set(), "bar", "void", Nil, List(), HashMap("this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()),
      SJClassDefinition(Set(), "Bar", "", Nil, List(
        SJFieldDefinition(Set(), "f", "Foo"),
        SJMethodDefinition(Set(), "bar", "void", Nil, List(
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
          SJCall(None, SJVariableAccess("tmp_1"), "bar", Nil)),
                           HashMap("tmp_1" -> "Foo", "this" -> "Bar")),
        SJConstructorDefinition(Set(Public()), "Bar", List(), List(), HashMap("this" -> "Bar"))), None, HashMap("f" -> "Foo")))
    getASTbyParsingFileNamed("NestedCall1.txt") should equal(expected)
  }

  "Parsing NestedCall2.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJFieldDefinition(Set(), "f", "Foo"),
        SJFieldDefinition(Set(), "a", "int"),
        SJMethodDefinition(Set(), "get", "Foo", Nil, List(
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
          SJReturn(SJVariableAccess("tmp_1"))),
                           HashMap("tmp_1" -> "Foo", "this" -> "Foo")),
        SJMethodDefinition(Set(), "bar", "int", Nil, List(
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
          SJCall(Some(SJVariableAccess("tmp_2")), SJVariableAccess("tmp_1"), "get", Nil),
          SJCall(Some(SJVariableAccess("tmp_3")), SJVariableAccess("tmp_2"), "get", Nil),
          SJFieldRead(SJVariableAccess("tmp_4"), SJVariableAccess("tmp_3"), "f"),
          SJFieldRead(SJVariableAccess("tmp_5"), SJVariableAccess("tmp_4"), "f"),
          SJCall(Some(SJVariableAccess("tmp_6")), SJVariableAccess("tmp_5"), "get", Nil),
          SJCall(Some(SJVariableAccess("tmp_7")), SJVariableAccess("tmp_6"), "get", Nil),
          SJFieldRead(SJVariableAccess("tmp_8"), SJVariableAccess("tmp_7"), "f"),
          SJFieldRead(SJVariableAccess("tmp_9"), SJVariableAccess("tmp_8"), "f"),
          SJFieldRead(SJVariableAccess("tmp_10"), SJVariableAccess("tmp_9"), "f"),
          SJFieldRead(SJVariableAccess("tmp_11"), SJVariableAccess("tmp_10"), "a"),
          SJReturn(SJVariableAccess("tmp_11"))),
                           HashMap("tmp_1" -> "Foo", "tmp_2" -> "Foo", "tmp_3" -> "Foo", "tmp_4" -> "Foo",
                                   "tmp_5" -> "Foo", "tmp_6" -> "Foo", "tmp_7" -> "Foo", "tmp_8" -> "Foo",
                                   "tmp_9" -> "Foo", "tmp_10" -> "Foo", "tmp_11" -> "int", "this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None,
                           HashMap("f" -> "Foo", "a" -> "int")))
    getASTbyParsingFileNamed("NestedCall2.txt") should equal(expected)
  }

  "Parsing While1.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJMethodDefinition(Set(), "bar", "void", Nil, List(
          SJAssignment(SJVariableAccess("i"), SJLiteral("1")),
          SJWhile(SJBinaryExpression(">", SJVariableAccess("i"), SJLiteral("0")),
                  List(SJAssignment(SJVariableAccess("i"), SJBinaryExpression("+", SJVariableAccess("i"), SJLiteral("1")))))),
                           HashMap("i" -> "int", "this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("While1.txt") should equal(expected)
  }

  "Parsing While2.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJFieldDefinition(Set(), "b", "boolean"),
        SJMethodDefinition(Set(), "bar", "void", Nil, List(
          SJAssignment(SJVariableAccess("i"), SJLiteral("1")),
          SJFieldWrite(SJVariableAccess("this"), "b", SJLiteral("true")),
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "b"),
          SJWhile(SJVariableAccess("tmp_1"),
                 List(SJFieldWrite(SJVariableAccess("this"), "b", SJLiteral("false")),
                      SJAssignment(SJVariableAccess("i"), SJBinaryExpression("+", SJVariableAccess("i"), SJLiteral("1"))),
                      SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "b")))),
                           HashMap("i" -> "int", "tmp_1" -> "boolean", "this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("b" -> "boolean")))
    getASTbyParsingFileNamed("While2.txt") should equal(expected)
  }

  "Parsing Fac.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Fac", "", List(), List(SJMethodDefinition(Set(Static()), "fac", "int",
       List(SJArgument("n", "int")), List(
         SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
            List(SJCall(Some(SJVariableAccess("x")), SJVariableAccess("this"), "fac",
                                  List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))),
                 SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("x")))),
            List(SJAssignment(SJVariableAccess("x"), SJLiteral("1")))),
         SJReturn(SJVariableAccess("x"))),
           HashMap("x" -> "int", "n" -> "int", "this" -> "Fac")),
      SJConstructorDefinition(Set(Public()), "Fac", List(), List(), HashMap("this" -> "Fac"))), None, HashMap()))
    getASTbyParsingFileNamed("Fac.txt") should equal(expected)
  }

  "Parsing Fac2.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Fac", "", List(), List(SJMethodDefinition(Set(Static()), "fac", "int",
       List(SJArgument("n", "int")), List(
         SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
            List(SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "fac",
                                  List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))),
                 SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1")))),
            List(SJAssignment(SJVariableAccess("x"), SJLiteral("1")))),
         SJReturn(SJVariableAccess("x"))),
           HashMap("x" -> "int", "tmp_1" -> "int", "n" -> "int", "this" -> "Fac")),
      SJConstructorDefinition(Set(Public()), "Fac", List(), List(), HashMap("this" -> "Fac"))), None, HashMap()))
    getASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing assert.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", List(), List(
      SJMethodDefinition(Set(), "foo", "void", List(), List(
        SJAssert(SJBinaryExpression("==", SJLiteral("5"), SJLiteral("5")))),
                       HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("assert.txt") should equal(expected)
  }

  "Parsing Tree_client.txt" should "produce the correct AST" in {
    val expected = List(
      SJInterfaceDefinition(Set(), "ITreeIterator", List(), List(
        SJMethodDefinition(Set(), "hasNext", "boolean", List(), List(), HashMap[String,String]()),
        SJMethodDefinition(Set(), "next", "int", List(), List(), HashMap[String,String]()))),
      SJInterfaceDefinition(Set(), "ITree", List(), List(
        SJMethodDefinition(Set(), "contains", "boolean", List(SJArgument("item", "int")), List(), HashMap("item" -> "int")),
        SJMethodDefinition(Set(), "add", "boolean", List(SJArgument("item", "int")), List(), HashMap("item" -> "int")),
        SJMethodDefinition(Set(), "snapshot", "ITree", List(), List(), HashMap[String,String]()),
        SJMethodDefinition(Set(), "iterator", "ITreeIterator", List(), List(), HashMap[String,String]()))),
      SJClassDefinition(Set(), "A1B1Tree", "", List("ITree"), List(
        SJConstructorDefinition(Set(Public()), "A1B1Tree", List(), List(), HashMap("this" -> "A1B1Tree"))), None, HashMap[String,String]()),
      SJClassDefinition(Set(), "World", "", List(), List(
        SJMethodDefinition(Set(), "main", "void", List(), List(
          SJNewExpression(SJVariableAccess("t"), "A1B1Tree", List()),
          SJCall(None, SJVariableAccess("t"), "add", List(SJLiteral("1"))),
          SJCall(None, SJVariableAccess("t"), "add", List(SJLiteral("2"))),
          SJCall(None, SJVariableAccess("t"), "add", List(SJLiteral("3"))),
          SJCall(Some(SJVariableAccess("s")), SJVariableAccess("t"), "snapshot", List()),
          SJCall(Some(SJVariableAccess("it")), SJVariableAccess("s"), "iterator", List()),
          SJCall(Some(SJVariableAccess("lc")), SJVariableAccess("it"), "hasNext", List()),
          SJWhile(SJVariableAccess("lc"), List(
            SJCall(Some(SJVariableAccess("x")), SJVariableAccess("it"), "next", List()),
            SJCall(None, SJVariableAccess("t"), "add", List(SJBinaryExpression("*", SJVariableAccess("x"), SJLiteral("3")))),
            SJCall(Some(SJVariableAccess("lc")), SJVariableAccess("it"), "hasNext", List()))),
          SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("t"), "contains", List(SJLiteral("1"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_1"), SJLiteral("true"))),
          SJCall(Some(SJVariableAccess("tmp_2")), SJVariableAccess("t"), "contains", List(SJLiteral("2"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_2"), SJLiteral("true"))),
          SJCall(Some(SJVariableAccess("tmp_3")), SJVariableAccess("t"), "contains", List(SJLiteral("3"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_3"), SJLiteral("true"))),
          SJCall(Some(SJVariableAccess("tmp_4")), SJVariableAccess("t"), "contains", List(SJLiteral("4"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_4"), SJLiteral("false"))),
          SJCall(Some(SJVariableAccess("tmp_5")), SJVariableAccess("t"), "contains", List(SJLiteral("5"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_5"), SJLiteral("false"))),
          SJCall(Some(SJVariableAccess("tmp_6")), SJVariableAccess("t"), "contains", List(SJLiteral("6"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_6"), SJLiteral("true"))),
          SJCall(Some(SJVariableAccess("tmp_7")), SJVariableAccess("t"), "contains", List(SJLiteral("7"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_7"), SJLiteral("false"))),
          SJCall(Some(SJVariableAccess("tmp_8")), SJVariableAccess("t"), "contains", List(SJLiteral("8"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_8"), SJLiteral("false"))),
          SJCall(Some(SJVariableAccess("tmp_9")), SJVariableAccess("t"), "contains", List(SJLiteral("9"))),
          SJAssert(SJBinaryExpression("==", SJVariableAccess("tmp_9"), SJLiteral("true")))),
                       HashMap("s" -> "ITree", "x" -> "int", "this" -> "World", "t" -> "ITree",
                               "lc" -> "boolean", "it" -> "TreeIterator",
                               "tmp_1" -> "boolean", "tmp_2" -> "boolean", "tmp_3" -> "boolean",
                               "tmp_4" -> "boolean", "tmp_5" -> "boolean", "tmp_6" -> "boolean",
                               "tmp_7" -> "boolean", "tmp_8" -> "boolean", "tmp_9" -> "boolean")),
        SJConstructorDefinition(Set(Public()), "World", List(), List(), HashMap("this" -> "World"))), None, HashMap[String,String]()))
    getASTbyParsingFileNamed("Tree_client.txt") should equal(expected)
  }

  "Parsing ClassWithConstructor.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(),"Person", "", Nil, List(
        SJFieldDefinition(Set(), "name", "String"),
        SJConstructorDefinition(Set(Public()), "Person", List(SJArgument("name","String")), List(
          SJFieldWrite(SJVariableAccess("this"), "name", SJVariableAccess("name"))),
                              HashMap("name" -> "String", "this" -> "Person"))), None, HashMap("name" -> "String")))
     getASTbyParsingFileNamed("ClassWithConstructor.txt") should equal(expected)
   }

  "Parsing OperatorTranslation.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(SJFieldDefinition(Set(), "number", "int"),
         SJMethodDefinition(Set(), "bar", "void", Nil, List(
           SJAssignment(SJVariableAccess("a"), SJLiteral("1")),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("+", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("-", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("*", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("/", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("%", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("&", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("^", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("|", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression("<<", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression(">>", SJVariableAccess("a"), SJLiteral("1"))),
           SJAssignment(SJVariableAccess("a"), SJBinaryExpression(">>>", SJVariableAccess("a"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("+", SJVariableAccess("tmp_1"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("-", SJVariableAccess("tmp_2"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_3"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("*", SJVariableAccess("tmp_3"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_4"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("/", SJVariableAccess("tmp_4"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_5"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("%", SJVariableAccess("tmp_5"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_6"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("&", SJVariableAccess("tmp_6"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_7"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("^", SJVariableAccess("tmp_7"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_8"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("|", SJVariableAccess("tmp_8"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_9"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression("<<", SJVariableAccess("tmp_9"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_10"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression(">>", SJVariableAccess("tmp_10"), SJLiteral("1"))),
           SJFieldRead(SJVariableAccess("tmp_11"), SJVariableAccess("this"), "number"),
           SJFieldWrite(SJVariableAccess("this"), "number", SJBinaryExpression(">>>", SJVariableAccess("tmp_11"), SJLiteral("1")))),
             HashMap("a" -> "int", "tmp_1" -> "int", "tmp_2" -> "int", "tmp_3" -> "int",
                     "tmp_4" -> "int", "tmp_5" -> "int", "tmp_6" -> "int", "tmp_7" -> "int",
                     "tmp_8" -> "int", "tmp_9" -> "int", "tmp_10" -> "int", "tmp_11" -> "int",
                     "this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(SJFieldWrite(SJVariableAccess("this"), "number", SJLiteral("0"))),
                              HashMap("this" -> "Foo"))), None, HashMap("number" -> "int")))
    getASTbyParsingFileNamed("OperatorTranslation.txt") should equal(expected)
  }
}


/*
  Tests related to binding
*/
class BindingsSpec extends ASTSpec {
  "Parsing Binding1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "foo", "void", Nil, List(SJAssignment(SJVariableAccess("a"), SJLiteral("20"))), HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Binding1.txt") should equal(expected)
  }

  "Parsing Binding2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "foo", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("20")),
        SJAssignment(SJVariableAccess("b"), SJLiteral("10"))),
                         HashMap("a" -> "int", "b" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Binding2.txt") should equal(expected)
  }

  "Parsing Binding3.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "int", Nil, List(SJReturn(SJLiteral("10"))), HashMap("this" -> "Foo")),
      SJMethodDefinition(Set(), "foo", "void", Nil, List(SJCall(Some(SJVariableAccess("a")), SJVariableAccess("this"), "bar", Nil)), HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Binding3.txt") should equal(expected)
  }

  "Parsing Binding4.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "a", "int"),
      SJMethodDefinition(Set(), "bar", "int", Nil, List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "a"),
        SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("this"), "a"),
        SJReturn(SJBinaryExpression("+", SJVariableAccess("tmp_2"), SJVariableAccess("tmp_1")))),
                       HashMap("tmp_1" -> "int", "tmp_2" -> "int", "this" -> "Foo")),
      SJMethodDefinition(Set(), "foo", "void", Nil, List(
        SJFieldRead(SJVariableAccess("b"), SJVariableAccess("this"), "a"),
        SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "bar", Nil),
        SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("this"), "a"),
        SJAssignment(SJVariableAccess("c"), SJBinaryExpression("+", SJVariableAccess("tmp_2"), SJVariableAccess("tmp_1")))),
                       HashMap("b" -> "int", "c" -> "int", "tmp_1" -> "int", "tmp_2" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("a" -> "int")))
    getASTbyParsingFileNamed("Binding4.txt") should equal(expected)
  }
}

/*
  Tests related to assignments
*/
class AssignmentsSpec extends ASTSpec {
  "Parsing FieldAssignment4.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(),"Point","",List(),List(
      SJConstructorDefinition(Set(),"Point",
        List(SJArgument("x","float"), SJArgument("y","float")),
        List(SJFieldWrite(SJVariableAccess("this"),"x",SJVariableAccess("x")), SJFieldWrite(SJVariableAccess("this"),"y",SJVariableAccess("y"))),
                            HashMap("this" -> "Point", "x" -> "float", "y" -> "float")),
      SJFieldDefinition(Set(),"x","float"), SJFieldDefinition(Set(),"y","float"),
      SJMethodDefinition(Set(),"flip","void",List(),List(
        SJFieldRead(SJVariableAccess("t"), SJVariableAccess("this"),"x"),
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "y"),
        SJFieldWrite(SJVariableAccess("this"),"x",SJVariableAccess("tmp_1")),
        SJFieldWrite(SJVariableAccess("this"),"y",SJVariableAccess("t"))),
                         HashMap("this" -> "Point", "t" -> "float", "tmp_1" -> "float"))),
           None, HashMap("x" -> "float", "y" -> "float")))
    getASTbyParsingFileNamed("FieldAssignment4.txt") should equal(expected)
  }

  "Parsing FieldAssignment3.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "b", "int"),
      SJMethodDefinition(Set(), "a", "int", Nil, List(SJReturn(SJLiteral("10"))), HashMap("this" -> "Foo")),
      SJMethodDefinition(Set(), "foo", "void", Nil, List(
        SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "a", Nil),
        SJFieldWrite(SJVariableAccess("this"), "b", SJVariableAccess("tmp_1"))),
                       HashMap("tmp_1" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("b" -> "int")))
    getASTbyParsingFileNamed("FieldAssignment3.txt") should equal(expected)
  }

  "Parsing FieldAssignment2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "b", "int"),
      SJMethodDefinition(Set(), "set", "void", Nil, List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "b"),
        SJFieldWrite(SJVariableAccess("this"), "b", SJBinaryExpression("+", SJVariableAccess("tmp_1"), SJLiteral("10")))),
                       HashMap("tmp_1" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("b" -> "int")))
    getASTbyParsingFileNamed("FieldAssignment2.txt") should equal(expected)
  }

  "Parsing FieldAssignment1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "b", "int"),
      SJMethodDefinition(Set(), "set", "void", Nil, List(
        SJFieldWrite(SJVariableAccess("this"), "b", SJLiteral("10"))),
                       HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("b" -> "int")))
    getASTbyParsingFileNamed("FieldAssignment1.txt") should equal(expected)
  }

  "Parsing Assignment1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("10")),
        SJAssignment(SJVariableAccess("a"), SJLiteral("20"))),
                       HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Assignment1.txt") should equal(expected)
  }

  "Parsing Assignment2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("10")),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("+", SJVariableAccess("a"), SJLiteral("20")))),
                       HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Assignment2.txt") should equal(expected)
  }

  "Parsing Assignment3.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("10")),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("+", SJVariableAccess("a"), SJLiteral("20")))),
                       HashMap("a" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Assignment3.txt") should equal(expected)
  }

  "Parsing Assignment4.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("10")),
        SJCall(Some(SJVariableAccess("a")), SJVariableAccess("this"), "foobar", List())),
                       HashMap("a" -> "int", "this" -> "Foo")),
      SJMethodDefinition(Set(), "foobar", "int", Nil, List(SJReturn(SJLiteral("10"))), HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Assignment4.txt") should equal(expected)
  }

  "Parsing Assignment5.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("10")),
        SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "foobar", List()),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("+", SJVariableAccess("a"), SJVariableAccess("tmp_1")))),
                       HashMap("a" -> "int", "tmp_1" -> "int", "this" -> "Foo")),
      SJMethodDefinition(Set(), "foobar", "int", Nil, List(SJReturn(SJLiteral("10"))), HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("Assignment5.txt") should equal(expected)
  }
}

/*
  Tests related to interfaces
*/
class InterfacesSpec extends ASTSpec {
  "Parsing Interface0.txt" should "produce the correct AST" in {
    val expected = List(SJInterfaceDefinition(Set(), "Foo", List(), List()))
    getASTbyParsingFileNamed("Interface0.txt") should equal(expected)
  }

  "Parsing Interface1.txt" should "produce the correct AST" in {
    val expected = List(SJInterfaceDefinition(Set(), "Foo", List(), List(
      SJMethodDefinition(Set(), "bar", "int", List(), List(), HashMap())
    )))
    getASTbyParsingFileNamed("Interface1.txt") should equal(expected)
  }

  "Parsing Interface2.txt" should "produce the correct AST" in {
    val expected = List(SJInterfaceDefinition(Set(), "Foo", List(), List(
      SJMethodDefinition(Set(), "bar", "void", List(), List(), HashMap())
    )))
    getASTbyParsingFileNamed("Interface2.txt") should equal(expected)
  }
}

/*
  Tests related to classes
*/
class ClassesSpec extends ASTSpec {
  "Parsing SimpleClass.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("SimpleClass.txt") should equal(expected)
  }

  "Parsing SimpleClassWithMethod.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", List(), List(
      SJMethodDefinition(Set(), "foo", "void", Nil, List(), HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("SimpleClassWithMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJMethodDefinition(Set(), "foo", "int", List(SJArgument("a", "int")),
          List(SJReturn(SJVariableAccess("a"))), HashMap("a" -> "int", "this" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "foo", "int", List(SJArgument("a", "int")), List(
        SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "foo", List(SJVariableAccess("a"))),
        SJCall(Some(SJVariableAccess("tmp_2")), SJVariableAccess("this"), "foo", List(SJVariableAccess("tmp_1"))),
        SJReturn(SJVariableAccess("tmp_2"))),
                       HashMap("a" -> "int", "tmp_1" -> "int", "tmp_2" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod2.txt") should equal(expected)
  }

  "Parsing SimpleClassWithSimpleField.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "foo", "int"),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("foo" -> "int")))
    getASTbyParsingFileNamed("SimpleClassWithSimpleField.txt") should equal(expected)
  }

  "Parsing MethodWithNoFieldAccessOrCallInAnExpression.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(),"Foo", "", List(), List(
      SJFieldDefinition(Set(), "f", "int"),
      SJMethodDefinition(Set(), "a", "int", List(), List(SJReturn(SJLiteral("10"))), HashMap("this" -> "Foo")),
      SJMethodDefinition(Set(), "b", "int", List(SJArgument("c", "int")), List(
        SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "a", List()),
        SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("this"), "f"),
        SJReturn(SJBinaryExpression("+", SJBinaryExpression("+", SJVariableAccess("tmp_2"), SJVariableAccess("tmp_1")), SJVariableAccess("c")))),
                       HashMap("c" -> "int", "tmp_1" -> "int", "tmp_2" -> "int", "this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("f" -> "int")))
    getASTbyParsingFileNamed("MethodWithNoFieldAccessOrCallInAnExpression.txt") should equal(expected)
  }

  //this fails as of now...
  "Parsing NestedClass.txt (fails at the moment)" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(), "B", "", List(), List(
        SJFieldDefinition(Set(), "a", "A"),
        SJMethodDefinition(Set(), "foo", "int", List(), List(
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "a"),
          SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("tmp_1"), "x"),
          SJReturn(SJVariableAccess("tmp_2"))),
                         HashMap("this" -> "B", "tmp_1" -> "A", "tmp_2" -> "int")),
        SJConstructorDefinition(Set(Public()), "B", List(SJArgument("a", "A")), List(
          SJFieldWrite(SJVariableAccess("this"), "a", SJVariableAccess("a"))),
                                HashMap("this" -> "B", "a" -> "A"))),
        Some("A"), HashMap("a" -> "A")),
      SJClassDefinition(Set(), "A", "", List(), List(
        SJFieldDefinition(Set(Public()), "x", "int"),
        SJMethodDefinition(Set(), "fancy", "int", List(), List(
          SJNewExpression(SJVariableAccess("tmp_1"), "B", List(SJVariableAccess("this"))),
          SJCall(Some(SJVariableAccess("tmp_2")), SJVariableAccess("tmp_1"), "foo", List()),
          SJReturn(SJVariableAccess("tmp_2"))), HashMap("this" -> "A", "tmp_1" -> "B", "tmp_2" -> "int")),
        SJConstructorDefinition(Set(Public()), "A", List(), List(), HashMap("this" -> "A"))),
      None, HashMap("x" -> "int")))
    getASTbyParsingFileNamed("NestedClass.txt") should equal(expected)
  }
}

/*
  Tests related to field initializers
*/
class InitializersSpec extends ASTSpec {

  "Parsing fieldWithInitializer.txt" should "produce the correct AST" in {
     val expected = List(SJClassDefinition(Set(), "A", "", Nil, List(
        SJFieldDefinition(Set(), "i", "int"),
        SJConstructorDefinition(Set(Public()), "A", Nil, List(
          SJFieldWrite(SJVariableAccess("this"), "i", SJLiteral("0"))), HashMap("this" -> "A"))), None, HashMap("i" -> "int")))
     getASTbyParsingFileNamed("fieldWithInitializer.txt") should equal(expected)
   }

  "Parsing fieldWithInitializerNoPreviousConstructor.txt" should "produce the correct AST" in {
     val expected = List(SJClassDefinition(Set(), "A", "", Nil, List(
       SJFieldDefinition(Set(), "i", "int"),
       SJConstructorDefinition(Set(Public()), "A", Nil, List(
         SJFieldWrite(SJVariableAccess("this"), "i", SJLiteral("0"))), HashMap("this" -> "A"))), None, HashMap("i" -> "int")))
     getASTbyParsingFileNamed("fieldWithInitializerNoPreviousConstructor.txt") should equal(expected)
   }

}

/*
  Tests related to Modifiers
*/
class ModifiersSpec extends ASTSpec {
  "Parsing ClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(Private(), Abstract()), "A", "", Nil, List(
      SJConstructorDefinition(Set(Public()), "A", List(), List(), HashMap("this" -> "A"))), None, HashMap()))
    getASTbyParsingFileNamed("ClassModifiers.txt") should equal(expected)
  }

  "Parsing InterfaceModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJInterfaceDefinition(Set(Private()), "A", Nil, Nil))
    getASTbyParsingFileNamed("InterfaceModifiers.txt") should equal(expected)
  }

  "Parsing MethodModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", List(),List(
      SJMethodDefinition(Set(Private()), "bar", "void", List(), List(), HashMap("this" -> "Foo")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap()))
    getASTbyParsingFileNamed("MethodModifiers.txt") should equal(expected)
  }

  "Parsing FieldModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(Private()), "a", "int"),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap("this" -> "Foo"))), None, HashMap("a" -> "int")))
    getASTbyParsingFileNamed("FieldModifiers.txt") should equal(expected)
  }

  "Parsing ConstructorModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "A", "", Nil, List(
      SJConstructorDefinition(Set(Private()), "A", Nil, List(), HashMap("this" -> "A"))), None, HashMap()))
    getASTbyParsingFileNamed("ConstructorModifiers.txt") should equal(expected)
  }

  "Parsing NestedClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(Private()), "B", "", List(), List(
        SJConstructorDefinition(Set(Public()), "B", List(), List(), HashMap("this" -> "B"))), Some("A"), HashMap()),
      SJClassDefinition(Set(Public()), "A", "", List(), List(
        SJConstructorDefinition(Set(Public()), "A", List(), List(), HashMap("this" -> "A"))), None, HashMap()))
    getASTbyParsingFileNamed("NestedClassModifiers.txt") should equal(expected)
  }

  "Parsing BlockModifiers.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "A", "", List(), List(SJBodyBlock(Some(Static()),List()), SJConstructorDefinition(Set(Public()), "A", List(), List(), HashMap("this" -> "A"))), None, HashMap()))
    getASTbyParsingFileNamed("BlockModifiers.txt") should equal(expected)
  }

  "Parsing NestedInterfaceModifiers.txt" should "produce the correct AST" in {
    //this fails at the moment
    val expected = List(
      SJInterfaceDefinition(Set(Private()), "B", List(), List()),
      SJInterfaceDefinition(Set(Public()), "A", List(), List()))
    getASTbyParsingFileNamed("NestedInterfaceModifiers.txt") should equal(expected)
  }
}

class FieldAssignment extends ASTSpec {

  "Parsing FieldMutationUsingThis and FieldMutationNotUsingThis" should "produce the same AST" in {
    getASTbyParsingFileNamed("FieldMutationUsingThis.txt") should equal (getASTbyParsingFileNamed("FieldMutationNotUsingThis.txt"))
  }

  "Parsing FieldMutationUsingThis2 and FieldMutationNotUsingThis2" should "produce the same AST" in {
    getASTbyParsingFileNamed("FieldMutationUsingThis2.txt") should equal (getASTbyParsingFileNamed("FieldMutationNotUsingThis2.txt"))
  }

}
