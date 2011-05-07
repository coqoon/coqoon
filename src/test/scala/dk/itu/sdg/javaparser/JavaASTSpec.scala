package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.util.parsing.input.{ StreamReader }

/*
 * Parses each file in src/test/resources/javaparser/source
 * and tests that the expected JavaAST AST is generated.
 */
class JavaASTSpec extends FlatSpec with ShouldMatchers with JavaAST {

  /*
   * NOTE:
   * The easiest way to do this is to run the Main method in sbt with the
   * given file and then in Emacs: M-x query-replace-regexp RET \([a-z0-9_+-*]+\) RET "\1"
   */

  "Parsing SimpleClass.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, Nil, None))
    getASTbyParsingFileNamed("SimpleClass.txt") should equal(expected)
  }

  "Parsing SimpleClassWithMethod.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", List(),
      List(JMethodDefinition("foo", "void", Nil,
        List(JBlock(Nil)))), None))
    getASTbyParsingFileNamed("SimpleClassWithMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition("Foo", "", Nil,
        List(
          JMethodDefinition("foo", "int",
            List(JArgument("a", "int")),
            List(JBlock(List(JReturn(JVariableAccess("a"))))))), None))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil,
      List(JMethodDefinition("foo", "int",
        List(JArgument("a", "int")),
        List(JBlock(
          List(JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "foo",
            List(JVariableAccess("a"))))), JBinding("tmp_2", "int", Some(JCall(JVariableAccess("this"), "foo",
            List(JVariableAccess("tmp_1"))))),
            JReturn(JVariableAccess("tmp_2"))))))), None))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod2.txt") should equal(expected)
  }

  "Parsing SimpleClassWithSimpleField.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil,
      List(JFieldDefinition("foo", "int")), None))
    getASTbyParsingFileNamed("SimpleClassWithSimpleField.txt") should equal(expected)
  }

  "Parsing MethodWithNoFieldAccessOrCallInAnExpression.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil,
      List(JFieldDefinition("f", "int"),
        JMethodDefinition("a", "int", Nil, List(JBlock(List(JReturn(JLiteral("10")))))),
        JMethodDefinition("b", "int", List(JArgument("c", "int")),
          List(JBlock(List(
            JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "a", Nil))),
            JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("this"), "f"))),
            JReturn(JBinaryExpression("+", JBinaryExpression("+", JVariableAccess("tmp_2"), JVariableAccess("tmp_1")), JVariableAccess("c")))))))),
      None))
    getASTbyParsingFileNamed("MethodWithNoFieldAccessOrCallInAnExpression.txt") should equal(expected)
  }

  "Parsing FieldAssignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("b", "int"),
      JMethodDefinition("a", "int", Nil, List(JBlock(List(JReturn(JLiteral("10")))))),
      JMethodDefinition("foo", "void", Nil, List(JBlock(List(
        JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "a", Nil))),
        JFieldWrite(JVariableAccess("this"), "b", JVariableAccess("tmp_1"))))))),
      None))
    getASTbyParsingFileNamed("FieldAssignment3.txt") should equal(expected)
  }

  "Parsing FieldAssignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("b", "int"),
      JMethodDefinition("set", "void", Nil, List(JBlock(List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "b"))),
        JFieldWrite(JVariableAccess("this"), "b", JBinaryExpression("+", JVariableAccess("tmp_1"), JLiteral("10")))))))),
      None))
    getASTbyParsingFileNamed("FieldAssignment2.txt") should equal(expected)
  }

  "Parsing FieldAssignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("b", "int"),
      JMethodDefinition("set", "void", Nil, List(JBlock(List(
        JFieldWrite(JVariableAccess("this"), "b", JLiteral("10"))))))),
      None))
    getASTbyParsingFileNamed("FieldAssignment1.txt") should equal(expected)
  }

  "Parsing Binding1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("foo", "void", Nil, List(JBlock(List(JBinding("a", "int", Some(JLiteral("20")))))))),
      None))
    getASTbyParsingFileNamed("Binding1.txt") should equal(expected)
  }

  "Parsing Binding2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo","",Nil,List(
      JMethodDefinition("foo","void",Nil,List(JBlock(List(
        JBinding("a","int",Some(JLiteral("20"))),
        JBinding("b","int",Some(JLiteral("10"))))
      )))
    ),None))
    getASTbyParsingFileNamed("Binding2.txt") should equal(expected)
  }

  "Parsing Binding3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "int", Nil, List(JBlock(List(JReturn(JLiteral("10")))))),
      JMethodDefinition("foo", "void", Nil, List(JBlock(List(JBinding("a", "int", Some(JCall(JVariableAccess("this"), "bar", Nil)))))))),
      None))
    getASTbyParsingFileNamed("Binding3.txt") should equal(expected)
  }

  "Parsing Binding4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("a", "int"),
      JMethodDefinition("bar", "int", Nil, List(JBlock(List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JReturn(JBinaryExpression("+", JVariableAccess("tmp_2"), JVariableAccess("tmp_1"))))))),
      JMethodDefinition("foo", "void", Nil, List(JBlock(List(
        JBinding("b", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("tmp_2", "int", Some(JCall(JVariableAccess("this"), "bar", Nil))),
        JBinding("tmp_3", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("c", "int", Some(JBinaryExpression("+", JVariableAccess("tmp_3"), JVariableAccess("tmp_2"))))))))),
      None))
    getASTbyParsingFileNamed("Binding4.txt") should equal(expected)
  }

  "Parsing Interface0.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition("Foo", List[String](), List[JBodyStatement]()))
    getASTbyParsingFileNamed("Interface0.txt") should equal(expected)
  }

  "Parsing Interface1.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition("Foo", List[String](), List(
      JMethodDefinition("bar", "int", List[JArgument](), List[JBodyStatement]())
    )))
    getASTbyParsingFileNamed("Interface1.txt") should equal(expected)
  }

  "Parsing Interface2.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition("Foo", List[String](), List(
      JMethodDefinition("bar", "void", List[JArgument](), List[JBodyStatement]())
    )))
    getASTbyParsingFileNamed("Interface2.txt") should equal(expected)
  }

  "Parsing Assignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JLiteral("20"))))))), None))
    getASTbyParsingFileNamed("Assignment1.txt") should equal(expected)
  }

  "Parsing Assignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("20")))))))),
      None))
    getASTbyParsingFileNamed("Assignment2.txt") should equal(expected)
  }

  "Parsing Assignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("20")))))))),
      None))
    getASTbyParsingFileNamed("Assignment3.txt") should equal(expected)
  }

  "Parsing Assignment4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JCall(JVariableAccess("this"), "foobar", List())))))),
      JMethodDefinition("foobar", "int", Nil, List(JBlock(List(
        JReturn(JLiteral("10"))))))),
      None))
    getASTbyParsingFileNamed("Assignment4.txt") should equal(expected)
  }

  "Parsing Assignment5.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "foobar", List()))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JVariableAccess("tmp_1"))))))),
      JMethodDefinition("foobar", "int", Nil, List(JBlock(List(
        JReturn(JLiteral("10"))))))),
      None))
    getASTbyParsingFileNamed("Assignment5.txt") should equal(expected)
  }

  "Parsing Postfix1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("0"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("1")))))))),
      None))
    getASTbyParsingFileNamed("Postfix1.txt") should equal(expected)
  }

  "Parsing Postfix2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("0"))),
        JAssignment("a", JBinaryExpression("-", JVariableAccess("a"), JLiteral("1")))))))),
      None))
    getASTbyParsingFileNamed("Postfix2.txt") should equal(expected)
  }

  "Parsing Postfix3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("a", "int"),
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JFieldWrite(JVariableAccess("this"), "a", JBinaryExpression("+", JVariableAccess("tmp_1"), JLiteral("1")))))))),
      None))
    getASTbyParsingFileNamed("Postfix3.txt") should equal(expected)
  }

  "Parsing Conditional1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", List(JArgument("a", "int")), List(JBlock(List(
        JBinding("tmp_1", "int", None),
        JConditional(JBinaryExpression("==", JVariableAccess("a"), JLiteral("10")),
                     JBlock(List(JAssignment("tmp_1", JLiteral("20")))),
                     JBlock(List(JAssignment("tmp_1", JLiteral("30"))))),
        JBinding("b", "int", Some(JVariableAccess("tmp_1")))))))),
      None))
    getASTbyParsingFileNamed("Conditional1.txt") should equal(expected)
  }

  "Parsing Conditional2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JFieldDefinition("c", "int"),
      JMethodDefinition("bar", "void", List(JArgument("a", "int")), List(JBlock(List(
        JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("this"), "c"))),
        JBinding("tmp_1", "int", None),
        JConditional(JBinaryExpression("==", JVariableAccess("a"), JVariableAccess("tmp_2")),
                     JBlock(List(JAssignment("tmp_1", JLiteral("20")))),
                     JBlock(List(JAssignment("tmp_1", JLiteral("30"))))),
        JBinding("b", "int", Some(JVariableAccess("tmp_1")))))))),
      None))
    getASTbyParsingFileNamed("Conditional2.txt") should equal(expected)
  }

  "Parsing NestedField1.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(JFieldDefinition("a", "int")), None),
           JClassDefinition("Bar", "", Nil, List(
             JFieldDefinition("f", "Foo"),
             JMethodDefinition("bar", "void", Nil, List(JBlock(List(
               JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))),
               JBinding("b", "int", Some(JFieldAccess(JVariableAccess("tmp_1"), "a")))
           ))))), None))
    getASTbyParsingFileNamed("NestedField1.txt") should equal(expected)
  }

  "Parsing NestedField2.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(
        JFieldDefinition("f", "Foo"),
        JFieldDefinition("a", "int"),
        JMethodDefinition("bar","int",Nil, List(JBlock(List(
          JBinding("tmp_1","Foo",Some(JFieldAccess(JVariableAccess("this"),"f"))),
          JBinding("tmp_2","Foo",Some(JFieldAccess(JVariableAccess("tmp_1"),"f"))),
          JBinding("tmp_3","Foo",Some(JFieldAccess(JVariableAccess("tmp_2"),"f"))),
          JBinding("tmp_4","Foo",Some(JFieldAccess(JVariableAccess("tmp_3"),"f"))),
          JBinding("tmp_5","Foo",Some(JFieldAccess(JVariableAccess("tmp_4"),"f"))),
          JBinding("tmp_6","int",Some(JFieldAccess(JVariableAccess("tmp_5"),"a"))),
          JReturn(JVariableAccess("tmp_6"))))))),None))
    getASTbyParsingFileNamed("NestedField2.txt") should equal(expected)
  }


  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(JMethodDefinition("bar", "void", Nil, List(JBlock(List())))), None),
           JClassDefinition("Bar", "", Nil, List(
             JFieldDefinition("f", "Foo"),
             JMethodDefinition("bar", "void", Nil, List(JBlock(List(
               JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))),
               JCall(JVariableAccess("tmp_1"), "bar", Nil)
           ))))), None))
    getASTbyParsingFileNamed("NestedCall1.txt") should equal(expected)
  }

  "Parsing NestedCall2.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(
        JFieldDefinition("f", "Foo"),
        JFieldDefinition("a", "int"),
        JMethodDefinition("get", "Foo", Nil, List(JBlock(List(JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))), JReturn(JVariableAccess("tmp_1")))))),
        JMethodDefinition("bar", "int", Nil, List(JBlock(List(
          JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))),
          JBinding("tmp_2", "Foo", Some(JCall(JVariableAccess("tmp_1"), "get", Nil))),
          JBinding("tmp_3", "Foo", Some(JCall(JVariableAccess("tmp_2"), "get", Nil))),
          JBinding("tmp_4", "Foo", Some(JFieldAccess(JVariableAccess("tmp_3"), "f"))),
          JBinding("tmp_5", "Foo", Some(JFieldAccess(JVariableAccess("tmp_4"), "f"))),
          JBinding("tmp_6", "Foo", Some(JCall(JVariableAccess("tmp_5"), "get", Nil))),
          JBinding("tmp_7", "Foo", Some(JCall(JVariableAccess("tmp_6"), "get", Nil))),
          JBinding("tmp_8", "Foo", Some(JFieldAccess(JVariableAccess("tmp_7"), "f"))),
          JBinding("tmp_9", "Foo", Some(JFieldAccess(JVariableAccess("tmp_8"), "f"))),
          JBinding("tmp_10", "Foo", Some(JFieldAccess(JVariableAccess("tmp_9"), "f"))),
          JBinding("tmp_11", "int", Some(JFieldAccess(JVariableAccess("tmp_10"), "a"))),
          JReturn(JVariableAccess("tmp_11"))
))))), None))
    getASTbyParsingFileNamed("NestedCall2.txt") should equal(expected)
  }

  "Parsing While1.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(
        JMethodDefinition("bar", "void", Nil, List(JBlock(List(
          JBinding("i", "int", Some(JLiteral("1"))),
          JWhile(JBinaryExpression(">", JVariableAccess("i"), JLiteral("0")),
                 JBlock(List(JAssignment("i", JBinaryExpression("+", JVariableAccess("i"), JLiteral("1"))))))
        ))))), None))
    getASTbyParsingFileNamed("While1.txt") should equal(expected)
  }

  "Parsing Fac.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Fac", "", List(), List(JMethodDefinition("fac", "int",
       List(JArgument("n", "int")),
        List(JBlock(List(JBinding("x", "int", None),
          JConditional(JBinaryExpression(">=", JVariableAccess("n"), JLiteral("0")),
            JBlock(List(JAssignment("x", JCall(JVariableAccess("this"), "fac",
                                  List(JBinaryExpression("-", JVariableAccess("n"), JLiteral("1"))))),
                        JAssignment("x", JBinaryExpression("*", JVariableAccess("n"), JVariableAccess("x"))))),
            JBlock(List(JAssignment("x", JLiteral("1"))))), JReturn(JVariableAccess("x"))))))), None))
    getASTbyParsingFileNamed("Fac.txt") should equal(expected)
  }

  "Parsing Fac2.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Fac", "", List(), List(JMethodDefinition("fac", "int",
       List(JArgument("n", "int")),
        List(JBlock(List(JBinding("x", "int", None),
          JConditional(JBinaryExpression(">=", JVariableAccess("n"), JLiteral("0")),
            JBlock(List(JAssignment("x", JCall(JVariableAccess("this"), "fac",
                                  List(JBinaryExpression("-", JVariableAccess("n"), JLiteral("1"))))),
                        JAssignment("x", JBinaryExpression("*", JVariableAccess("n"), JVariableAccess("x"))))),
            JBlock(List(JAssignment("x", JLiteral("1"))))), JReturn(JVariableAccess("x"))))))), None))
    getASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing assert.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", List[String](), List(
      JMethodDefinition("foo", "void", List[JArgument](), List(JBlock(List(
        JAssert(JBinaryExpression("==", JLiteral("5"), JLiteral("5")))))))), None))
    getASTbyParsingFileNamed("assert.txt") should equal(expected)    
  }

  "Parsing Tree_client.txt" should "produce the correct AST" in {
    val expected = List(
      JInterfaceDefinition("ITreeIterator", List[String](), List(
        JMethodDefinition("hasNext", "boolean", List[JArgument](), List[JBodyStatement]()),
        JMethodDefinition("next", "int", List[JArgument](), List[JBodyStatement]()))),
      JInterfaceDefinition("ITree", List[String](), List(
        JMethodDefinition("contains", "boolean", List(JArgument("item", "int")), List[JBodyStatement]()),
        JMethodDefinition("add", "int", List(JArgument("item", "int")), List[JBodyStatement]()),
        JMethodDefinition("snapshot", "ITree", List[JArgument](), List[JBodyStatement]()),
        JMethodDefinition("iterator", "ITreeIterator", List[JArgument](), List[JBodyStatement]()))),
      JClassDefinition("A1B1Tree", "", List("ITree"), List[JStatement](), None),
      JClassDefinition("World", "", List[String](), List(
        JMethodDefinition("main", "void", List[JArgument](), List(JBlock(List(
          JBinding("t", "ITree", Some(JNewExpression("A1B1Tree", List[JExpression]()))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("1"))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("2"))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("3"))),
          JBinding("s", "ITree", Some(JCall(JVariableAccess("t"), "snapshot", List[JExpression]()))),
          JBinding("lc", "boolean", Some(JCall(JVariableAccess("it"), "hasNext", List[JExpression]()))),
          JWhile(JBinaryExpression("==", JVariableAccess("lc"), JLiteral("true")),
            JBlock(List(
              JBinding("x", "int", Some(JCall(JVariableAccess("it"), "next", List[JExpression]()))),
              JCall(JVariableAccess("t"), "add", List(JBinaryExpression("*", JVariableAccess("x"), JLiteral("3")))),
              JAssignment("lc", JCall(JVariableAccess("it"), "hasNext", List[JExpression]()))))),
          JBinding("tmp_1", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("1"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_1"), JLiteral("true"))),
          JBinding("tmp_2", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("2"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_2"), JLiteral("true"))),
          JBinding("tmp_3", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("3"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_3"), JLiteral("true"))),
          JBinding("tmp_4", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("4"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_4"), JLiteral("false"))),
          JBinding("tmp_5", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("5"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_5"), JLiteral("false"))),
          JBinding("tmp_6", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("6"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_6"), JLiteral("true"))),
          JBinding("tmp_7", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("7"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_7"), JLiteral("false"))),
          JBinding("tmp_8", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("8"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_8"), JLiteral("false"))),
          JBinding("tmp_9", "boolean", Some(JCall(JVariableAccess("t"), "contains", List(JLiteral("9"))))),
          JAssert(JBinaryExpression("==", JVariableAccess("tmp_9"), JLiteral("true")))))))), None))
    getASTbyParsingFileNamed("Tree_client.txt") should equal(expected)
  }

  "Parsing ClassWithConstructor.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition("Person","",Nil,List(
        JFieldDefinition("name","String"), 
        JConstructorDefinition("new","Person",List(JArgument("name","String")),
          List(JBlock(List(JFieldWrite(JVariableAccess("this"),"name",JFieldAccess(JVariableAccess("this"),"name"))))))),None))
     getASTbyParsingFileNamed("ClassWithConstructor.txt") should equal(expected)    
   }

  "Parsing OperatorTranslation.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo","",Nil,List(JFieldDefinition("number","int"),   
         JMethodDefinition("bar","void",Nil,List(JBlock(List(
           JBinding("a","int",Some(JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("+",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("-",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("*",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("/",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("%",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("&",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("^",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("|",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression("<<",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression(">>",JVariableAccess("a"),JLiteral("1"))), 
           JAssignment("a",JBinaryExpression(">>>",JVariableAccess("a"),JLiteral("1"))), 
           JBinding("tmp_1","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("+",JVariableAccess("tmp_1"),JLiteral("1"))), 
           JBinding("tmp_2","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("-",JVariableAccess("tmp_2"),JLiteral("1"))), 
           JBinding("tmp_3","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("*",JVariableAccess("tmp_3"),JLiteral("1"))), 
           JBinding("tmp_4","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("/",JVariableAccess("tmp_4"),JLiteral("1"))), 
           JBinding("tmp_5","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("%",JVariableAccess("tmp_5"),JLiteral("1"))), 
           JBinding("tmp_6","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("&",JVariableAccess("tmp_6"),JLiteral("1"))), 
           JBinding("tmp_7","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("^",JVariableAccess("tmp_7"),JLiteral("1"))), 
           JBinding("tmp_8","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("|",JVariableAccess("tmp_8"),JLiteral("1"))), 
           JBinding("tmp_9","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("<<",JVariableAccess("tmp_9"),JLiteral("1"))), 
           JBinding("tmp_10","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression(">>",JVariableAccess("tmp_10"),JLiteral("1"))), 
           JBinding("tmp_11","int",Some(JFieldAccess(JVariableAccess("this"),"number"))), 
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression(">>>",JVariableAccess("tmp_11"),JLiteral("1")))))))),None))
    getASTbyParsingFileNamed("OperatorTranslation.txt") should equal(expected)    
  }

  /*
   * Returns the JavaAST produced by parsing the file named "name" inside of the
   * folder src/test/resources/javaparser/source.
   */
  def getASTbyParsingFileNamed(name : String) : List[JStatement] = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(getSourceFileNamed(name))))
    FinishAST.doitHelper(parseH(in))
  }

  def getSourceFileNamed(name : String) : File = {
    new File(List("src", "test", "resources", "javaparser", "source", name).mkString(File.separator))
  }

}
