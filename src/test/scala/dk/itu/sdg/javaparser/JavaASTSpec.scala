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
          List(JBinding("tmp_1", "int", Some(JCall("this", "foo",
            List(JVariableAccess("a"))))), JBinding("tmp_2", "int", Some(JCall("this", "foo",
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
            JBinding("tmp_1", "int", Some(JCall("this", "a", Nil))),
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
        JBinding("tmp_1", "int", Some(JCall("this", "a", Nil))),
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
      JMethodDefinition("foo", "void", Nil, List(JBlock(List(JBinding("a", "int", Some(JCall("this", "bar", Nil)))))))),
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
        JBinding("tmp_2", "int", Some(JCall("this", "bar", Nil))),
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
        JAssignment("a", JCall("this", "foobar", List())))))),
      JMethodDefinition("foobar", "int", Nil, List(JBlock(List(
        JReturn(JLiteral("10"))))))),
      None))
    getASTbyParsingFileNamed("Assignment4.txt") should equal(expected)
  }

  "Parsing Assignment5.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition("Foo", "", Nil, List(
      JMethodDefinition("bar", "void", Nil, List(JBlock(List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JBinding("tmp_1", "int", Some(JCall("this", "foobar", List()))),
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
               JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "f"))),
               JBinding("b", "int", Some(JFieldAccess(JVariableAccess("tmp_1"), "a")))
           ))))), None))
    getASTbyParsingFileNamed("NestedField1.txt") should equal(expected)
  }


  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition("Foo", "", Nil, List(JMethodDefinition("bar", "void", Nil, List(JBlock(List())))), None),
           JClassDefinition("Bar", "", Nil, List(
             JFieldDefinition("f", "Foo"),
             JMethodDefinition("bar", "void", Nil, List(JBlock(List(
               JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "f"))),
               JCall("tmp_1", "bar", Nil)
           ))))), None))
    getASTbyParsingFileNamed("NestedCall1.txt") should equal(expected)
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
            JBlock(List(JAssignment("x", JCall("this", "fac",
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
            JBlock(List(JAssignment("x", JCall("this", "fac",
                                  List(JBinaryExpression("-", JVariableAccess("n"), JLiteral("1"))))),
                        JAssignment("x", JBinaryExpression("*", JVariableAccess("n"), JVariableAccess("x"))))),
            JBlock(List(JAssignment("x", JLiteral("1"))))), JReturn(JVariableAccess("x"))))))), None))
    getASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing SnapshotTrees.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition("NodeState", "", List(), List(
        JFieldDefinition("node", "Node"),
        JFieldDefinition("yielded", "boolean"),
        JMethodDefinition("init_", "NodeState", List(JArgument("node", "Node")), List(JBlock(List(
          JFieldWrite(JVariableAccess("this"), "node", JVariableAccess("node")),
          JFieldWrite(JVariableAccess("this"), "yielded", JLiteral("false"))))))),
        Some("TreeIterator")),
     JClassDefinition("TreeIterator", "", List("Iterator<Integer>"), List(
       JFieldDefinition("oldStamp", "int"),
       JFieldDefinition("context", "Stack<NodeState>"),
       JMethodDefinition("init_", "TreeIterator",
         List(JArgument("tree", "A1B1Tree"), JArgument("oldStamp", "int")), List(JBlock(List(
           JBinding("tmp_1", "Node", Some(JFieldAccess(JVariableAccess("tree"), "root"))),
           JCall("this", "pushLeftPath", List(JVariableAccess("tmp_1"))),
           JFieldWrite(JVariableAccess("this"), "oldStamp", JVariableAccess("oldStamp")))))),
       JMethodDefinition("pushLeftPath", "void", List(JArgument("node", "Node")), List(JBlock(List(
         JWhile(JBinaryExpression("!=", JVariableAccess("node"), JLiteral("null")), JBlock(List(
           JBinding("tmp_1", "NodeState", Some(JNewExpression("NodeState", List(JVariableAccess("node"))))),
           JCall("context", "push", List(JVariableAccess("tmp_1"))),
           JBinding("tmp_2", "Node", Some(JFieldAccess(JVariableAccess("node"),"left"))),
           JAssignment("node", JVariableAccess("tmp_2"))))))))),
       JMethodDefinition("hasNext", "boolean", List(), List(JBlock(List(
         JBinding("tmp_1", "boolean", Some(JCall("context", "empty", List()))),
         JReturn(JUnaryExpression("!", JVariableAccess("tmp_1"))))))),
       JMethodDefinition("next", "Integer", List(), List(JBlock(List(
         JBinding("result", "Integer", None),
         JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "oldStamp"))),
         JConditional(JBinaryExpression("!=", JVariableAccess("stamp"), JVariableAccess("tmp_1")),
                      JBlock(List(JNewExpression("ConcurrentModificationException", List(JLiteral("Tree was modified during iteration"))))),
                      JBlock(List())),
         JBinding("tmp_2", "boolean", Some(JCall("this", "hasNext", List()))),
         JConditional(JVariableAccess("tmp_2"), JBlock(List(
           JBinding("nodeState", "NodeState", Some(JCall("context", "peek", List()))),
           JBinding("tmp_4", "Node", Some(JFieldAccess(JVariableAccess("nodeState"), "node"))),
           JBinding("tmp_5", "int", Some(JFieldAccess(JVariableAccess("tmp_4"), "item"))),
           JAssignment("result", JVariableAccess("tmp_5")),
           JFieldWrite(JVariableAccess("nodeState"), "yielded", JLiteral("true")),
           JBinding("tmp_6", "Node", Some(JFieldAccess(JVariableAccess("nodeState"), "node"))),
           JBinding("tmp_7", "Node", Some(JFieldAccess(JVariableAccess("tmp_6"), "rght"))),
           JConditional(JBinaryExpression("!=", JVariableAccess("tmp_7"), JLiteral("null")), JBlock(List(
             JBinding("tmp_8", "Node", Some(JFieldAccess(JVariableAccess("nodeState"), "node"))),
             JBinding("tmp_9", "Node", Some(JFieldAccess(JVariableAccess("tmp_8"), "rght"))),
             JCall("this", "pushLeftPath", List(JVariableAccess("tmp_9"))))),
            JBlock(List(
             JBinding("tmp_10", "NodeState", Some(JCall("context", "peek", List()))),
             JBinding("tmp_11", "boolean", Some(JFieldAccess(JVariableAccess("tmp_10"), "yielded"))),
             JBinding("tmp_12", "boolean", Some(JCall("context", "empty", List()))),
              JWhile(JBinaryExpression("&&",
                                       JUnaryExpression("!", JVariableAccess("tmp_12")),
                                       JVariableAccess("tmp_11")), JBlock(List(
                                        JCall("context", "pop", List()))))))))),
          JBlock(List(JNewExpression("Error", List(JLiteral("Iterator: No more items")))))),
         JReturn(JVariableAccess("result")))))),
       JMethodDefinition("remove", "void", List(), List(JBlock(List(
         JNewExpression("Error", List(JLiteral("remove not implemented")))))))),
       Some("A1B1Tree")),
      JClassDefinition("Node", "", List(), List(
        JFieldDefinition("item", "int"),
        JFieldDefinition("left", "Node"),
        JFieldDefinition("rght", "Node"),
        JMethodDefinition("init_", "Node", List(
          JArgument("left", "Node"), JArgument("item", "int"), JArgument("rght", "Node")), List(JBlock(List(
            JFieldWrite(JVariableAccess("this"), "left", JVariableAccess("left")),
            JFieldWrite(JVariableAccess("this"), "item", JVariableAccess("item")),
            JFieldWrite(JVariableAccess("this"), "rght", JVariableAccess("rght")))))),
        JMethodDefinition("toString", "String", List(), List(JBlock(List(
          JBinding("tmp_2", "Node", Some(JFieldAccess(JVariableAccess("this"),"rght"))),
          JBinding("tmp_1", "String", None),
          JConditional(JBinaryExpression("==", JVariableAccess("tmp_2"), JLiteral("null")),
                       JBlock(List(JAssignment("tmp_1", JLiteral("_")))),
                       JBlock(List(
                         JBinding("tmp_3", "String", Some(JCall("rght", "toString", List()))),
                         JAssignment("tmp_1", JVariableAccess("tmp_3"))))),
          JBinding("tmp_4", "int", Some(JFieldAccess(JVariableAccess("this"), "item"))),
          JBinding("tmp_6", "Node", Some(JFieldAccess(JVariableAccess("this"), "left"))),
          JBinding("tmp_5", "String", None),
          JConditional(JBinaryExpression("==", JVariableAccess("tmp_6"), JLiteral("null")),
                       JBlock(List(JAssignment("tmp_5", JLiteral("_")))),
                       JBlock(List(
                         JBinding("tmp_7", "String", Some(JCall("left", "toString", List()))),
                         JAssignment("tmp_5", JVariableAccess("tmp_7"))))),
          JReturn(JBinaryExpression("+", JBinaryExpression("+", JBinaryExpression("+", JBinaryExpression("+", JBinaryExpression("+", JLiteral("["), JVariableAccess("tmp_5")), JLiteral(",")), JVariableAccess("tmp_4")), JVariableAccess("tmp_1")), JLiteral("]")))))))),
             Some("A1B1Tree")),
      JInterfaceDefinition("ITree", List("Iterable<Integer>"), List(
        JMethodDefinition("contains", "boolean", List(JArgument("item", "int")), List()),
        JMethodDefinition("add", "boolean", List(JArgument("item", "int")),List()),
        JMethodDefinition("get", "int", List(JArgument("i", "int")), List()),
        JMethodDefinition("snapshot", "ITree", List(),List()),
        JMethodDefinition("iterator", "Iterator<Integer>", List(), List()))),
      JClassDefinition("A1B1Tree", "", List("ITree"), List(
        JFieldDefinition("root", "Node"),
        JFieldDefinition("isSnapshot", "boolean"),
        JFieldDefinition("hasSnapshot", "boolean"),
        JFieldDefinition("stamp", "int"),
        JMethodDefinition("init_", "A1B1Tree", List(JArgument("tree", "A1B1Tree")), List(JBlock(List(
          JBinding("tmp_1", "Node", Some(JFieldAccess(JVariableAccess("tree"), "root"))),
          JFieldWrite(JVariableAccess("this"), "root", JVariableAccess("tmp_1")),
          JFieldWrite(JVariableAccess("this"), "isSnapshot", JLiteral("true")))))),
        JMethodDefinition("contains", "boolean", List(JArgument("item", "int")), List(JBlock(List(
          JBinding("node", "Node", Some(JFieldAccess(JVariableAccess("this"), "root"))),
          JBinding("found", "boolean", Some(JLiteral("false"))),
          JWhile(JBinaryExpression("&&",
                                   JUnaryExpression("!", JVariableAccess("found")),
                                   JBinaryExpression("!=", JVariableAccess("node"), JLiteral("null"))),
                 JBlock(List(
                   JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
                   JConditional(JBinaryExpression("<", JVariableAccess("item"), JVariableAccess("tmp_2")),
                     JBlock(List(JBinding("tmp_3", "Node", Some(JFieldAccess(JVariableAccess("node"), "left"))),
                       JAssignment("node", JVariableAccess("tmp_3")))),
                     JBlock(List(JBinding("tmp_4", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
                       JConditional(JBinaryExpression("<", JVariableAccess("tmp_4"), JVariableAccess("item")),
                         JBlock(List(
                           JBinding("tmp_5", "Node", Some(JFieldAccess(JVariableAccess("node"), "rght"))),
                           JAssignment("node", JVariableAccess("tmp_5")))),
                          JBlock(List(JAssignment("found", JLiteral("true"))))))))))),
          JReturn(JVariableAccess("found")))))),
 JMethodDefinition("get", "int", List(JArgument("i", "int")), List(JBlock(List(
   JBinding("tmp_1", "Node", Some(JFieldAccess(JVariableAccess("this"), "root"))),
   JBinding("tmp_2", "int",
            Some(JCall("this", "getNode", List(JVariableAccess("tmp_1"), JVariableAccess("i"))))),
   JReturn(JVariableAccess("tmp_2")))))),
 JMethodDefinition("count", "int", List(JArgument("n", "Node")), List(JBlock(List(
   JBinding("res", "int", None),
   JConditional(JBinaryExpression("==", JVariableAccess("n"), JLiteral("null")),
     JBlock(List(JAssignment("res", JLiteral("0")))),
     JBlock(List(
       JBinding("tmp_1", "Node", Some(JFieldAccess(JVariableAccess("n"), "rght"))),
       JAssignment("res", JCall("this", "count", List(JVariableAccess("tmp_1")))),
       JBinding("tmp_2", "Node", Some(JFieldAccess(JVariableAccess("n"), "left"))),
       JAssignment("res", JCall("this", "count", List(JVariableAccess("tmp_2")))),
       JAssignment("res", JBinaryExpression("+", JBinaryExpression("+", JVariableAccess("res"), JLiteral("1")), JVariableAccess("res")))))),
   JReturn(JVariableAccess("res")))))),
  JMethodDefinition("getNode", "int", List(JArgument("n", "Node"), JArgument("i", "int")), List(JBlock(List(
    JBinding("tmp_1", "Node", Some(JFieldAccess(JVariableAccess("n"), "left"))),
    JBinding("leftCount", "int", Some(JCall("this", "count", List(JVariableAccess("tmp_1"))))),
    JWhile(JBinaryExpression("!=", JVariableAccess("i"), JVariableAccess("leftCount")), JBlock(List(
      JConditional(JBinaryExpression("<", JVariableAccess("i"), JVariableAccess("leftCount")),
       JBlock(List(
         JBinding("tmp_3", "Node", Some(JFieldAccess(JVariableAccess("n"), "left"))),
         JAssignment("n", JVariableAccess("tmp_3")))),
       JBlock(List(JBinding("tmp_4", "Node", Some(JFieldAccess(JVariableAccess("n"), "rght"))),
                   JAssignment("n", JVariableAccess("tmp_4")),
                   JAssignment("i", JBinaryExpression("-", JBinaryExpression("-", JVariableAccess("i"), JVariableAccess("leftCount")), JLiteral("1")))))),
      JBinding("tmp_5", "Node", Some(JFieldAccess(JVariableAccess("n"), "left"))),
      JAssignment("leftCount", JCall("this", "count", List(JVariableAccess("tmp_5"))))))),
    JBinding("tmp_6", "int", Some(JFieldAccess(JVariableAccess("n"), "item"))),
    JReturn(JVariableAccess("tmp_6")))))),
        JMethodDefinition("add", "boolean", List(JArgument("item", "int")), List(JBlock(List(
          JBinding("updated", "RefBool", Some(JNewExpression("RefBool", List()))),
          JBinding("tmp_2", "boolean", Some(JFieldAccess(JVariableAccess("this"), "isSnapshot"))),
          JConditional(JVariableAccess("tmp_2"),
                       JBlock(List(JNewExpression("RuntimeException", List(JLiteral("Illegal to add to snapshot"))))),
                       JBlock(List(JBinding("tmp_3", "Node", Some(JFieldAccess(JVariableAccess("this"), "root"))),
                                   JBinding("tmp_4", "Node", Some(JCall("this", "addRecursive", List(JVariableAccess("tmp_3"), JVariableAccess("item"), JVariableAccess("updated"))))),
                                   JFieldWrite(JVariableAccess("this"), "root", JVariableAccess("tmp_4"))))),
          JBinding("tmp_5", "boolean", Some(JFieldAccess(JVariableAccess("updated"), "value"))),
          JReturn(JVariableAccess("tmp_5")))))),
  JMethodDefinition("addRecursive", "Node",
                    List(JArgument("node", "Node"), JArgument("item", "int"), JArgument("updated", "RefBool")), List(JBlock(List(
    JBinding("res", "Node", Some(JVariableAccess("node"))),
    JConditional(JBinaryExpression("==", JVariableAccess("node"), JLiteral("null")),
      JBlock(List(
        JFieldWrite(JVariableAccess("updated"), "value", JLiteral("true")),
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "stamp"))),
        JFieldWrite(JVariableAccess("this"), "stamp", JBinaryExpression("+", JVariableAccess("tmp_1"), JLiteral("1"))),
        JAssignment("res", JNewExpression("Node", List(JVariableAccess("item")))))),
      JBlock(List(
        JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
        JConditional(JBinaryExpression("<", JVariableAccess("item"), JVariableAccess("tmp_2")),
          JBlock(List(
            JBinding("tmp_3", "Node", Some(JFieldAccess(JVariableAccess("node"), "left"))),
            JBinding("newLeft", "Node", Some(JCall("this", "addRecursive", List(JVariableAccess("tmp_3"), JVariableAccess("item"), JVariableAccess("updated"))))),
            JBinding("tmp_5", "boolean", Some(JFieldAccess(JVariableAccess("updated"), "value"))),
            JBinding("tmp_6", "boolean", Some(JFieldAccess(JVariableAccess("this"), "hasSnapshot"))),
            JConditional(JBinaryExpression("&&", JVariableAccess("tmp_6"), JVariableAccess("tmp_5")),
              JBlock(List(
                JBinding("tmp_7", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
                JAssignment("res", JFieldAccess(JVariableAccess("node"), "rght")),
                JAssignment("res", JNewExpression("Node", List(JVariableAccess("newLeft"), JVariableAccess("tmp_7"), JVariableAccess("res")))))),
              JBlock(List(JFieldWrite(JVariableAccess("node"), "left", JVariableAccess("newLeft"))))))),
          JBlock(List(
            JBinding("tmp_8", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
            JConditional(JBinaryExpression("<", JVariableAccess("tmp_8"), JVariableAccess("item")),
              JBlock(List(
                JBinding("tmp_9", "Node", Some(JFieldAccess(JVariableAccess("node"), "rght"))),
                JBinding("newRght", "Node", Some(JCall("this", "addRecursive", List(JVariableAccess("tmp_9"), JVariableAccess("item"), JVariableAccess("updated"))))),
                JBinding("tmp_11", "boolean", Some(JFieldAccess(JVariableAccess("updated"), "value"))),
                JBinding("tmp_12", "boolean", Some(JFieldAccess(JVariableAccess("this"), "hasSnapshot"))),
                JConditional(JBinaryExpression("&&", JVariableAccess("tmp_12"), JVariableAccess("tmp_11")),
                  JBlock(List(
                    JAssignment("res", JFieldAccess(JVariableAccess("node"), "left")),
                    JBinding("tmp_13", "int", Some(JFieldAccess(JVariableAccess("node"), "item"))),
                    JAssignment("res", JNewExpression("Node", List(JVariableAccess("res"), JVariableAccess("tmp_13"), JVariableAccess("newRght")))))),
                  JBlock(List(JFieldWrite(JVariableAccess("node"), "rght", JVariableAccess("newRght"))))))),
              JBlock(List())))))))),
          JReturn(JVariableAccess("res")))))),
 JMethodDefinition("snapshot", "ITree", List(), List(JBlock(List(
   JBinding("tmp_1", "boolean", Some(JFieldAccess(JVariableAccess("this"), "isSnapshot"))),
   JConditional(JVariableAccess("tmp_1"),
                JBlock(List(JNewExpression("RuntimeException", List(JLiteral("Illegal to snapshot a snapshot"))))),
                JBlock(List(JFieldWrite(JVariableAccess("this"), "hasSnapshot", JLiteral("true"))))),
   JBinding("tmp_2", "A1B1Tree", Some(JNewExpression("A1B1Tree", List(JVariableAccess("this"))))),
   JReturn(JVariableAccess("tmp_2")))))),
 JMethodDefinition("iterator", "Iterator<Integer>", List(), List(JBlock(List(
   JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "stamp"))),
   JBinding("tmp_2", "TreeIterator", Some(JNewExpression("TreeIterator", List(JVariableAccess("this"), JVariableAccess("tmp_1"))))),
   JReturn(JVariableAccess("tmp_2")))))),
 JMethodDefinition("toString", "String", List(), List(JBlock(List(
   JBinding("tmp_1", "String", Some(JCall("root", "toString", List()))),
   JReturn(JVariableAccess("tmp_1"))))))),None),
JClassDefinition("RefBool", "", List(), List(JFieldDefinition("value", "boolean")), None))
    getASTbyParsingFileNamed("SnapshotTrees.txt") should equal(expected)
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
