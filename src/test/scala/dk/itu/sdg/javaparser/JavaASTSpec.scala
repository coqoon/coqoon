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
  // TODO: This pretty much just crashed the parser.
    val expected = Nil
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
