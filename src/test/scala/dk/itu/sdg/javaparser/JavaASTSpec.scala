package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.util.parsing.input.{ StreamReader }

trait ASTSpec extends FlatSpec with ShouldMatchers with JavaAST {
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

/*
 * Parses each file in src/test/resources/javaparser/source
 * and tests that the expected JavaAST AST is generated.
 */
class JavaASTSpec extends ASTSpec {

  /*
   * NOTE:
   * The easiest way to do this is to run the Main method in sbt with the
   * given file and then in Emacs: M-x query-replace-regexp RET \([a-z0-9_+-*]+\) RET "\1"
   */

  "Parsing Postfix1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("0"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("1"))))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Postfix1.txt") should equal(expected)
  }

  "Parsing Postfix2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("0"))),
        JAssignment("a", JBinaryExpression("-", JVariableAccess("a"), JLiteral("1"))))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Postfix2.txt") should equal(expected)
  }

  "Parsing Postfix3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "a", "int", None),
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JFieldWrite(JVariableAccess("this"), "a", JBinaryExpression("+", JVariableAccess("tmp_1"), JLiteral("1"))))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Postfix3.txt") should equal(expected)
  }

  "Parsing Conditional1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", List(JArgument("a", "int")), List(JBlock(None, List(
        JBinding("tmp_1", "int", None),
        JConditional(JBinaryExpression("==", JVariableAccess("a"), JLiteral("10")),
                     JBlock(None, List(JAssignment("tmp_1", JLiteral("20")))),
                     JBlock(None, List(JAssignment("tmp_1", JLiteral("30"))))),
        JBinding("b", "int", Some(JVariableAccess("tmp_1")))))))             ,
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Conditional1.txt") should equal(expected)
  }

  "Parsing Conditional2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "c", "int", None),
      JMethodDefinition(Set(), "bar", "void", List(JArgument("a", "int")), List(JBlock(None, List(
        JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("this"), "c"))),
        JBinding("tmp_1", "int", None),
        JConditional(JBinaryExpression("==", JVariableAccess("a"), JVariableAccess("tmp_2")),
                     JBlock(None, List(JAssignment("tmp_1", JLiteral("20")))),
                     JBlock(None, List(JAssignment("tmp_1", JLiteral("30"))))),
        JBinding("b", "int", Some(JVariableAccess("tmp_1")))))))             ,
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Conditional2.txt") should equal(expected)
  }

  "Parsing NestedField1.txt" should "produce the correct AST" in {
    val expected =
      List(
        JClassDefinition(Set(),"Foo", "", Nil, List(
          JFieldDefinition(Set(), "a", "int", None),
          JConstructorDefinition(Set(Public()),"Foo",List(),List())),None),
        JClassDefinition(Set(),"Bar", "", Nil, List(
          JFieldDefinition(Set(), "f", "Foo", None),
          JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
            JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))),
            JBinding("b", "int", Some(JFieldAccess(JVariableAccess("tmp_1"), "a")))
          )))),
          JConstructorDefinition(Set(Public()),"Bar",List(),List())),None))
    getASTbyParsingFileNamed("NestedField1.txt") should equal(expected)
  }

  "Parsing NestedField2.txt" should "produce the correct AST" in {
    
    val expected = List(JClassDefinition(Set(),"Foo","",List(),List(
      JFieldDefinition(Set(),"f","Foo",None), 
      JFieldDefinition(Set(),"a","int",None), 
      JMethodDefinition(Set(),"bar","int",List(),List(JBlock(None,List(
        JBinding("tmp_1","Foo",Some(JFieldAccess(JVariableAccess("this"),"f"))),
        JBinding("tmp_2","Foo",Some(JFieldAccess(JVariableAccess("tmp_1"),"f"))),
        JBinding("tmp_3","Foo",Some(JFieldAccess(JVariableAccess("tmp_2"),"f"))),
        JBinding("tmp_4","Foo",Some(JFieldAccess(JVariableAccess("tmp_3"),"f"))),
        JBinding("tmp_5","Foo",Some(JFieldAccess(JVariableAccess("tmp_4"),"f"))),
        JBinding("tmp_6","int",Some(JFieldAccess(JVariableAccess("tmp_5"),"a"))),
        JReturn(JVariableAccess("tmp_6")))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("NestedField2.txt") should equal(expected)
  }


  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition(Set(),"Foo", "", Nil, List(
        JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List()))),
        JConstructorDefinition(Set(Public()),"Foo",List(),List())),None),
      JClassDefinition(Set(),"Bar", "", Nil, List(
        JFieldDefinition(Set(), "f", "Foo", None),
        JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
          JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))),
          JCall(JVariableAccess("tmp_1"), "bar", Nil)
        )))),
        JConstructorDefinition(Set(Public()),"Bar",List(),List())),None))
    getASTbyParsingFileNamed("NestedCall1.txt") should equal(expected)
  }

  "Parsing NestedCall2.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition(Set(),"Foo", "", Nil, List(
        JFieldDefinition(Set(), "f", "Foo", None),
        JFieldDefinition(Set(), "a", "int", None),
        JMethodDefinition(Set(), "get", "Foo", Nil, List(JBlock(None, List(JBinding("tmp_1", "Foo", Some(JFieldAccess(JVariableAccess("this"), "f"))), JReturn(JVariableAccess("tmp_1")))))),
        JMethodDefinition(Set(), "bar", "int", Nil, List(JBlock(None, List(
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
          JReturn(JVariableAccess("tmp_11")))))), 
        JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("NestedCall2.txt") should equal(expected)
  }

  "Parsing While1.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition(Set(),"Foo", "", Nil, List(
        JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
          JBinding("i", "int", Some(JLiteral("1"))),
          JWhile(JBinaryExpression(">", JVariableAccess("i"), JLiteral("0")),
                 JBlock(None, List(JAssignment("i", JBinaryExpression("+", JVariableAccess("i"), JLiteral("1"))))))
        )))), 
        JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("While1.txt") should equal(expected)
  }

  "Parsing Fac.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition(Set(),"Fac", "", List(), List(JMethodDefinition(Set(Static()), "fac", "int",
       List(JArgument("n", "int")),
        List(JBlock(None, List(JBinding("x", "int", None),
          JConditional(JBinaryExpression(">=", JVariableAccess("n"), JLiteral("0")),
            JBlock(None, List(JAssignment("x", JCall(JVariableAccess("this"), "fac",
                                  List(JBinaryExpression("-", JVariableAccess("n"), JLiteral("1"))))),
                        JAssignment("x", JBinaryExpression("*", JVariableAccess("n"), JVariableAccess("x"))))),
            JBlock(None, List(JAssignment("x", JLiteral("1"))))), JReturn(JVariableAccess("x")))))), 
      JConstructorDefinition(Set(Public()),"Fac",List(),List())),None))
    getASTbyParsingFileNamed("Fac.txt") should equal(expected)
  }

  "Parsing Fac2.txt" should "produce the correct AST" in {
    val expected =
      List(JClassDefinition(Set(),"Fac", "", List(), List(
        JMethodDefinition(Set(Static()), "fac", "int",List(JArgument("n", "int")),List(
          JBlock(None, List(
            JBinding("x", "int", None),
            JConditional(JBinaryExpression(">=", JVariableAccess("n"), JLiteral("0")),JBlock(None, List(
              JAssignment("x", JCall(JVariableAccess("this"), "fac",List(JBinaryExpression("-", JVariableAccess("n"), JLiteral("1"))))),
              JAssignment("x", JBinaryExpression("*", JVariableAccess("n"), JVariableAccess("x"))))),
            JBlock(None, List(JAssignment("x", JLiteral("1"))))), JReturn(JVariableAccess("x"))
          ))
        )), 
        JConstructorDefinition(Set(Public()),"Fac",List(),List())),None)) 
    getASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing assert.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", List[String](), List(
      JMethodDefinition(Set(), "foo", "void", List[JArgument](), List(JBlock(None, List(
        JAssert(JBinaryExpression("==", JLiteral("5"), JLiteral("5"))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("assert.txt") should equal(expected)    
  }

  "Parsing Tree_client.txt" should "produce the correct AST" in {
    val expected = List(
      JInterfaceDefinition(Set(), "ITreeIterator", List[String](), List(
        JMethodDefinition(Set(), "hasNext", "boolean", List[JArgument](), List[JBodyStatement]()),
        JMethodDefinition(Set(), "next", "int", List[JArgument](), List[JBodyStatement]()))),
      JInterfaceDefinition(Set(), "ITree", List[String](), List(
        JMethodDefinition(Set(), "contains", "boolean", List(JArgument("item", "int")), List[JBodyStatement]()),
        JMethodDefinition(Set(), "add", "int", List(JArgument("item", "int")), List[JBodyStatement]()),
        JMethodDefinition(Set(), "snapshot", "ITree", List[JArgument](), List[JBodyStatement]()),
        JMethodDefinition(Set(), "iterator", "ITreeIterator", List[JArgument](), List[JBodyStatement]()))),
      JClassDefinition(Set(),"A1B1Tree", "", List("ITree"), List[JStatement](), None),
      JClassDefinition(Set(),"World", "", List[String](), List(
        JMethodDefinition(Set(), "main", "void", List[JArgument](), List(JBlock(None, List(
          JBinding("t", "ITree", Some(JNewExpression("A1B1Tree", List[JExpression]()))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("1"))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("2"))),
          JCall(JVariableAccess("t"), "add", List(JLiteral("3"))),
          JBinding("s", "ITree", Some(JCall(JVariableAccess("t"), "snapshot", List[JExpression]()))),
          JBinding("lc", "boolean", Some(JCall(JVariableAccess("it"), "hasNext", List[JExpression]()))),
          JWhile(JBinaryExpression("==", JVariableAccess("lc"), JLiteral("true")),
            JBlock(None, List(
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
      JClassDefinition(Set(),"Person","",Nil,List(
        JFieldDefinition(Set(), "name","String", None), 
        JConstructorDefinition(Set(Public()),"Person",List(JArgument("name","String")),List(
          JFieldWrite(JVariableAccess("this"),"name",JFieldAccess(JVariableAccess("this"),"name"))))),None))
     getASTbyParsingFileNamed("ClassWithConstructor.txt") should equal(expected)    
   }

  "Parsing OperatorTranslation.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(), "number","int", None),   
         JMethodDefinition(Set(), "bar","void",Nil,List(JBlock(None, List(
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
           JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression(">>>",JVariableAccess("tmp_11"),JLiteral("1"))))))),
        JConstructorDefinition(Set(Public()),"Foo",List(),List(JFieldWrite(JVariableAccess("this"),"number",JLiteral("0"))))),None))   
           
    getASTbyParsingFileNamed("OperatorTranslation.txt") should equal(expected)    
  }
}


/*
  Tests related to binding
*/
class BindingsSpec extends ASTSpec {
  "Parsing Binding1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "foo", "void", Nil, List(JBlock(None, List(JBinding("a", "int", Some(JLiteral("20"))))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Binding1.txt") should equal(expected)
  }

  "Parsing Binding2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JMethodDefinition(Set(), "foo","void",Nil,List(JBlock(None, List(
        JBinding("a","int",Some(JLiteral("20"))),
        JBinding("b","int",Some(JLiteral("10"))))
      ))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Binding2.txt") should equal(expected)
  }

  "Parsing Binding3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "int", Nil, List(JBlock(None, List(JReturn(JLiteral("10")))))),
      JMethodDefinition(Set(), "foo", "void", Nil, List(JBlock(None, List(JBinding("a", "int", Some(JCall(JVariableAccess("this"), "bar", Nil))))))), 
    JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Binding3.txt") should equal(expected)
  }

  "Parsing Binding4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "a", "int", None),
      JMethodDefinition(Set(), "bar", "int", Nil, List(JBlock(None, List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("tmp_2", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JReturn(JBinaryExpression("+", JVariableAccess("tmp_2"), JVariableAccess("tmp_1"))))))),
      JMethodDefinition(Set(), "foo", "void", Nil, List(JBlock(None, List(
        JBinding("b", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("tmp_2", "int", Some(JCall(JVariableAccess("this"), "bar", Nil))),
        JBinding("tmp_3", "int", Some(JFieldAccess(JVariableAccess("this"), "a"))),
        JBinding("c", "int", Some(JBinaryExpression("+", JVariableAccess("tmp_3"), JVariableAccess("tmp_2")))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Binding4.txt") should equal(expected)
  }
}

/*
  Tests related to assignments
*/
class AssignmentsSpec extends ASTSpec {
  "Parsing FieldAssignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "b", "int", None),
      JMethodDefinition(Set(), "a", "int", Nil, List(JBlock(None, List(JReturn(JLiteral("10")))))),
      JMethodDefinition(Set(), "foo", "void", Nil, List(JBlock(None, List(
        JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "a", Nil))),
        JFieldWrite(JVariableAccess("this"), "b", JVariableAccess("tmp_1")))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("FieldAssignment3.txt") should equal(expected)
  }

  "Parsing FieldAssignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "b", "int", None),
      JMethodDefinition(Set(), "set", "void", Nil, List(JBlock(None, List(
        JBinding("tmp_1", "int", Some(JFieldAccess(JVariableAccess("this"), "b"))),
        JFieldWrite(JVariableAccess("this"), "b", JBinaryExpression("+", JVariableAccess("tmp_1"), JLiteral("10"))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("FieldAssignment2.txt") should equal(expected)
  }

  "Parsing FieldAssignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JFieldDefinition(Set(), "b", "int", None),
      JMethodDefinition(Set(), "set", "void", Nil, List(JBlock(None, List(
        JFieldWrite(JVariableAccess("this"), "b", JLiteral("10")))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("FieldAssignment1.txt") should equal(expected)
  }
  
  "Parsing Assignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JLiteral("20")))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Assignment1.txt") should equal(expected)
  }

  "Parsing Assignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("20"))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Assignment2.txt") should equal(expected)
  }

  "Parsing Assignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JLiteral("20"))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Assignment3.txt") should equal(expected)
  }

  "Parsing Assignment4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JAssignment("a", JCall(JVariableAccess("this"), "foobar", List())))))),
      JMethodDefinition(Set(), "foobar", "int", Nil, List(JBlock(None, List(
        JReturn(JLiteral("10")))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Assignment4.txt") should equal(expected)
  }

  "Parsing Assignment5.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(
      JMethodDefinition(Set(), "bar", "void", Nil, List(JBlock(None, List(
        JBinding("a", "int", Some(JLiteral("10"))),
        JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "foobar", List()))),
        JAssignment("a", JBinaryExpression("+", JVariableAccess("a"), JVariableAccess("tmp_1"))))))),
      JMethodDefinition(Set(), "foobar", "int", Nil, List(JBlock(None, List(
        JReturn(JLiteral("10")))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("Assignment5.txt") should equal(expected)
  }
}

/*
  Tests related to interfaces
*/
class InterfacesSpec extends ASTSpec {
  "Parsing Interface0.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(), "Foo", List[String](), List[JBodyStatement]()))
    getASTbyParsingFileNamed("Interface0.txt") should equal(expected)
  }

  "Parsing Interface1.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(), "Foo", List[String](), List(
      JMethodDefinition(Set(), "bar", "int", List[JArgument](), List[JBodyStatement]())
    )))
    getASTbyParsingFileNamed("Interface1.txt") should equal(expected)
  }

  "Parsing Interface2.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(), "Foo", List[String](), List(
      JMethodDefinition(Set(), "bar", "void", List[JArgument](), List[JBodyStatement]())
    )))
    getASTbyParsingFileNamed("Interface2.txt") should equal(expected)
  }
}

/*
  Tests related to classes
*/
class ClassesSpec extends ASTSpec {
  "Parsing SimpleClass.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil, List(JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("SimpleClass.txt") should equal(expected)
  }

  "Parsing SimpleClassWithMethod.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", List(),List(
      JMethodDefinition(Set(), "foo", "void", Nil,
        List(JBlock(None, Nil))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("SimpleClassWithMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition(Set(),"Foo", "", Nil, List(
        JMethodDefinition(Set(), "foo", "int",
          List(JArgument("a", "int")),
          List(JBlock(None, List(JReturn(JVariableAccess("a")))))), 
        JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil,List(
      JMethodDefinition(Set(), "foo", "int",
        List(JArgument("a", "int")),
        List(JBlock(None, 
          List(JBinding("tmp_1", "int", Some(JCall(JVariableAccess("this"), "foo",
            List(JVariableAccess("a"))))), JBinding("tmp_2", "int", Some(JCall(JVariableAccess("this"), "foo",
            List(JVariableAccess("tmp_1"))))),
            JReturn(JVariableAccess("tmp_2")))))),
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("SimpleClassMoreComplexMethod2.txt") should equal(expected)
  }

  "Parsing SimpleClassWithSimpleField.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo", "", Nil,
      List(JFieldDefinition(Set(), "foo", "int", None), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("SimpleClassWithSimpleField.txt") should equal(expected)
  }

  "Parsing MethodWithNoFieldAccessOrCallInAnExpression.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",List(),List(
      JFieldDefinition(Set(),"f","int",None), 
      JMethodDefinition(Set(),"a","int",List(),List(JBlock(None,List(JReturn(JLiteral("10")))))), 
      JMethodDefinition(Set(),"b","int",List(JArgument("c","int")),List(
        JBlock(None,List(
          JBinding("tmp_1","int",Some(JCall(JVariableAccess("this"),"a",List()))), 
          JBinding("tmp_2","int",Some(JFieldAccess(JVariableAccess("this"),"f"))), 
          JReturn(JBinaryExpression("+",JBinaryExpression("+",JVariableAccess("tmp_2"),JVariableAccess("tmp_1")),JVariableAccess("c"))))))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("MethodWithNoFieldAccessOrCallInAnExpression.txt") should equal(expected)
  }
}

/*
  Tests related to field initializers
*/
class InitializersSpec extends ASTSpec {
  
  "Parsing fieldWithInitializer.txt" should "produce the correct AST" in {
     val expected = List(JClassDefinition(Set(),"A","",Nil,List(
        JFieldDefinition(Set(),"i","int",None), 
        JConstructorDefinition(Set(Public()),"A",Nil,List(
          JFieldWrite(JVariableAccess("this"),"i",JLiteral("0"))))),None))
     getASTbyParsingFileNamed("fieldWithInitializer.txt") should equal(expected)
   }
  
  "Parsing fieldWithInitializerNoPreviousConstructor.txt" should "produce the correct AST" in {
     val expected = List(JClassDefinition(Set(),"A","",Nil,List(
       JFieldDefinition(Set(),"i","int",None), 
       JConstructorDefinition(Set(Public()),"A",Nil,List(
         JFieldWrite(JVariableAccess("this"),"i",JLiteral("0"))))),None))
     getASTbyParsingFileNamed("fieldWithInitializerNoPreviousConstructor.txt") should equal(expected)
   }
 
}

/*
  Tests related to Modifiers
*/
class ModifiersSpec extends ASTSpec {
  "Parsing ClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(Private(), Abstract()),"A","",Nil,List(
      JConstructorDefinition(Set(Public()),"A",List(),List())),None))
    getASTbyParsingFileNamed("ClassModifiers.txt") should equal(expected)    
  }
  
  "Parsing InterfaceModifiers.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(Private()),"A",Nil,Nil))    
    getASTbyParsingFileNamed("InterfaceModifiers.txt") should equal(expected)    
  }
  
  "Parsing MethodModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",List(),List(
      JMethodDefinition(Set(Private()),"bar","void",List(),List(JBlock(None,List()))), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("MethodModifiers.txt") should equal(expected)    
  }
  
  "Parsing FieldModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(Private()),"a","int", None), 
      JConstructorDefinition(Set(Public()),"Foo",List(),List())),None))
    getASTbyParsingFileNamed("FieldModifiers.txt") should equal(expected)    
  }
  
  "Parsing ConstructorModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"A","",Nil,List(JConstructorDefinition(Set(Private()),"A",Nil,List())),None))
    getASTbyParsingFileNamed("ConstructorModifiers.txt") should equal(expected)    
  }
  
  "Parsing NestedClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(
      JClassDefinition(Set(Private()),"B","",List(),List(
        JConstructorDefinition(Set(Public()),"B",List(),List())),Some("A")), 
      JClassDefinition(Set(Public()),"A","",List(),List(
        JConstructorDefinition(Set(Public()),"A",List(),List())),None))
    getASTbyParsingFileNamed("NestedClassModifiers.txt") should equal(expected)    
  }
  
  "Parsing BlockModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"A","",List(),List(JBlock(Some(Static()),List()), JConstructorDefinition(Set(Public()),"A",List(),List())),None))
    getASTbyParsingFileNamed("BlockModifiers.txt") should equal(expected)    
  }
  
  "Parsing NestedInterfaceModifiers.txt" should "produce the correct AST" in {
    /*
      TODO: The JInterfaces are not *pulled* out in the outer scope yet. 
    */
    val expected = List(
      JInterfaceDefinition(Set(Public()),"A",List(),List(
        JInterfaceDefinition(Set(Private()),"B",List(),List()))))
    getASTbyParsingFileNamed("NestedInterfaceModifiers.txt") should equal(expected)    
  }
}
