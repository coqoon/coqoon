/*
  This tests the conversion from JavaAST to SimpleJavaAST (well, not yet but in the future 
  it will) It parses each file in src/test/resources/javaparser/source and tests that the 
  expected SimpleJavaAST AST is generated.
  
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
                        HashMap("a" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Postfix1.txt") should equal(expected)
  }

  "Parsing Postfix2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJAssignment(SJVariableAccess("a"), SJLiteral("0")),
        SJAssignment(SJVariableAccess("a"), SJBinaryExpression("-", SJVariableAccess("a"), SJLiteral("1")))),
                      HashMap("a" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Postfix2.txt") should equal(expected)
  }

  "Parsing Postfix3.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "a", "int"),
      SJMethodDefinition(Set(), "bar", "void", Nil, List(
        SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "a"),
        SJFieldWrite(SJVariableAccess("this"), "a", SJBinaryExpression("+", SJVariableAccess("tmp_1"), SJLiteral("1")))),
                       HashMap("tmp_1" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Postfix3.txt") should equal(expected)
  }

  "Parsing Conditional1.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJMethodDefinition(Set(), "bar", "void", List(SJArgument("a", "int")), List(
        SJConditional(SJBinaryExpression("==", SJVariableAccess("a"), SJLiteral("10")),
                      List(SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("20"))),
                      List(SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("30")))),
        SJAssignment(SJVariableAccess("b"), SJVariableAccess("tmp_1"))),
                       HashMap("tmp_1" -> "int", "b" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Conditional1.txt") should equal(expected)
  }

  //TODO: optimizer should get rid of tmp_1 (assign directly to b)!
  "Parsing Conditional2.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", Nil, List(
      SJFieldDefinition(Set(), "c", "int"),
      SJMethodDefinition(Set(), "bar", "void", List(SJArgument("a", "int")), List(
        SJFieldRead(SJVariableAccess("tmp_2"), SJVariableAccess("this"), "c"),
        SJConditional(SJBinaryExpression("==", SJVariableAccess("a"), SJVariableAccess("tmp_2")),
                      List(SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("20"))),
                      List(SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("30")))),
        SJAssignment(SJVariableAccess("b"), SJVariableAccess("tmp_1"))),
                       HashMap("tmp_1" -> "int", "tmp_2" -> "int", "b" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Conditional2.txt") should equal(expected)
  }

  "Parsing NestedField1.txt" should "produce the correct AST" in {
    val expected =
      List(
        SJClassDefinition(Set(), "Foo", "", Nil, List(
          SJFieldDefinition(Set(), "a", "int"),
          SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None),
        SJClassDefinition(Set(), "Bar", "", Nil, List(
          SJFieldDefinition(Set(), "f", "Foo"),
          SJMethodDefinition(Set(), "bar", "void", Nil, List(
            SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
            SJFieldRead(SJVariableAccess("b"), SJVariableAccess("tmp_1"), "a")),
                             HashMap("tmp_1" -> "Foo", "b" -> "int")),
          SJConstructorDefinition(Set(Public()), "Bar", List(), List(), HashMap())), None))
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
                               "tmp_4" -> "Foo", "tmp_5" -> "Foo", "tmp_6" -> "int")),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("NestedField2.txt") should equal(expected)
  }


  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJMethodDefinition(Set(), "bar", "void", Nil, List(), HashMap()),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None),
      SJClassDefinition(Set(), "Bar", "", Nil, List(
        SJFieldDefinition(Set(), "f", "Foo"),
        SJMethodDefinition(Set(), "bar", "void", Nil, List(
          SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "f"),
          SJCall(None, SJVariableAccess("tmp_1"), "bar", Nil)),
                           HashMap("tmp_1" -> "Foo")),
        SJConstructorDefinition(Set(Public()), "Bar", List(), List(), HashMap())), None))
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
                           HashMap("tmp_1" -> "Foo")),
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
                                   "tmp_9" -> "Foo", "tmp_10" -> "Foo", "tmp_11" -> "int")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("NestedCall2.txt") should equal(expected)
  }

  "Parsing While1.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Foo", "", Nil, List(
        SJMethodDefinition(Set(), "bar", "void", Nil, List(
          SJAssignment(SJVariableAccess("i"), SJLiteral("1")),
          SJWhile(SJBinaryExpression(">", SJVariableAccess("i"), SJLiteral("0")),
                  List(SJAssignment(SJVariableAccess("i"), SJBinaryExpression("+", SJVariableAccess("i"), SJLiteral("1")))))),
                           HashMap("i" -> "int")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
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
          SJWhile(SJBinaryExpression("==", SJVariableAccess("tmp_1"), SJLiteral("true")),
                 List(SJFieldWrite(SJVariableAccess("this"), "b", SJLiteral("false")),
                      SJAssignment(SJVariableAccess("i"), SJBinaryExpression("+", SJVariableAccess("i"), SJLiteral("1"))),
                      SJFieldRead(SJVariableAccess("tmp_1"), SJVariableAccess("this"), "b")))),
                           HashMap("i" -> "int", "tmp_1" -> "boolean")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
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
           HashMap("x" -> "int")),
      SJConstructorDefinition(Set(Public()), "Fac", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Fac.txt") should equal(expected)
  }

  "Parsing Fac2.txt" should "produce the correct AST" in {
    val expected =
      List(SJClassDefinition(Set(), "Fac", "", List(), List(SJMethodDefinition(Set(Static()), "fac", "int",
       List(SJArgument("n", "int")), List(
         SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
            List(SJCall(Some(SJVariableAccess("x")), SJVariableAccess("this"), "fac",
                                  List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))),
                 SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("x")))),
            List(SJAssignment(SJVariableAccess("x"), SJLiteral("1")))),
         SJReturn(SJVariableAccess("x"))),
           HashMap("x" -> "int")),
      SJConstructorDefinition(Set(Public()), "Fac", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing assert.txt" should "produce the correct AST" in {
    val expected = List(SJClassDefinition(Set(), "Foo", "", List(), List(
      SJMethodDefinition(Set(), "foo", "void", List(), List(
        SJAssert(SJBinaryExpression("==", SJLiteral("5"), SJLiteral("5")))),
                       HashMap()),
      SJConstructorDefinition(Set(Public()), "Foo", List(), List(), HashMap())), None))
    getASTbyParsingFileNamed("assert.txt") should equal(expected)
  }

/* TODO: do it!
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
*/

  "Parsing ClassWithConstructor.txt" should "produce the correct AST" in {
    val expected = List(
      SJClassDefinition(Set(),"Person", "", Nil, List(
        SJFieldDefinition(Set(), "name", "String"), 
        SJConstructorDefinition(Set(Public()), "Person", List(SJArgument("name","String")), List(
          SJFieldWrite(SJVariableAccess("this"), "name", SJVariableAccess("name"))),
                              HashMap()), None))
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
             HashMap("a" -> "int", "tmp_1" -> "int", "tmp_2" -> "int", "tmp_3" -> "int", "tmp_4" -> "int", "tmp_5" -> "int",
                 "tmp_6" -> "int", "tmp_7" -> "int", "tmp_8" -> "int", "tmp_9" -> "int", "tmp_10" -> "int", "tmp_11" -> "int")),
        SJConstructorDefinition(Set(Public()), "Foo", List(), List(SJFieldWrite(SJVariableAccess("this"), "number", SJLiteral("0"))),
                              HashMap())), None))
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
