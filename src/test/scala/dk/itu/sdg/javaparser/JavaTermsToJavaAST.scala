/*
  This tests the conversion from JavaTerms to JavaAST. 

  @author Mads Hartmann Jensen
*/

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JavaTermsToJavaASTSPec extends ASTSpec {
  
  "Parsing Postfix1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JMethodDefinition(Set(),"bar","void",Nil,List(
        JBlock(None,List(
          JBinding("a","int",Some(JLiteral("0"))), 
          JPostfixExpression("++",JVariableAccess("a"))))))),None))    
    getJavaASTbyParsingFileNamed("Postfix1.txt") should equal(expected)
  }
  
  "Parsing Postfix2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(
        JBinding("a","int",Some(JLiteral("0"))), JPostfixExpression("--",JVariableAccess("a"))))))),None))
    getJavaASTbyParsingFileNamed("Postfix2.txt") should equal(expected)
  }
  
  "Parsing Postfix3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(), "Foo", "", Nil, List(
      JFieldDefinition(Set(),"a","int",None), 
      JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(
        JPostfixExpression("++",JFieldAccess(JVariableAccess("this"),"a"))))))),None))
    getJavaASTbyParsingFileNamed("Postfix3.txt") should equal(expected)
  }
  
  "Parsing Conditional1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(), "Foo", "", Nil, List(
      JMethodDefinition(Set(),"bar","void", List(JArgument("a","int")), List(JBlock(None, List(
        JBinding("b", "int", Some(JConditional(JBinaryExpression("==",JVariableAccess("a"),JLiteral("10")), 
                                               JBlock(None,List(JLiteral("20"))), 
                                               JBlock(None,List(JLiteral("30"))))))))))),None))
    getJavaASTbyParsingFileNamed("Conditional1.txt") should equal(expected)
  }

  "Parsing Conditional2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(),"c","int",None), 
      JMethodDefinition(Set(),"bar","void",List(JArgument("a","int")),List(JBlock(None,List(
        JBinding("b","int",Some(JConditional(JBinaryExpression("==",JVariableAccess("a"),JFieldAccess(JVariableAccess("this"),"c")),JBlock(None,List(JLiteral("20"))),JBlock(None,List(JLiteral("30"))))))))))),None))
    getJavaASTbyParsingFileNamed("Conditional2.txt") should equal(expected)
  }

  "Parsing Conditional3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(),"c","int",None),
      JFieldDefinition(Set(),"b","int",None),
      JMethodDefinition(Set(),"bar","void",List(JArgument("a","int")),List(JBlock(None,List(
        JFieldWrite(JVariableAccess("this"),"b",
          JConditional(JBinaryExpression("==",JVariableAccess("a"),JFieldAccess(JVariableAccess("this"),"c")),
          JBlock(None,List(JLiteral("20"))),
          JBlock(None,List(JLiteral("30")))))))))),None))
    getJavaASTbyParsingFileNamed("Conditional3.txt") should equal(expected)
  }

  "Parsing NestedField1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(),"a","int",None)),None), 
      JClassDefinition(Set(),"Bar","",Nil,List(
        JFieldDefinition(Set(),"f","Foo",None), 
        JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(
          JBinding("b","int",Some(JFieldAccess(JFieldAccess(JVariableAccess("this"),"f"),"a")))))))),None))
    getJavaASTbyParsingFileNamed("NestedField1.txt") should equal(expected)
  }
  
  "Parsing NestedField2.txt" should "produce the correct AST" in { 
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"f","Foo",None), JFieldDefinition(Set(),"a","int",None), JMethodDefinition(Set(),"bar","int",Nil,List(JBlock(None,List(JReturn(JFieldAccess(JFieldAccess(JFieldAccess(JFieldAccess(JFieldAccess(JFieldAccess(JVariableAccess("this"),"f"),"f"),"f"),"f"),"f"),"a"))))))),None))
    getJavaASTbyParsingFileNamed("NestedField2.txt") should equal(expected)
  }

  "Parsing NestedCall1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,Nil)))),None), JClassDefinition(Set(),"Bar","",Nil,List(JFieldDefinition(Set(),"f","Foo",None), JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JCall(JFieldAccess(JVariableAccess("this"),"f"),"bar",Nil)))))),None))
    getJavaASTbyParsingFileNamed("NestedCall1.txt") should equal(expected)
  }

  "Parsing NestedCall2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"f","Foo",None), JFieldDefinition(Set(),"a","int",None), JMethodDefinition(Set(),"get","Foo",Nil,List(JBlock(None,List(JReturn(JFieldAccess(JVariableAccess("this"),"f")))))), JMethodDefinition(Set(),"bar","int",Nil,List(JBlock(None,List(JReturn(JFieldAccess(JFieldAccess(JFieldAccess(JFieldAccess(JCall(JCall(JFieldAccess(JFieldAccess(JCall(JCall(JFieldAccess(JVariableAccess("this"),"f"),"get",Nil),"get",Nil),"f"),"f"),"get",Nil),"get",Nil),"f"),"f"),"f"),"a"))))))),None))
    getJavaASTbyParsingFileNamed("NestedCall2.txt") should equal(expected)
  }

  "Parsing While1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("i","int",Some(JLiteral("1"))), JWhile(JBinaryExpression(">",JVariableAccess("i"),JLiteral("0")),JBlock(None,List(JPostfixExpression("++",JVariableAccess("i")))))))))),None))
    getJavaASTbyParsingFileNamed("While1.txt") should equal(expected)
  }

  "Parsing While2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(), "b", "boolean", None),
      JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("i","int",Some(JLiteral("1"))), JFieldWrite(JVariableAccess("this"), "b", JLiteral("true")), JWhile(JFieldAccess(JVariableAccess("this"), "b"),JBlock(None,List(JFieldWrite(JVariableAccess("this"), "b", JLiteral("false")), JPostfixExpression("++",JVariableAccess("i")))))))))),None))
    getJavaASTbyParsingFileNamed("While2.txt") should equal(expected)
  }

  "Parsing Fac.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Fac","",Nil,List(JMethodDefinition(Set(Static()),"fac","int",List(JArgument("n","int")),List(JBlock(None,List(JBinding("x","int",None), JConditional(JBinaryExpression(">=",JVariableAccess("n"),JLiteral("0")),JBlock(None,List(JAssignment("x",JCall(JVariableAccess("this"),"fac",List(JBinaryExpression("-",JVariableAccess("n"),JLiteral("1"))))), JAssignment("x",JBinaryExpression("*",JVariableAccess("n"),JVariableAccess("x"))))),JBlock(None,List(JAssignment("x",JLiteral("1"))))), JReturn(JVariableAccess("x"))))))),None))
    getJavaASTbyParsingFileNamed("Fac.txt") should equal(expected)
  }

  "Parsing Fac2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Fac","",Nil,List(
      JMethodDefinition(Set(Static()),"fac","int",List(JArgument("n","int")),List(JBlock(None,List(
        JBinding("x","int",None),
        JConditional(JBinaryExpression(">=",JVariableAccess("n"),JLiteral("0")),
          JBlock(None,List(JAssignment("x",JBinaryExpression("*",JVariableAccess("n"),JCall(JVariableAccess("this"),"fac",List(JBinaryExpression("-",JVariableAccess("n"),JLiteral("1")))))))),
          JBlock(None,List(JAssignment("x",JLiteral("1"))))),
        JReturn(JVariableAccess("x"))))))),None))
    getJavaASTbyParsingFileNamed("Fac2.txt") should equal(expected)
  }

  "Parsing assert.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JAssert(JBinaryExpression("==",JLiteral("5"),JLiteral("5")))))))),None))
    getJavaASTbyParsingFileNamed("assert.txt") should equal(expected)
  }

  "Parsing Tree_client.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(),"ITreeIterator",Nil,List(
      JMethodDefinition(Set(),"hasNext","boolean",Nil,Nil),
      JMethodDefinition(Set(),"next","int",Nil,Nil))),
      JInterfaceDefinition(Set(),"ITree",Nil,List(
        JMethodDefinition(Set(),"contains","boolean",List(JArgument("item","int")),Nil),
        JMethodDefinition(Set(),"add","boolean",List(JArgument("item","int")),Nil),
        JMethodDefinition(Set(),"snapshot","ITree",Nil,Nil),
        JMethodDefinition(Set(),"iterator","ITreeIterator",Nil,Nil))),
        JClassDefinition(Set(),"A1B1Tree","",List("ITree"),Nil,None),
        JClassDefinition(Set(),"World","",Nil,List(
          JMethodDefinition(Set(),"main","void",Nil,List(JBlock(None,List(
            JBinding("t","ITree",Some(JNewExpression("A1B1Tree",Nil))),
            JCall(JVariableAccess("t"),"add",List(JLiteral("1"))),
            JCall(JVariableAccess("t"),"add",List(JLiteral("2"))),
            JCall(JVariableAccess("t"),"add",List(JLiteral("3"))),
            JBinding("s","ITree",Some(JCall(JVariableAccess("t"),"snapshot",Nil))),
            JBinding("it","TreeIterator",Some(JCall(JVariableAccess("s"),"iterator",Nil))),
            JBinding("lc","boolean",Some(JCall(JVariableAccess("it"),"hasNext",Nil))),
            JWhile(JVariableAccess("lc"),JBlock(None,List(
              JBinding("x","int",Some(JCall(JVariableAccess("it"),"next",Nil))),
              JCall(JVariableAccess("t"),"add",List(JBinaryExpression("*",JVariableAccess("x"),JLiteral("3")))),
              JAssignment("lc",JCall(JVariableAccess("it"),"hasNext",Nil))))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("1"))),JLiteral("true"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("2"))),JLiteral("true"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("3"))),JLiteral("true"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("4"))),JLiteral("false"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("5"))),JLiteral("false"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("6"))),JLiteral("true"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("7"))),JLiteral("false"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("8"))),JLiteral("false"))),
            JAssert(JBinaryExpression("==",JCall(JVariableAccess("t"),"contains",List(JLiteral("9"))),JLiteral("true")))))))),None))
    getJavaASTbyParsingFileNamed("Tree_client.txt") should equal(expected)
  }

  "Parsing ClassWithConstructor.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(), "Person", "", Nil, List(
      JFieldDefinition(Set(), "name", "String", None),
      JConstructorDefinition(Set(Public()), "Person", List(JArgument("name","String")), List(JBlock(None,List(
        JFieldWrite(JVariableAccess("this"), "name", JVariableAccess("name"))))))), None))
     getJavaASTbyParsingFileNamed("ClassWithConstructor.txt") should equal(expected)
   }

  "Parsing OperatorTranslation.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(),"number","int",Some(JLiteral("0"))),
      JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(
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
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("+",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("-",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("*",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("/",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("%",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("&",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("^",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("|",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression("<<",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression(">>",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1"))),
        JFieldWrite(JVariableAccess("this"),"number",JBinaryExpression(">>>",JFieldAccess(JVariableAccess("this"),"number"),JLiteral("1")))))))),None))
    getJavaASTbyParsingFileNamed("OperatorTranslation.txt") should equal(expected)
  }

}

class JavaTermsToJavaASTBindingsSpec extends ASTSpec {
  "Parsing Binding1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("20")))))))),None))
    getJavaASTbyParsingFileNamed("Binding1.txt") should equal(expected)
  }

  "Parsing Binding2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("20"))), JBinding("b","int",Some(JLiteral("10")))))))),None))
    getJavaASTbyParsingFileNamed("Binding2.txt") should equal(expected)
  }

  "Parsing Binding3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","int",Nil,List(JBlock(None,List(JReturn(JLiteral("10")))))), JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JCall(JVariableAccess("this"),"bar",Nil)))))))),None))
    getJavaASTbyParsingFileNamed("Binding3.txt") should equal(expected)
  }

  "Parsing Binding4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"a","int",None), JMethodDefinition(Set(),"bar","int",Nil,List(JBlock(None,List(JReturn(JBinaryExpression("+",JFieldAccess(JVariableAccess("this"),"a"),JFieldAccess(JVariableAccess("this"),"a"))))))), JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JBinding("b","int",Some(JFieldAccess(JVariableAccess("this"),"a"))), JBinding("c","int",Some(JBinaryExpression("+",JFieldAccess(JVariableAccess("this"),"a"),JCall(JVariableAccess("this"),"bar",Nil))))))))),None))
    getJavaASTbyParsingFileNamed("Binding4.txt") should equal(expected)
  }

  "Parsing FieldDeclaration1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(
      JFieldDefinition(Set(),"a","int",None),
      JFieldDefinition(Set(),"b","int",None)),None))
    getJavaASTbyParsingFileNamed("FieldDeclaration1.txt") should equal(expected)
  }
}

class JavaTermsToJavaASTAssignmentsSpec extends ASTSpec {
  "Parsing FieldAssignment4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Point","",List(),List(
      JConstructorDefinition(Set(),"Point",
        List(JArgument("x","float"), JArgument("y","float")),
        List(JBlock(None,List(JFieldWrite(JVariableAccess("this"),"x",JVariableAccess("x")), JFieldWrite(JVariableAccess("this"),"y",JVariableAccess("y")))))),
      JFieldDefinition(Set(),"x","float",None), JFieldDefinition(Set(),"y","float",None),
      JMethodDefinition(Set(),"flip","void",List(),List(
        JBlock(None,List(
          JBinding("t","float",Some(JFieldAccess(JVariableAccess("this"),"x"))),
          JFieldWrite(JVariableAccess("this"),"x",JFieldAccess(JVariableAccess("this"),"y")),
          JFieldWrite(JVariableAccess("this"),"y",JVariableAccess("t"))))))),None))
    getJavaASTbyParsingFileNamed("FieldAssignment4.txt") should equal(expected)
  }

  "Parsing FieldAssignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"b","int",None), JMethodDefinition(Set(),"a","int",Nil,List(JBlock(None,List(JReturn(JLiteral("10")))))), JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,List(JFieldWrite(JVariableAccess("this"),"b",JCall(JVariableAccess("this"),"a",Nil))))))),None))
    getJavaASTbyParsingFileNamed("FieldAssignment3.txt") should equal(expected)
  }

  "Parsing FieldAssignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"b","int",None), JMethodDefinition(Set(),"set","void",Nil,List(JBlock(None,List(JFieldWrite(JVariableAccess("this"),"b",JBinaryExpression("+",JFieldAccess(JVariableAccess("this"),"b"),JLiteral("10")))))))),None))
    getJavaASTbyParsingFileNamed("FieldAssignment2.txt") should equal(expected)
  }

  "Parsing FieldAssignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"b","int",None), JMethodDefinition(Set(),"set","void",Nil,List(JBlock(None,List(JFieldWrite(JVariableAccess("this"),"b",JLiteral("10"))))))),None))
    getJavaASTbyParsingFileNamed("FieldAssignment1.txt") should equal(expected)
  }

  "Parsing Assignment1.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("10"))), JAssignment("a",JLiteral("20"))))))),None))
    getJavaASTbyParsingFileNamed("Assignment1.txt") should equal(expected)
  }

  "Parsing Assignment2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("10"))), JAssignment("a",JBinaryExpression("+",JVariableAccess("a"),JLiteral("20")))))))),None))
    getJavaASTbyParsingFileNamed("Assignment2.txt") should equal(expected)
  }

  "Parsing Assignment3.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("10"))), JAssignment("a",JBinaryExpression("+",JVariableAccess("a"),JLiteral("20")))))))),None))
    getJavaASTbyParsingFileNamed("Assignment3.txt") should equal(expected)
  }

  "Parsing Assignment4.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("10"))), JAssignment("a",JCall(JVariableAccess("this"),"foobar",Nil)))))), JMethodDefinition(Set(),"foobar","int",Nil,List(JBlock(None,List(JReturn(JLiteral("10"))))))),None))
    getJavaASTbyParsingFileNamed("Assignment4.txt") should equal(expected)
  }

  "Parsing Assignment5.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,List(JBlock(None,List(JBinding("a","int",Some(JLiteral("10"))), JAssignment("a",JBinaryExpression("+",JVariableAccess("a"),JCall(JVariableAccess("this"),"foobar",Nil))))))), JMethodDefinition(Set(),"foobar","int",Nil,List(JBlock(None,List(JReturn(JLiteral("10"))))))),None))
    getJavaASTbyParsingFileNamed("Assignment5.txt") should equal(expected)
  }
}

class JavaTermsToJavaASTInterfacesSpec extends ASTSpec {
  "Parsing Interface0.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(),"Foo",Nil,Nil))
    getJavaASTbyParsingFileNamed("Interface0.txt") should equal(expected)
  }

  "Parsing Interface1.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(),"Foo",Nil,List(JMethodDefinition(Set(),"bar","int",Nil,Nil))))
    getJavaASTbyParsingFileNamed("Interface1.txt") should equal(expected)
  }

  "Parsing Interface2.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(),"Foo",Nil,List(JMethodDefinition(Set(),"bar","void",Nil,Nil))))
    getJavaASTbyParsingFileNamed("Interface2.txt") should equal(expected)
  }
}

class JavaTermsToJavaASTClassesSpec extends ASTSpec {
  "Parsing SimpleClass.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,Nil,None))
    getJavaASTbyParsingFileNamed("SimpleClass.txt") should equal(expected)
  }

  "Parsing SimpleClassWithMethod.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"foo","void",Nil,List(JBlock(None,Nil)))),None))
    getJavaASTbyParsingFileNamed("SimpleClassWithMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"foo","int",List(JArgument("a","int")),List(JBlock(None,List(JReturn(JVariableAccess("a"))))))),None))
    getJavaASTbyParsingFileNamed("SimpleClassMoreComplexMethod.txt") should equal(expected)
  }

  "Parsing SimpleClassMoreComplexMethod2.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(),"foo","int",List(JArgument("a","int")),List(JBlock(None,List(JReturn(JCall(JVariableAccess("this"),"foo",List(JCall(JVariableAccess("this"),"foo",List(JVariableAccess("a"))))))))))),None))
    getJavaASTbyParsingFileNamed("SimpleClassMoreComplexMethod2.txt") should equal(expected)
  }

  "Parsing SimpleClassWithSimpleField.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"foo","int",None)),None))
    getJavaASTbyParsingFileNamed("SimpleClassWithSimpleField.txt") should equal(expected)
  }

  "Parsing MethodWithNoFieldAccessOrCallInAnExpression.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(),"f","int",None), JMethodDefinition(Set(),"a","int",Nil,List(JBlock(None,List(JReturn(JLiteral("10")))))), JMethodDefinition(Set(),"b","int",List(JArgument("c","int")),List(JBlock(None,List(JReturn(JBinaryExpression("+",JBinaryExpression("+",JFieldAccess(JVariableAccess("this"),"f"),JCall(JVariableAccess("this"),"a",Nil)),JVariableAccess("c")))))))),None))
    getJavaASTbyParsingFileNamed("MethodWithNoFieldAccessOrCallInAnExpression.txt") should equal(expected)
  }
}

class JavaTermsToJavaASTInitializersSpec extends ASTSpec {
  "Parsing fieldWithInitializer.txt" should "produce the correct AST" in {
     val expected = List(JClassDefinition(Set(),"A","",Nil,List(JFieldDefinition(Set(),"i","int",Some(JLiteral("0"))), JConstructorDefinition(Set(Public()),"A",Nil,List(JBlock(None,Nil)))),None))
     getJavaASTbyParsingFileNamed("fieldWithInitializer.txt") should equal(expected)
   }

  "Parsing fieldWithInitializerNoPreviousConstructor.txt" should "produce the correct AST" in {
     val expected = List(JClassDefinition(Set(),"A","",Nil,List(JFieldDefinition(Set(),"i","int",Some(JLiteral("0")))),None))
     getJavaASTbyParsingFileNamed("fieldWithInitializerNoPreviousConstructor.txt") should equal(expected)
   }
}

class JavaTermsToJavaASTModifiersSpec extends ASTSpec {

  "Parsing ClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(Private(), Abstract()),"A","",Nil,Nil,None))
    getJavaASTbyParsingFileNamed("ClassModifiers.txt") should equal(expected)
  }

  "Parsing InterfaceModifiers.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(Private()),"A",Nil,Nil))
    getJavaASTbyParsingFileNamed("InterfaceModifiers.txt") should equal(expected)
  }

  "Parsing MethodModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JMethodDefinition(Set(Private()),"bar","void",Nil,List(JBlock(None,Nil)))),None))
    getJavaASTbyParsingFileNamed("MethodModifiers.txt") should equal(expected)
  }

  "Parsing FieldModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"Foo","",Nil,List(JFieldDefinition(Set(Private()),"a","int",None)),None))
    getJavaASTbyParsingFileNamed("FieldModifiers.txt") should equal(expected)
  }

  "Parsing ConstructorModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"A","",Nil,List(JConstructorDefinition(Set(Private()),"A",Nil,List(JBlock(None,Nil)))),None))
    getJavaASTbyParsingFileNamed("ConstructorModifiers.txt") should equal(expected)
  }

  "Parsing NestedClassModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(Public()),"A","",Nil,List(JClassDefinition(Set(Private()),"B","",Nil,Nil,Some("A"))),None))
    getJavaASTbyParsingFileNamed("NestedClassModifiers.txt") should equal(expected)
  }

  "Parsing BlockModifiers.txt" should "produce the correct AST" in {
    val expected = List(JClassDefinition(Set(),"A","",Nil,List(JBlock(Some(Static()),Nil)),None))
    getJavaASTbyParsingFileNamed("BlockModifiers.txt") should equal(expected)
  }

  "Parsing NestedInterfaceModifiers.txt" should "produce the correct AST" in {
    val expected = List(JInterfaceDefinition(Set(Public()),"A",Nil,List(JInterfaceDefinition(Set(Private()),"B",Nil,Nil))))
    getJavaASTbyParsingFileNamed("NestedInterfaceModifiers.txt") should equal(expected)
  }
}
