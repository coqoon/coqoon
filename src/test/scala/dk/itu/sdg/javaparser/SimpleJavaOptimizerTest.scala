package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
import org.scalatest.Ignore
import SimpleJavaOptimizer._

class ReadWriteVariablesOfStatement extends FlatSpec with ShouldMatchers {

    "SJVariableAccess" should "add variable to read set" in {
      rwOfStatement(SJVariableAccess("tmp_1")) should equal (Some(ReadsAndWrites(HashSet("tmp_1"),HashSet())))
    }

    "SJCall" should "add variables to both the read and write sets" in {
      val readAndWrite = rwOfStatement(SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "fac",List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))))
      readAndWrite should equal (Some(ReadsAndWrites(HashSet("this", "n"), HashSet("tmp_1"))))
    }

    "SJAssignment" should "add variables to possible both read and write sets" in {
      val readAndWrite = rwOfStatement(SJAssignment(SJVariableAccess("x"),
        SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1"))))
      readAndWrite should equal (Some(ReadsAndWrites(HashSet("tmp_1", "n"),HashSet("x"))))
    }
}

class RemoveDeadVariables extends FlatSpec with ShouldMatchers {

  "Removing dead variables" should "remove obviouslyDead in the example" in  {
    val before = SJMethodDefinition(Set(Static()), "m","int",Nil,List(
      SJAssignment(SJVariableAccess("obviouslyDead"),SJLiteral("42")),
      SJReturn(SJLiteral("42"))
    ),HashMap("obviouslyDead" -> "int"))

    val after = SJMethodDefinition(Set(Static()), "m","int",Nil,List(
      SJReturn(SJLiteral("42"))
    ),HashMap[String,String]())

    liveVariableRewrite(before) should equal (after)
  }

  it should "not remove a dead variable in a while loop if it's read in the condition" in {
    val before = SJMethodDefinition(Set(Static()), "sum", "int", Nil, List(
      SJAssignment(SJVariableAccess("keepGoing"),SJLiteral("true")),
      SJAssignment(SJVariableAccess("sum"),SJLiteral("0")),
      SJWhile(SJBinaryExpression("==",SJVariableAccess("keepGoing"),SJLiteral("true")), List(
        SJAssignment(SJVariableAccess("sum"),SJBinaryExpression("+",SJVariableAccess("sum"),SJLiteral("1"))),
        SJConditional(SJBinaryExpression(">",SJVariableAccess("sum"),SJLiteral("42")),List(
          SJAssignment(SJVariableAccess("keepGoing"),SJLiteral("false"))
        ), Nil)
      )),
      SJReturn(SJVariableAccess("sum"))
    ), HashMap("keepGoing" -> "boolean", "sum" -> "int"))

    val after = before

    liveVariableRewrite(before) should equal (after)
  }

  it should """not remove a dead variable if it's using SJNewExpression in the
               assignment of the variable as it might have side-effects""" in {

    val before = SJMethodDefinition(Set(Static()), "m","int",Nil,List(
                   SJNewExpression(SJVariableAccess("obviouslyDead"),"Number",List(SJLiteral("42"))),
                   SJReturn(SJLiteral("42"))
                 ),HashMap("obviouslyDead" -> "int"))

    val after = before

    liveVariableRewrite(before) should equal (after)

  }

  it should """not remove a dead variable if it's using SJCall in the
               assignment of the variable as it might have side-effects""" in {

    val before = SJMethodDefinition(Set(Static()), "m","int",Nil,List(
                   SJCall(Some(SJVariableAccess("obviouslyDead")),SJVariableAccess("obj"),"randomInt",Nil),
                   SJReturn(SJLiteral("42"))
                 ),HashMap("obviouslyDead" -> "int"))

    val after = before

    liveVariableRewrite(before) should equal (after)

  }

  "Replacing dead variables" should "replace tmp_1 with x in fac" in {
    val before = SJMethodDefinition(Set(Static()), "fac", "int",
      List(SJArgument("n", "int")), List(
        SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
          List(
            SJCall(
              Some(SJVariableAccess("tmp_1")),
              SJVariableAccess("this"),
              "fac",
              List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))
            ),
            SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1")))
          ),
          List(
            SJAssignment(SJVariableAccess("x"), SJLiteral("1"))
          )
        ),
        SJReturn(SJVariableAccess("x"))),
      HashMap("x" -> "int", "tmp_1" -> "int", "n" -> "int", "this" -> "Fac"))

    val after = SJMethodDefinition(Set(Static()), "fac", "int",
        List(SJArgument("n", "int")), List(
          SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
            List(
              SJCall(
                Some(SJVariableAccess("x")),
                SJVariableAccess("this"),
                "fac",
                List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))
              ),
              SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("x")))
            ),
            List(
              SJAssignment(SJVariableAccess("x"), SJLiteral("1"))
            )
          ),
          SJReturn(SJVariableAccess("x"))),
        HashMap("x" -> "int", "n" -> "int", "this" -> "Fac"))

    liveVariableRewrite(before) should equal (after)
  }

  it should "should not replace dead variable if it's used in more than one assignment inside the block" in {
    val before = SJMethodDefinition(Set(Static()), "fac", "int",
      List(SJArgument("n", "int")), List(
        SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("42")),
        SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
          List(
            SJAssignment(SJVariableAccess("tmp_1"),SJBinaryExpression("-",SJVariableAccess("tmp_1"),SJLiteral("1"))),
            SJCall(
              Some(SJVariableAccess("tmp_1")),
              SJVariableAccess("this"),
              "fac",
              List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))
            ),
            SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1")))
          ),
          List(
            SJAssignment(SJVariableAccess("x"), SJLiteral("1"))
          )
        ),
        SJReturn(SJVariableAccess("x"))),
      HashMap("x" -> "int", "tmp_1" -> "int", "n" -> "int", "this" -> "Fac"))

    val after = before

    liveVariableRewrite(before) should equal (after)
  }

  it should "only replace a dead variable if it's of the same type." in {

    val before = SJMethodDefinition(Set(Static()), "test", "string", Nil,
      List(
        SJAssignment(SJVariableAccess("tmp_1"),SJLiteral("42")),
        SJAssignment(SJVariableAccess("x"),SJBinaryExpression("+", SJVariableAccess("test"),SJVariableAccess("tmp_1"))),
        SJReturn(SJVariableAccess("x"))
      ),
      HashMap("tmp_1" -> "int", "x" -> "string"))

    val after = before

    liveVariableRewrite(before) should equal (after)

  }

}

class OptimizeVariables extends FlatSpec with ShouldMatchers {

  /*
    This fails. It's not able to optimize the code yet.
  */
  "Parsing Conditional3.txt" should "produce the correct AST" in {
    val before = SJMethodDefinition(Set(),"bar","void",List(SJArgument("a","int")),
        List(
          SJFieldRead(SJVariableAccess("tmp_2"),SJVariableAccess("this"),"c"),
          SJConditional(SJBinaryExpression("==",SJVariableAccess("a"),SJVariableAccess("tmp_2")),
              List(SJAssignment(SJVariableAccess("tmp_1"),SJLiteral("20"))),
              List(SJAssignment(SJVariableAccess("tmp_1"),SJLiteral("30")))),
          SJFieldWrite(SJVariableAccess("this"),"b",SJVariableAccess("tmp_1"))),
        HashMap("this" -> "Foo", "a" -> "int", "tmp_2" -> "int", "tmp_1" -> "Object"))

    val after = SJMethodDefinition(Set(),"bar","void",List(SJArgument("a","int")),
        List(
          SJFieldRead(SJVariableAccess("tmp_1"),SJVariableAccess("this"),"c"),
          SJConditional(SJBinaryExpression("==",SJVariableAccess("a"),SJVariableAccess("tmp_1")),
              List(SJFieldWrite(SJVariableAccess("this"),"b",SJLiteral("20"))),
              List(SJFieldWrite(SJVariableAccess("this"),"b",SJLiteral("30"))))),
        HashMap("this" -> "Foo", "a" -> "int", "tmp_1" -> "int"))

    liveVariableRewrite(before) should equal (after)
  }

  /*
    This fails. It shows that the dead variable rewrite currently isn't mature enough to deal with
    multiple occurences of a variable even though they could be optimized
  */
  "replacing multiple variables" should "replace a variable used at more places" in {
    val before = SJMethodDefinition(Set(Static()), "test", "int",
       List(SJArgument("n", "int")),
       List(
         SJAssignment(SJVariableAccess("tmp_1"), SJLiteral("42")),
         SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
                       List(SJAssignment(SJVariableAccess("tmp_1"), SJBinaryExpression("+", SJVariableAccess("tmp_1"), SJLiteral("1")))),
                       List(SJAssignment(SJVariableAccess("tmp_1"), SJBinaryExpression("-", SJVariableAccess("tmp_1"), SJLiteral("1"))))),
         SJAssignment(SJVariableAccess("x"), SJVariableAccess("tmp_1")),
         SJReturn(SJVariableAccess("x"))),
       HashMap("tmp_1" -> "int", "x" -> "int"))

    val after = SJMethodDefinition(Set(Static()), "test", "int",
       List(SJArgument("n", "int")),
       List(
         SJAssignment(SJVariableAccess("x"), SJLiteral("42")),
         SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
                       List(SJAssignment(SJVariableAccess("x"), SJBinaryExpression("+", SJVariableAccess("x"), SJLiteral("1")))),
                       List(SJAssignment(SJVariableAccess("x"), SJBinaryExpression("-", SJVariableAccess("x"), SJLiteral("1"))))),
         SJReturn(SJVariableAccess("x"))),
       HashMap("x" -> "int"))

    liveVariableRewrite(before) should equal (after)
  }
}
