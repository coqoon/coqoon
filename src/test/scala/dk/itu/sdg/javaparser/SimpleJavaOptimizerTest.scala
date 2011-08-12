package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet, HashMap }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }
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
  
  "Removing dead variables in fac" should "replace tmp_1 with x" in {
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
        HashMap("x" -> "int", "tmp_1" -> "int", "n" -> "int", "this" -> "Fac"))
        
      liveVariableRewrite(before) should equal (after)
  }
  
  
  
}