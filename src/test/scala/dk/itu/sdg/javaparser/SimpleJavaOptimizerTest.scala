package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet }
import org.scalatest.{ FlatSpec }
import org.scalatest.matchers.{ ShouldMatchers }

class ReadWriteVariablesOfStatement extends FlatSpec with ShouldMatchers {
  
    import SimpleJavaOptimizer._
  
    "SJVariableAccess" should "add variable to read set" in {
      rwOfStatement(SJVariableAccess("tmp_1")) should equal (Some(ReadsAndWrites(HashSet(Read("tmp_1")),HashSet())))
    }
    
    "SJCall" should "add variables to both the read and write sets" in {
      val readAndWrite = rwOfStatement(SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "fac",List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))))
      readAndWrite should equal (Some(ReadsAndWrites(HashSet(Read("this"), Read("n")), HashSet(Write("tmp_1")))))
    }
    
    "SJAssignment" should "add variables to possible both read and write sets" in {
      val readAndWrite = rwOfStatement(SJAssignment(SJVariableAccess("x"), 
        SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1"))))
      readAndWrite should equal (Some(ReadsAndWrites(HashSet(Read("tmp_1"), Read("n")),HashSet(Write("x")))))
    }
}
