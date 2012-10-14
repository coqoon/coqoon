
package dk.itu.sdg.coq

import dk.itu.sdg.javaparser.ASTSpec

import org.scalatest.{ BeforeAndAfterEach, BeforeAndAfterAll }

import java.io.{ InputStreamReader, FileInputStream, File }
import scala.util.parsing.input.{ StreamReader }

import dk.itu.sdg.kopitiam.{ CoqCallback, CoqResponse, CoqTop, CoqWarning, CoqError, CoqShellReady, CoqShellTokens, PrintActor }

object WarningOutputter extends CoqCallback {
  var readyfortest : Boolean = false
  var lasttokens : Option[CoqShellTokens] = None
  var warnings : List[String] = List()
  var errors : List[String] = List()

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(mono, CoqShellTokens(th, gl, con, loc)) =>
        lasttokens = Some(x.asInstanceOf[CoqShellReady].tokens)
        if (gl == 5 && loc == 0)
          readyfortest = true
      case CoqWarning(x) => warnings ::= x
      case CoqError(x, y, s, l) => errors ::= y
      case y => Console.println("received " + y)
    }
  }
}

trait CoqSpec extends ASTSpec with BeforeAndAfterAll with BeforeAndAfterEach {
  def runTest () : Boolean = {
    CoqTop.checkForCoqBinary
  }

  override def beforeAll (configMap : Map[String, Any]) {
    CoqTop.coqpath = "/opt/local/bin/"
    if (CoqTop.checkForCoqBinary) {
      CoqTop.init()
      val started = CoqTop.startCoq()
      assert(started == true)
      PrintActor.register(WarningOutputter)
      //register listener to fail on err
      CoqTop.writeToCoq("Add LoadPath \"/Users/hannes/tomeso/git/semantics/coq/\".")
      CoqTop.writeToCoq("Require Import Tactics.")
      CoqTop.writeToCoq("Open Scope string_scope.")
      CoqTop.writeToCoq("Open Scope list_scope.")
    }
  }

  override def afterAll (configMap : Map[String, Any]) {
    CoqTop.killCoq
  }

  override def beforeEach () {
    WarningOutputter.warnings = List()
    WarningOutputter.errors = List()
  }

  override def afterEach () {
    val btlen = WarningOutputter.lasttokens match {
      case None => 0
      case Some(CoqShellTokens(th, gl, con, loc)) => con.length
    }
    CoqTop.writeToCoq("Backtrack 5 0 " + btlen + ".")
  }

  def runCoq (classfile : String, name : String) : Option[(List[String], List[String])] = {
    //wait for coq to be ready!
    if (!runTest)
      return None
    val coqoutput = getCoqOutputFromFile(classfile, name);
    Console.println("writing to coq:\n" + coqoutput.mkString("\n"))
    for (x <- coqoutput)
      CoqTop.writeToCoq(x)
    Console.println("done!")
    //backtrack to a safe position for next test
    return Some((WarningOutputter.warnings, WarningOutputter.errors))
  }
}


