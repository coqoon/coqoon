package dk.itu.coqoon.core.coqtop.coqidetop

import java.io.Reader
import scala.io.Source
import scala.xml.{XML, Elem}
import scala.xml.parsing.ConstructingParser
import dk.itu.coqoon.core.CoqoonPreferences
import dk.itu.coqoon.core.coqtop.{CoqProgram, CoqProgramInstance}

private class StringBuilderSource(
    sb : StringBuilder) extends scala.io.Source {
  override val iter = sb.iterator
  /* Suppress all error reporting -- sb will *usually* be an incomplete XML
   * document */
  override def report(pos : Int, msg : String, out : java.io.PrintStream) = ()
}
object StringBuilderSource {
  def apply(sb : StringBuilder) : scala.io.Source = new StringBuilderSource(sb)
}

private class CoqOutputParser(input : StringBuilder)
    extends ConstructingParser(StringBuilderSource(input), false) {
  override def replacementText(entity : String) = entity match {
    case "nbsp" => Source.fromString("\u00a0")
    case _ => super.replacementText(entity)
  }
  nextch()
}
object CoqOutputParser {
  def parse(sb : StringBuilder) = new CoqOutputParser(sb).document
}

class CoqIdeTopImpl(args : Seq[String]) extends CoqIdeTop_v20170413 {
  import CoqIdeTopImpl._

  private var pr : Option[CoqProgramInstance] = None
  private def notifyListeners(e : Elem) = ()
  
  private def send(e : Elem) : Elem = {
    if (pr == None) {
      val ct = CoqProgram.makeProgram(
          "/home/alec/coq-instances/coq-8.8.1/usr/bin/coqtop")
      if (!ct.check) {
        throw new java.io.IOException("Couldn't find the coqtop program")
      } else if (ct.version == None) {
        throw new java.io.IOException(
            "Couldn't detect Coq version")
      }
      pr = Option(ct.run(
          (args ++ Seq("-toploop", "coqidetop", "-main-channel", "stdfds")) ++
          CoqoonPreferences.ExtraArguments.get))
      ReaderThread.start
    }
    ReaderThread.ValueLock synchronized {
      import ReaderThread.ValueLock._
      replyAwaited = true
      println("-> " + e.toString)
      pr.get.stdin.write(e.toString)
      pr.get.stdin.flush
      while (value.isEmpty)
        ReaderThread.ValueLock.wait
      try {
        value.get
      } finally {
        queuedFeedback.foreach(notifyListeners)
        value = None
        replyAwaited = false
      }
    }
  }

  private object ReaderThread extends Thread {
    object ValueLock {
      var replyAwaited = false
      var value : Option[Elem] = None
      var queuedFeedback : Seq[Elem] = Seq()
    }

    private val buf : Array[Char] = Array(32768)
    
    var running = true
    override def run() =
      while (running) {
        @scala.annotation.tailrec def _util(
            sofar_ : StringBuilder = StringBuilder.newBuilder) :
                Option[Elem] = {
          var sofar = sofar_
          val count = pr.get.stdout.read(buf)
          if (count == -1)
            return None
          sofar ++= buf.toSeq.take(count)
          if (sofar.endsWith(">")) {
            try {
              val doc = CoqOutputParser.parse(sofar)
              Some(doc.children(0).asInstanceOf[Elem])
            } catch {
              case e : scala.xml.parsing.FatalError =>
                _util(sofar)
            }
          } else _util(sofar)
        }
        val elem = _util().get
        println("<- " + elem)
        ValueLock synchronized {
          import ValueLock._
          if (!replyAwaited) {
            notifyListeners(elem)
          } else if (elem.label == "value") {
            value = Some(elem)
            ValueLock.notify
          } else {
            queuedFeedback :+= elem
          }
        }
      }
  }

  def about() = unwrapAboutResponse(send(wrapAboutCall))
  def add(stateId : Integer, command : String, v : Interface.verbose) =
    unwrapAddResponse(send(wrapAddCall(stateId, command, v)))
  def goal() = unwrapGoalResponse(send(wrapGoalCall))
  def status(force : Boolean) =
    unwrapStatusResponse(send(wrapStatusCall(force)))
}
private object CoqIdeTopImpl {
  import Interface._
  import Interface.XML._

  def wrapAboutCall() =
    <call val="About">{
    wrapUnit()}</call>
  def unwrapAboutResponse(e : Elem) =
    unwrapValue(unwrapCoqInfo)(e)

  def wrapAddCall(stateId : Integer, command : String, v : verbose) =
    <call val="Add">{
    wrapPair(
        wrapPair(wrapString, wrapInt),
        wrapPair(wrapStateId, wrapBoolean))((command, 1), (stateId, v))}</call>
  def unwrapAddResponse(e : Elem) =
    unwrapValue(unwrapPair(
        unwrapStateId, unwrapPair(
            unwrapUnion(unwrapUnit, unwrapStateId), unwrapString)))(e)

  def wrapGoalCall() =
    <call val="Goal">{
    wrapUnit()}</call>
  def unwrapGoalResponse(e : Elem) =
    unwrapValue(unwrapOption(unwrapGoals))(e)

  def wrapStatusCall(force : Boolean) =
    <call val="Status">{
    wrapBoolean(force)
    }</call>
  def unwrapStatusResponse(e : Elem) =
    unwrapValue(unwrapStatus)(e)
}

object CoqIdeTopImplTest {
  import Interface._
  import Interface.XML._

  val good1 =
    <value val="good">
      <pair>
        <state_id val="98"/>
        <pair>
          <union val="in_l"><unit/></union>
          <string>Added to the thing.</string>
        </pair>
      </pair>
    </value>
  val good2 =
    <value val="good">
      <pair>
        <state_id val="98"/>
        <pair>
          <union val="in_r"><state_id val="101"/></union>
          <string>Added to the other thing.</string>
        </pair>
      </pair>
    </value>
  val fail1 =
    <value val="fail"
        loc_s="4"
        loc_e="10">
      <state_id val="98"/>
      <richpp>This isn't good.</richpp>
    </value>
  val fail2 =
    <value val="fail">
      <state_id val="98"/>
      <richpp>That wasn't good.</richpp>
    </value>
  val document = Seq("Theorem t : True /\\ True /\\ True.", "Proof.", "split.", "trivial.", "split.", "trivial.", "split.", "trivial.", "split.", "Qed.")
  def main(args : Array[String]) : Unit = {
    val a = new CoqIdeTopImpl(Seq())
    var head = 1
    println(a.about())
    document foreach {
      case s =>
        a.add(head, s, true) match {
          case Interface.Good((newHead, (Left(()), c))) =>
            head = newHead
            a.status(true) match {
              case Interface.Good(Interface.status(_, Some(p), _, _)) =>
                println(s"Now editing proof $p")
                println(a.goal)
              case _ =>
            }
          case Interface.Fail((s, l, e)) =>
            println(e)
        }
    }
    /*println(wrapAddCall(1, "Set Printing All.", true))
    Seq(good1, good2, fail1, fail2).foreach(e => println(unwrapAddResponse(e)))*/
  }
}