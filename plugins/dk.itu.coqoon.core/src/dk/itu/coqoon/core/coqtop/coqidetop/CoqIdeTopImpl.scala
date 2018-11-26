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
      if (_DEBUG_PRINT)
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
        if (_DEBUG_PRINT)
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

  override def about() = unwrapAboutResponse(send(wrapAboutCall))
  override def add(
      stateId : Integer, command : CharSequence, v : Interface.verbose) =
    unwrapAddResponse(send(wrapAddCall(stateId, command, v)))
  def annotate(annotation : String) =
    unwrapAnnotateResponse(send(wrapAnnotateCall(annotation)))
  override def editAt(stateId : Integer) =
    unwrapEditAtResponse(send(wrapEditAtCall(stateId)))
  override def evars() = unwrapEvarsResponse(send(wrapEvarsCall))
  override def getOptions() =
    unwrapGetOptionsResponse(send(wrapGetOptionsCall))
  override def goal() = unwrapGoalResponse(send(wrapGoalCall))
  override def hints() = unwrapHintsResponse(send(wrapHintsCall))
  override def init(scriptPath : Option[String]) =
    unwrapInitResponse(send(wrapInitCall(scriptPath)))
  override def mkCases(s : String) =
    unwrapMkCasesResponse(send(wrapMkCasesCall(s)))
  def printAst(stateId : Integer) =
    unwrapPrintAstResponse(send(wrapPrintAstCall(stateId)))
  override def query(routeId : Integer, query : String, stateId : Integer) =
    unwrapQueryResponse(send(wrapQueryCall(routeId, query, stateId)))
  /* override def quit() = unwrapQuitResponse(send(wrapQuitCall)) */
  override def search(
      constraints : Seq[(Interface.search_constraint, Boolean)]) =
    unwrapSearchResponse(send(wrapSearchCall(constraints)))
  override def setOptions(
      options : Seq[(Seq[String], Interface.option_value)]) =
    unwrapSetOptionsResponse(send(wrapSetOptionsCall(options)))
  override def status(force : Boolean) =
    unwrapStatusResponse(send(wrapStatusCall(force)))
  override def stopWorker(worker : String) =
    unwrapStopWorkerResponse(send(wrapStopWorkerCall(worker)))
}
private object CoqIdeTopImpl {
  private final val _DEBUG_PRINT = false

  import Interface._
  import Interface.XML._

  def wrapAboutCall() =
    <call val="About">{
    wrapUnit()}</call>
  def unwrapAboutResponse(e : Elem) =
    unwrapValue(unwrapCoqInfo)(e)

  def wrapAddCall(stateId : Integer, command : CharSequence, v : verbose) =
    <call val="Add">{
    wrapPair(
        wrapPair(wrapString, wrapInt),
        wrapPair(wrapStateId, wrapBoolean))((command, 1), (stateId, v))}</call>
  def unwrapAddResponse(e : Elem) =
    unwrapValue(unwrapPair(
        unwrapStateId, unwrapPair(
            unwrapUnion(unwrapUnit, unwrapStateId), unwrapString)))(e)

  def wrapAnnotateCall(annotation : String) =
    <call val="Annotate">{
    wrapString(annotation)
    }</call>
  def unwrapAnnotateResponse(e : Elem) =
    unwrapValue(_unwrapRaw)(e)

  def wrapEditAtCall(stateId : Integer) =
    <call val="EditAt">{
    wrapStateId(stateId)}</call>
  def unwrapEditAtResponse(e : Elem) =
    unwrapValue(unwrapUnion(
        unwrapUnit, unwrapPair(unwrapStateId, unwrapPair(
            unwrapStateId, unwrapStateId))))(e)

  def wrapEvarsCall() =
    <call val="Evars">{
    wrapUnit()}</call>
  def unwrapEvarsResponse(e : Elem) =
    unwrapValue(unwrapOption(unwrapList(unwrapString)))(e)

  def wrapGetOptionsCall() =
    <call val="GetOptions">{
    wrapUnit()}</call>
  def unwrapGetOptionsResponse(e : Elem) =
    unwrapValue(unwrapList(unwrapPair(
        unwrapList(unwrapString), unwrapOptionState)))(e)

  def wrapGoalCall() =
    <call val="Goal">{
    wrapUnit()}</call>
  def unwrapGoalResponse(e : Elem) =
    unwrapValue(unwrapOption(unwrapGoals))(e)

  def wrapHintsCall() =
    <call val="Hints">{
    wrapUnit()}</call>
  def unwrapHintsResponse(e : Elem) =
    unwrapValue(unwrapOption(unwrapPair(
        unwrapList(unwrapHint), unwrapHint)))(e)

  def wrapInitCall(scriptPath : Option[String]) =
    <call val="Init">{
    wrapOption(wrapString)(scriptPath)
    }</call>
  def unwrapInitResponse(e : Elem) =
    unwrapValue(unwrapStateId)(e)

  def wrapMkCasesCall(s : String) =
    <call val="MkCases">{
    wrapString(s)
    }</call>
  def unwrapMkCasesResponse(e : Elem) =
    unwrapValue(unwrapList(unwrapList(unwrapString)))(e)

  def wrapPrintAstCall(sid : state_id) =
    <call val="PrintAst">{
    wrapStateId(sid)
    }</call>
  def unwrapPrintAstResponse(e : Elem) =
    unwrapValue(_unwrapRaw)(e)

  def wrapQueryCall(rid : Int, query : String, sid : Int) =
    <call val="Query">{
    wrapPair(wrapRouteId, wrapPair(wrapString, wrapStateId))(
        rid, (query, sid))}</call>
  def unwrapQueryResponse(e : Elem) =
    unwrapValue(unwrapString)(e)

  def wrapQuitCall() =
    <call val="Quit">{
    wrapUnit()}</call>
  def unwrapQuitResponse(e : Elem) =
    unwrapValue(unwrapUnit)(e)

  def wrapSearchCall(constraints : Seq[(search_constraint, Boolean)]) =
    <call val="Search">{
    wrapList(wrapPair(wrapSearchConstraint, wrapBoolean))(constraints)}</call>
  def unwrapSearchResponse(e : Elem) =
    unwrapValue(unwrapList(unwrapCoqObject(unwrapString)))(e)

  def wrapSetOptionsCall(options : Seq[(Seq[String], option_value)]) =
    <call val="SetOptions">{
    wrapList(wrapPair(wrapList(wrapString), wrapOptionValue))(options)}</call>
  def unwrapSetOptionsResponse(e : Elem) =
    unwrapValue(unwrapUnit)(e)

  def wrapStatusCall(force : Boolean) =
    <call val="Status">{
    wrapBoolean(force)
    }</call>
  def unwrapStatusResponse(e : Elem) =
    unwrapValue(unwrapStatus)(e)

  def wrapStopWorkerCall(worker : String) =
    <call val="StopWorker">{
    wrapString(worker)
    }</call>
  def unwrapStopWorkerResponse(e : Elem) =
    unwrapValue(unwrapUnit)(e)
}

object CoqIdeTopImplTest {
  import Interface._
  import Interface.XML._

  val document = Seq("Theorem t : True /\\ True /\\ True.", "Proof.", "split.", "trivial.", "split.", "trivial.", "split.", "trivial.", "split.", "Qed.")
  def main(args : Array[String]) : Unit = {
    val a = new CoqIdeTopImpl(Seq())
    var head = 1
    a.init(None)
    println(a.about())
    println(a.getOptions)
    a.setOptions(Seq((Seq("Printing", "All"), BoolValue(true))))
    print(a.search(Seq((Interface.Name_Pattern("^I$"), true))))
    document foreach {
      case s =>
        a.add(head, s, true) match {
          case Interface.Good((newHead, (Left(()), c))) =>
            println(a.printAst(head))
            println(a.annotate(s))
            head = newHead
            println(a.goal)
            a.status(true) match {
              case Interface.Good(Interface.status(_, Some(p), _, _)) =>
                println(s"Now editing proof $p")
                println(a.goal)
              case _ =>
            }
            println(a.query(1, "Check I.", newHead))
            println(a.evars())
            println(a.hints())
          case Interface.Fail((s, l, e)) =>
            println(e)
        }
    }
    /*println(wrapAddCall(1, "Set Printing All.", true))
    Seq(good1, good2, fail1, fail2).foreach(e => println(unwrapAddResponse(e)))*/
  }
}