/* CoqTopIdeSlave.scala
 * Communicate with coqtop using the new -ideslave protocol
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core.coqtop

import dk.itu.coqoon.core.{Activator, CoqoonPreferences}
import dk.itu.coqoon.core.utilities.TotalReader

trait CoqProgram {
  def path : String
  def check : Boolean = new java.io.File(path).exists
  def run(args : Seq[String],
      start : ProcessBuilder => Process = (a => a.start)) : CoqProgramInstance
}
object CoqProgram {
  private class ProgramImpl(name : String) extends CoqProgram {
    override def path : String = CoqoonPreferences.getCoqPath match {
      case Some(path) => path + java.io.File.separator + name
      case _ => name
    }
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplPOSIX(path +: args, start)
  }

  private class ProgramImplWindows(
      name : String) extends ProgramImpl(name + ".exe") {
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplWindows(path +: args, start)
  }

  def apply(name : String) : CoqProgram =
    if (PlatformUtilities.isWindows) {
      new ProgramImplWindows(name)
    } else new ProgramImpl(name)
}

trait CoqProgramInstance {
  import java.io.{Reader, Writer}

  def stdin : Writer
  def stdout : Reader
  def readAll : (Int, String) = {
    val r = TotalReader.read(stdout)
    (waitFor, r)
  }

  def kill
  def waitFor : Int
  def interrupt
}

private class CoqProgramInstanceImpl(argv : Seq[String],
    start : ProcessBuilder => Process) extends CoqProgramInstance {
  protected val (in, out, pr) = {
    import java.io.{InputStreamReader, OutputStreamWriter}

    val pb = new ProcessBuilder(argv : _*)
    val pr = start(pb)
    val in = new OutputStreamWriter(pr.getOutputStream, "UTF-8")
    val out = new InputStreamReader(pr.getInputStream, "UTF-8")
    (in, out, pr)
  }

  override def stdin = in
  override def stdout = out

  override def kill = pr.destroy
  override def waitFor = pr.waitFor
  override def interrupt = ()
}

private class CoqProgramInstanceImplWindows(
    argv : Seq[String], start : ProcessBuilder => Process)
        extends CoqProgramInstanceImpl(argv.map(a => '"' + a + '"'), start) {
  override def interrupt = if (pr != null) {
    import WindowsConsoleUtilities._
    CoqProgramInstanceImplWindows.extractHandle(
        pr).map(getProcessId).foreach(pid => {
      freeConsole
      attachConsole(pid)
      ignoreCtrlC
      sendCtrlC
      freeConsole
    })
  }
}
private object CoqProgramInstanceImplWindows {
  def extractHandle(p : Process) : Option[Long] = try {
    p.exitValue
    None
  } catch {
    case _ : IllegalThreadStateException =>
      val klass = p.getClass
      val handleField = try {
        Some(klass.getDeclaredField("handle"))
      } catch {
        case _ : NoSuchFieldException => None
      }
      handleField.map(hf => {
        hf.setAccessible(true)
        hf.getLong(p)
      })
  }
}

private class CoqProgramInstanceImplPOSIX(argv : Seq[String],
    start : ProcessBuilder => Process) extends CoqProgramInstanceImpl(
        Seq("/bin/sh", "-c", "echo $$; exec \"$@\"", "wrapper") ++ argv,
            start) {
  private var pid : String = {
    var pid = ""
    var a : Int = stdout.read
    while (a != -1 && a != '\n') {
      pid += a.asInstanceOf[Char]
      a = stdout.read
    }
    pid
  }

  import scala.sys.process.Process
  override def interrupt() = Process(Seq("kill", "-INT", pid)).run
}

private object PlatformUtilities {
  def getOSName = System.getProperty("os.name").toLowerCase
  def isWindows = getOSName.contains("windows")
}

trait CoqTopIdeSlave {
  def version : String

  def kill
  def interrupt
}

trait CoqTopIdeSlave_v20120710 extends CoqTopIdeSlave {
  import CoqTypes._

  override def version = "20120710"

  /* All of these methods perform synchronised and blocking I/O */
  def interp(raw : raw, verbose : verbose, string : String) : value[String]
  def rewind(steps : Int) : value[Int]
  def goals : value[Option[goals]]
  def hints : value[Option[Pair[List[hint], hint]]]
  def status : value[status]
  def inloadpath(dir : String) : value[Boolean]
  def mkcases(inductive : String) : value[List[List[String]]]
  def evars : value[Option[List[evar]]]
  def search(sf : search_flags) : value[List[coq_object[String]]]
  def get_options : value[List[Pair[option_name, option_state]]]
  def set_options(
      options : List[Pair[option_name, option_value]]) : value[Unit]
  def quit : value[Unit]
  /* ? */ def about : value[coq_info]

  def transaction[A](f : CoqTopIdeSlave_v20120710 => A) : value[A] = {
    val status = this.status match {
      case Good(s) => Some(s.status_statenum)
      case _ => None
    }
    try {
      CoqTypes.Good(f(transactionOverlay))
    } catch {
      case CoqFail(ep) =>
        interp(true, false, "BackTo " + status.getOrElse(1) + ".")
        CoqTypes.Fail[A](ep)
    }
  }

  def transactionOverlay() : CoqTopIdeSlave_v20120710 =
    new ExceptionalCoqTopIdeSlave_v20120710(this)
  def optionOverlay() : CoqTopIdeSlave_v20120710 =
    new OptionalCoqTopIdeSlave_v20120710(this)

  def getOption(name : option_name) : Option[option_state] =
      get_options match {
    case Good(options) => options.find(a => a._1 == name).map(_._2)
    case _ => None
  }
  def getOptionValue(name : option_name) : Option[option_value] =
    getOption(name).map(_.opt_value)
}
object CoqTopIdeSlave_v20120710 {
  def apply() : Option[CoqTopIdeSlave_v20120710] = apply(Seq.empty)

  def apply(args : Seq[String]) : Option[CoqTopIdeSlave_v20120710] = {
    val ct = new CoqTopIdeSlaveImpl(args)
    ct.about
    Some(ct.optionOverlay)
  }
}

class OptionCache(ct : CoqTopIdeSlave_v20120710) {
  import CoqTypes._

  var optionCache : Map[option_name, option_state] = Map()
  ct.get_options match {
    case Good(options) => options.foreach(a => optionCache = optionCache + a)
    case _ =>
  }

  def get(name : option_name) : Option[option_state] = optionCache.get(name)
  def getValue(name : option_name) : Option[option_value] =
    get(name).map(_.opt_value)
  def set(name : option_name, value : option_value) : Unit = {
    ct.set_options(List((name, value))) match {
      case Good(_) =>
        val os = get(name).get
        optionCache = optionCache + Pair(name, option_state(
            os.opt_sync, os.opt_depr, os.opt_name, value))
      case _ =>
    }
  }
}

import java.io.{Reader, Writer}
import scala.sys.process.Process

private class CoqTopIdeSlaveImpl(
    args : Seq[String]) extends CoqTopIdeSlave_v20120710 {
  private var pr : Option[CoqProgramInstance] = None

  override def kill = {
    pr.foreach(_.kill)
    pr = None
  }

  override def interrupt = { pr.foreach(_.interrupt) }

  private var buf = new Array[Char](32768)

  import scala.xml.{Attribute, Elem, Node, Null, Text, XML}
  private def sendRaw(n : Elem) : Elem = synchronized {
    if (pr == None) {
      val ct = CoqProgram("coqtop")
      if (!ct.check)
        throw new java.io.IOException("Couldn't find the coqtop program")
      pr = Option(ct.run(args ++ Seq("-ideslave")))
    }
    pr.get.stdin.write(n.toString())
    pr.get.stdin.flush()
    /* println("TO   " + n.toString()) */
    var t = new String()
    @scala.annotation.tailrec def _util : Elem = {
      val count = pr.get.stdout.read(buf)
      if (count == -1)
        return null
      t ++= buf.toSeq.take(count)
      if (t.endsWith("</value>")) {
        XML.loadString(t)
      } else _util
    }
    val x = _util
    /* println("FROM " + x.toString()) */
    x
  }

  private def send[A](n : Elem, f : Elem => A) = Option(sendRaw(n)) match {
    case Some(result) => unwrapValue(result, f)
    case None => CoqTypes.Fail[A]((None, "Unexpected end-of-stream"))
  }

  private def childElements(e : Elem) : Seq[Elem] = {
    (e \ "_").collect({case el : Elem => el})
  }

  import scala.xml.UnprefixedAttribute
  def attr(name : String, value : String) : UnprefixedAttribute =
    new UnprefixedAttribute(name, value, Null)

  private def wrapString(a : String) : Elem = <string>{a}</string>
  private def unwrapString(e : Elem) = e.text.trim()

  private def wrapInt(a : Int) : Elem = <int>{a}</int>
  private def unwrapInt(e : Elem) = unwrapString(e).toInt

  private def wrapBoolean(a : Boolean) : Elem =
    Elem(null, "bool", attr("val", a.toString), scala.xml.TopScope, true)

  private def unwrapBoolean(e : Elem) = e.attribute("val") match {
  	case Some(Seq(Text(a))) => a.trim.toBoolean
  	case _ => false
  }

  private def unwrapValue[A](e : Elem, f : Elem => A) : CoqTypes.value[A] = {
    e.attribute("val") match {
      case Some(Seq(Text("fail"))) => {
        CoqTypes.Fail(Pair(
            Pair(e.attribute("loc_s"), e.attribute("loc_e")) match {
              case Pair(Some(Seq(Text(a))), Some(Seq(Text(b)))) =>
                Some(a.toInt, b.toInt)
              case _ => None
            }, e.text))
      }
      case Some(Seq(Text("good"))) => CoqTypes.Good(f(e))
      case Some(Seq(Text("unsafe"))) => CoqTypes.Unsafe(f(e))
      case _ => null
    }
  }

  private def wrapOption[A](a : Option[A], f : A => Elem) : Elem = {
    val wr = a match {
      case Some(b) => ("some", Seq(f(b)))
      case None => ("none", Seq())
    }
    Elem(null, "option", attr("val", wr._1), scala.xml.TopScope, true, wr._2 : _*)
  }

  private def unwrapOption[A](e : Elem, f : Elem => A) = {
    e.attribute("val") match {
      case Some(Seq(Text("some"))) => Some(f(childElements(e).head))
      case _ => None
    }
  }

  private def wrapList[A](sf : List[A], f : A => Elem) : Elem = {
    val children = sf.map(f)
    Elem(null, "list", Null, scala.xml.TopScope, true, children : _*)
  }

  private def unwrapList[A](e : Elem, f : Elem => A) = {
    childElements(e).map(f).toList
  }

  private def wrapPair[A, B](a : Pair[A, B], f : A => Elem, g : B => Elem) = {
    val children = List(f(a._1), g(a._2))
    Elem(null, "pair", Null, scala.xml.TopScope, true, children : _*)
  }

  private def unwrapPair[A, B](e : Elem, f : Elem => A, g : Elem => B) = {
    val ce = childElements(e)
    Pair(f(ce(0)), g(ce(1)))
  }

  private def unwrapHint(a : Elem) = {
    CoqTypes.hint(
        unwrapList(a,
            unwrapPair(_, unwrapString, unwrapString)))
  }

  private def unwrapStatus(a : Elem) = {
    val ce = childElements(a)
    CoqTypes.status(
        unwrapList(ce(0), unwrapString),
        unwrapOption(ce(1), unwrapString),
        unwrapList(ce(2), unwrapString),
        unwrapInt(ce(3)), unwrapInt(ce(4)))
  }

  private def unwrapGoal(a : Elem) = {
    val ce = childElements(a)
    CoqTypes.goal(
        unwrapString(ce(0)),
        unwrapList(ce(1), unwrapString),
        unwrapString(ce(2)))
  }

  private def unwrapGoals(a : Elem) = {
    val ce = childElements(a)
    CoqTypes.goals(
        unwrapList(ce(0), unwrapGoal),
        unwrapList(ce(1), unwrapPair(_,
            unwrapList(_, unwrapGoal),
            unwrapList(_, unwrapGoal))))
  }

  private def unwrapCoqObjectString(a : Elem) = {
    val ce = childElements(a)
    CoqTypes.coq_object[String](
        unwrapList(ce(0), unwrapString),
        unwrapList(ce(1), unwrapString),
        unwrapString(ce(2)))
  }

  private def wrapOptionName(a : List[String]) = wrapList(a, wrapString)

  private def unwrapOptionName(a : Elem) = unwrapList(a, unwrapString)

  private def unwrapOptionState(a : Elem) = {
    val ce = childElements(a)
    CoqTypes.option_state(
        unwrapBoolean(ce(0)),
        unwrapBoolean(ce(1)),
        unwrapString(ce(2)),
        unwrapOptionValue(ce(3)))
  }

  private def wrapOptionValue(st : CoqTypes.option_value) : Elem = {
    val wr = st match {
      case b : CoqTypes.BoolValue =>
        ("boolvalue", wrapBoolean(b.value))
      case i : CoqTypes.IntValue =>
        ("intvalue", wrapOption(i.value, wrapInt))
      case s : CoqTypes.StringValue =>
        ("stringvalue", wrapString(s.value))
    }
    Elem(null, "option_value",
        attr("val", wr._1), scala.xml.TopScope, true, wr._2)
  }

  private def unwrapOptionValue(a : Elem) = {
    val ch = childElements(a).head
    a.attribute("val") match {
      case Some(Seq(Text("intvalue"))) =>
        CoqTypes.IntValue(unwrapOption(ch, unwrapInt))
      case Some(Seq(Text("boolvalue"))) =>
        CoqTypes.BoolValue(unwrapBoolean(ch))
      case Some(Seq(Text("stringvalue"))) =>
        CoqTypes.StringValue(unwrapString(ch))
      case _ => null
    }
  }

  override def interp(r : CoqTypes.raw, v : CoqTypes.verbose, s : String) =
    send(
      (<call val="interp" id="0">{s}</call> %
        attr("raw", if (r) "true" else null) %
        attr("verbose", if (v) "true" else null)),
      unwrapString)

  override def rewind(steps : Int) =
    send(
      (<call val="rewind" /> % attr("steps", steps.toString)),
      a => unwrapInt(childElements(a).head))

  override def goals =
    send(
      (<call val="goal" />),
      a => unwrapOption(
          childElements(a).head, unwrapGoals))

  override def hints =
    send(
      (<call val="hints" />),
      a => unwrapOption(
          childElements(a).head,
          unwrapPair(_,
              unwrapList(_, unwrapHint(_)),
              unwrapHint(_))))

  override def status =
    send(
      (<call val="status" />),
      a => unwrapStatus(childElements(a).head))

  override def inloadpath(dir : String) =
    send(
      (<call val="inloadpath">{dir}</call>),
      a => unwrapBoolean(childElements(a).head))

  override def mkcases(inductive : String) =
    send(
      (<call val="mkcases">{inductive}</call>),
      a => unwrapList(childElements(a).head, unwrapList(_, { _.text })))

  override def evars =
    send(
      (<call val="evars" />),
      a => unwrapOption(
          childElements(a).head,
          unwrapList(_, b => { CoqTypes.evar(b.text)})))

  private def wrapSearchFlags(sf : CoqTypes.search_flags) : List[Elem] =
    sf.map(_ match {
      case (c : CoqTypes.search_constraint, b) =>
        Elem(null, "pair", Null, scala.xml.TopScope, true, c match {
          case p : CoqTypes.Name_Pattern =>
            Elem(null, "search_constraint",
                attr("val", "name_pattern"), scala.xml.TopScope, true,
                wrapString(p.value))
          case p : CoqTypes.Type_Pattern =>
            Elem(null, "search_constraint",
                attr("val", "type_pattern"), scala.xml.TopScope, true,
                wrapString(p.value))
          case p : CoqTypes.SubType_Pattern =>
            Elem(null, "search_constraint",
                attr("val", "subtype_pattern"), scala.xml.TopScope, true,
                wrapString(p.value))
          case p : CoqTypes.In_Module =>
            Elem(null, "search_constraint",
                attr("val", "in_module"), scala.xml.TopScope, true,
                wrapList(p.value, wrapString))
          case p : CoqTypes.Include_Blacklist =>
            Elem(null, "search_constraint",
                attr("val", "include_blacklist"), scala.xml.TopScope, true)
        }, wrapBoolean(b))
    })

  private def wrapSearch(sf : CoqTypes.search_flags) : Elem = {
    val children = wrapSearchFlags(sf)
    Elem(null, "call",
        attr("val", "search"), scala.xml.TopScope, true, children : _*)
  }

  override def search(sf : CoqTypes.search_flags) =
    send(
      wrapSearch(sf),
      a => unwrapList(
          childElements(a).head,
          unwrapCoqObjectString))

  override def get_options =
    send(
      (<call val="getoptions" />),
      a => unwrapList(
          childElements(a).head,
          unwrapPair(_,
              unwrapOptionName,
              unwrapOptionState)))

  private def wrapOptions(
      options : List[Pair[CoqTypes.option_name, CoqTypes.option_value]]) :
          List[Elem] = {
    options.map(wrapPair(_, wrapOptionName, wrapOptionValue))
  }

  private def wrapSetOptions(
      options : List[Pair[CoqTypes.option_name, CoqTypes.option_value]]) :
          Elem = {
    val children = wrapOptions(options)
    Elem(null, "call",
        attr("val", "setoptions"), scala.xml.TopScope, true, children : _*)
  }

  override def set_options(
      options : List[Pair[CoqTypes.option_name, CoqTypes.option_value]]) = {
    send(
      wrapSetOptions(options),
      _ => ())
  }

  override def quit =
    send(
      (<call val="quit" />),
      _ => ())

  override def about =
    send(
      (<call val="about" />),
      a => {
        val cif = (a \ "coq_info").head.child
        CoqTypes.coq_info(
          cif(0).text, cif(1).text, cif(2).text, cif(3).text)
      })
}

private class ExceptionalCoqTopIdeSlave_v20120710(
    base : CoqTopIdeSlave_v20120710) extends CoqTopIdeSlave_v20120710 {
  import CoqTypes._

  private def check[A](result : value[A]) : value[A] = result match {
    case Good(a) => result
    case Unsafe(a) => result
    case Fail(ep) => throw new CoqFail(ep)
  }

  override def kill = base.kill
  override def interrupt = base.interrupt

  override def interp(raw : raw, verbose : verbose, string : String) =
    check(base.interp(raw, verbose, string))
  override def rewind(steps : Int) = check(base.rewind(steps))
  override def goals = check(base.goals)
  override def hints = check(base.hints)
  override def status = check(base.status)
  override def inloadpath(dir : String) = check(base.inloadpath(dir))
  override def mkcases(inductive : String) = check(base.mkcases(inductive))
  override def evars = check(base.evars)
  override def search(sf : search_flags) = check(base.search(sf))
  override def get_options = check(base.get_options)
  override def set_options(options : List[Pair[option_name, option_value]]) =
    check(base.set_options(options))
  override def quit = check(base.quit)
  override def about = check(base.about)
}
private case class CoqFail(
    ep : Pair[CoqTypes.location, String]) extends Exception

private class OptionalCoqTopIdeSlave_v20120710(
    base : CoqTopIdeSlave_v20120710) extends CoqTopIdeSlave_v20120710 {
  import CoqTypes._

  var options : Map[option_name, option_value] = Map()

  private def check[A](result : value[A]) : value[A] = {
    Option(options.toList).filterNot(_.isEmpty).foreach(base.set_options(_))
    optionCache = None
    result
  }

  override def kill = base.kill
  override def interrupt = base.interrupt

  override def interp(raw : raw, verbose : verbose, string : String) =
    check(base.interp(raw, verbose, string))
  override def rewind(steps : Int) = check(base.rewind(steps))
  override def goals = base.goals
  override def hints = base.hints
  override def status = base.status
  override def inloadpath(dir : String) = base.inloadpath(dir)
  override def mkcases(inductive : String) = base.mkcases(inductive)
  override def evars = base.evars
  override def search(sf : search_flags) = base.search(sf)

  var optionCache :
      Option[CoqTypes.value[List[(option_name, option_state)]]] = None

  override def get_options = optionCache match {
    case Some(a) => a
    case None =>
      val v = base.get_options
      optionCache = Some(v)
      v
  }
  override def set_options(options : List[(option_name, option_value)]) =
      base.set_options(options) match {
    case a : Good[Unit] =>
      optionCache = None
      this.options ++= options; a
    case a => a
  }
  override def quit = base.quit
  override def about = base.about
}
