/* CoqTopIdeSlave.scala
 * Communicate with coqtop using the new -ideslave protocol
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam
import scala.collection.mutable.ArrayBuilder

trait CoqTopIdeSlave {
  def version : String
  
  def kill
  def restart
  def interrupt
}

object CoqTopIdeSlave {
  def forVersion(version : String) : Option[CoqTopIdeSlave] = {
    version match {
      case "20120710" => Some(new CoqTopIdeSlaveImpl())
      case _ => None
    }
  }
  
  import java.io.File
  def checkProgramPath() : Boolean = new File(getProgramPath).exists()
  
  def getProgramPath : String = getProgramPath("coqtop")
  
  def getProgramPath(program : String) : String = {
    val ac = Activator.getDefault
    getProgramPath(program,
      if (ac != null) ac.getPreferenceStore.getString("coqpath") else "")
  }
  
  def getProgramPath(program : String, dir : String) : String = {
    def isWindows =
      System.getProperty("os.name").toLowerCase.contains("windows")
    val programName = (if (isWindows) program + ".exe" else program)
    if (dir == null || dir.length == 0) {
      programName
    } else dir + File.separator + programName
  }
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
  def set_options(options : List[Pair[option_name, option_value]])
  def quit : value[Unit]
  /* ? */ def about : value[coq_info]
}

private class CoqTopIdeSlaveImpl extends CoqTopIdeSlave_v20120710 {
  import java.io.{InputStreamReader, OutputStreamWriter, Reader, Writer}
  import scala.sys.process.{Process, ProcessIO}
  
  private var in : Writer = null
  private var out : Reader = null
  
  private var pr : Process = null
  
  override def kill = {
    if (in != null)
      in.close()
    if (out != null)
      out.close()
    if (pr != null)
      pr.destroy
    pr = null
    in = null
    out = null
  }
  
  override def restart = {
    kill
    pr = Process(Seq(CoqTopIdeSlave.getProgramPath, "-ideslave")).run(
      new ProcessIO(
        a => in = new OutputStreamWriter(a),
        a => out = new InputStreamReader(a),
        _ => ()))
  }
  restart
  
  import scala.xml.{Attribute, Elem, Node, Null, Text, XML}
  private def sendRaw(n : Elem) : Elem = synchronized {
    in.write(n.toString())
    in.flush()
    println("TO   " + n.toString())
    var t = new String()
    @scala.annotation.tailrec def _util : Elem = {
      val c = out.read()
      if (c == -1)
        return null
      val ch = c.asInstanceOf[Char]
      t += ch
      if (ch == '>' && t.endsWith("</value>")) {
        XML.loadString(t)
      } else _util
    }
    val x = _util
    println("FROM " + x.toString())
    x
  }
  
  private def send[A](n : Elem, f : Elem => A) = {
    unwrapValue(sendRaw(n), f)
  }
  
  private def childElements(e : Elem) : Seq[Elem] = {
    (e \ "_").collect({case el : Elem => el})
  }
  
  import scala.xml.UnprefixedAttribute
  
  implicit def pairToAttribute(a : Pair[String, String]) : UnprefixedAttribute =
    new UnprefixedAttribute(a._1, a._2, Null)
  
  private def wrapString(a : String) : Elem = <string>{a}</string>
  private def unwrapString(e : Elem) = e.text.trim()
  
  private def wrapInt(a : Int) : Elem = <int>{a}</int>
  private def unwrapInt(e : Elem) = unwrapString(e).toInt
  
  private def wrapBoolean(a : Boolean) : Elem =
    Elem(null, "bool", ("val", a.toString), scala.xml.TopScope)
    
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
      case Some(b) => ("some", f(b))
      case None => ("none", null)
    }
    Elem(null, "option", ("val", wr._1), scala.xml.TopScope, wr._2)
  }
  
  private def unwrapOption[A](e : Elem, f : Elem => A) = {
    e.attribute("val") match {
      case Some(Seq(Text("some"))) => Some(f(childElements(e).first))
      case _ => None
    }
  }
  
  private def wrapList[A](sf : List[A], f : A => Elem) : Elem = {
    val children = sf.map(f)
    Elem(null, "list", Null, scala.xml.TopScope, children : _*)
  }
  
  private def unwrapList[A](e : Elem, f : Elem => A) = {
    childElements(e).map(f).toList
  }
  
  private def wrapPair[A, B](a : Pair[A, B], f : A => Elem, g : B => Elem) = {
    val children = List(f(a._1), g(a._2))
    Elem(null, "pair", Null, scala.xml.TopScope, children : _*)
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
    Elem(null, "option_value", ("val", wr._1), scala.xml.TopScope, wr._2)
  }
  
  private def unwrapOptionValue(a : Elem) = {
    val ch = childElements(a).first
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
  
  override def interrupt = Unit // TODO
  
  override def interp(r : CoqTypes.raw, v : CoqTypes.verbose, s : String) =
    send(
      (<call val="interp">{s}</call> %
        ("raw", if (r) "true" else null) %
        ("verbose", if (v) "true" else null)),
      unwrapString)
  
  override def rewind(steps : Int) =
    send(
      (<call val="rewind" /> % ("steps", steps.toString)),
      a => unwrapInt(childElements(a).first))

  override def goals =
    send(
      (<call val="goal" />),
      a => unwrapOption(
          childElements(a).first, unwrapGoals))
  
  override def hints =
    send(
      (<call val="hints" />),
      a => unwrapOption(
          childElements(a).first,
          unwrapPair(_,
              unwrapList(_, unwrapHint(_)),
              unwrapHint(_))))
      
  override def status =
    send(
      (<call val="status" />),
      a => unwrapStatus(childElements(a).first))

  override def inloadpath(dir : String) =
    send(
      (<call val="inloadpath">{dir}</call>),
      a => unwrapBoolean(childElements(a).first))

  override def mkcases(inductive : String) =
    send(
      (<call val="mkcases">{inductive}</call>),
      a => unwrapList(childElements(a).first, unwrapList(_, { _.text })))
      
  override def evars =
    send(
      (<call val="evars" />),
      a => unwrapOption(
          childElements(a).first,
          unwrapList(_, b => { CoqTypes.evar(b.text)})))
  
  private def wrapSearchFlags(sf : CoqTypes.search_flags) : List[Elem] =
    sf.map(_ match {
      case (c : CoqTypes.search_constraint, b) =>
        Elem(null, "pair", Null, scala.xml.TopScope, c match {
          case p : CoqTypes.Name_Pattern =>
            Elem(null, "search_constraint",
                ("val", "name_pattern"), scala.xml.TopScope,
                wrapString(p.value))
          case p : CoqTypes.Type_Pattern =>
            Elem(null, "search_constraint",
                ("val", "type_pattern"), scala.xml.TopScope,
                wrapString(p.value))
          case p : CoqTypes.SubType_Pattern =>
            Elem(null, "search_constraint",
                ("val", "subtype_pattern"), scala.xml.TopScope,
                wrapString(p.value))
          case p : CoqTypes.In_Module =>
            Elem(null, "search_constraint",
                ("val", "in_module"), scala.xml.TopScope,
                wrapList(p.value, wrapString))
          case p : CoqTypes.Include_Blacklist =>
            Elem(null, "search_constraint",
                ("val", "include_blacklist"), scala.xml.TopScope)
        }, wrapBoolean(b))
    })
  
  private def wrapSearch(sf : CoqTypes.search_flags) : Elem = {
    val children = wrapSearchFlags(sf)
    Elem(null, "call", ("val", "search"), scala.xml.TopScope, children : _*)
  }
  
  override def search(sf : CoqTypes.search_flags) =
    send(
      wrapSearch(sf),
      a => unwrapList(
          childElements(a).first,
          unwrapCoqObjectString))
  
  override def get_options =
    send(
      (<call val="getoptions" />),
      a => unwrapList(
          childElements(a).first,
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
    Elem(null, "setoptions", Null, scala.xml.TopScope, children : _*)
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
        val cif = (a \ "coq_info").first.child
        CoqTypes.coq_info(
          cif(0).text, cif(1).text, cif(2).text, cif(3).text)
      })
}
