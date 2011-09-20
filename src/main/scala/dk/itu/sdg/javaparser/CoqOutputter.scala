/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

trait CoqOutputter extends JavaToSimpleJava {
  import scala.collection.immutable.HashMap

  def getArgs (x : List[SJArgument]) : List[String] = {
    x.flatMap {
      case SJArgument(id, jtype) => Some(id)
      case y => log.warning("in getArgs, unexpected " + y); None
    }
  }

  def interfaceMethods (interface : String, body : List[SJBodyDefinition]) : (List[String], List[String]) = {
    //input: (int get(), void set (int v))
    //output: ("get : T -> val", "set : T -> val -> T"),
    //        ([A]t:T, C:.:"get" |-> (params) {{ pre }}-{{ post }}, [A]t:T, C:.:"set" |-> (params) {{ pre }}-{{ post }})
    // where pre and post are lifted get/set thingies, together with magic representation
    // or more concise: pre is sm_bin R ("this":expr) (sm_const t)
    // post is (void): "", sm_bin R ("this":expr) (sm_bin name t args)
    // post is (non-void): "r", sm_bin R (this) (sm_bin name t args) </\> r = name t
    var ms : List[String] = List[String]()
    val results = body.flatMap {
      case SJMethodDefinition(modifiers, name, typ, params, body, locals) =>
        val pre = "sm_bin R (\"this\":expr) (sm_const t)"
        var paras = getArgs(params)
        val ps = if (paras.length == 0) "" else paras.map("(\"" + _ + "\":expr)").reduceLeft(_ + _)
        val postm = "sm_bin R (\"this\":expr) (sm_bin " + name + " (sm_const t) " + ps + ")"
        //XXX: fixme! only valid if no side effects
        val postns = "sm_bin R (\"this\":expr) (sm_const t)"
        val post =
          if (typ == "void")
            "\"\", " + postm
          else
            //XXX: also only true for no side effects _and_ single argument for mathematical fun
            "\"ret\", " + postns + " </\\> (\"ret\":expr) == (" + name + " t:expr)"
        //XXX: fixme! only no T if no side effects
        val rettype = if (typ == "void") "T" else "val"
        val margs =
          if (params.length == 0) ""
          else "-> " + params.map(_ => "val").reduceLeft(_ + " -> " + _)
        ms ::= name
        Some(("(" + name + " : T " + margs + " -> " + rettype + ")",
            "([A]t : T, C :.: \"" + name + "\" |-> (" +  printArgList("this" :: paras) + ") {{ " + pre + " }}-{{ " + post + " }})"))
      case _ => None
    }
    //thus we need to save _1 in ClassTable, don't we?
    //val funs = results.map(_._1)
    //ClassTable.registerInterfaceFunctions(interface, ms, funs)
    (results.map(_._1), results.map(_._2))
  }

  def interfaceSpec (supers : List[String]) : (List[String], List[String]) = {
    //input: List("ICell")
    //output: (List("get : T -> val", "set : T -> val -> T"), List("ICell C T _ get set")
    //val results = supers.map({ x =>
    //  val (ps, pas) = ClassTable.interfaceFunctions(x)
    //  (ps, x + " C T R " + pas.reduceLeft(_ + " " + _))
    //})
    //if (results.length == 0)
      (List[String](), List[String]())
    //else
    //  (results.map(_._1).reduceLeft(_ ++ _), results.map(_._2))
  }

  //TODO: that's wrong, since operations depend on argument types...
  def translateOp (x : String) : String = {
    if (x == ">") "egt"
    else if (x == "<") "elt"
    else if (x == "-") "eminus"
    else if (x == "*") "etimes"
    else if (x == "+") "eplus"
    else if (x == "==") "eeq"
    else if (x == "!=") "eneq"
    else if (x == "!") "enot"
    else if (x == "&&") "eand"
    else { Console.println("translateOp dunno Key " + x); "" }
  }

/*
  def exprtotype (x : SJExpression) : String = {
    x match {
      case SJBinaryExpression(op, l, r) => exprtotype(l) //assumption: typeof(x op y) == typeof(x) == typeof(y)
      case SJUnaryExpression(op, e)     => exprtotype(e) //assumption: typeof(op x) == typeof(x)
      case SJLiteral(x)                 => if (x == "null") "Object" else try { x.toInt.toString; "int" } catch { case e => "String" }
      case SJVariableAccess(v)          => ClassTable.getLocalVar(cname, mname, v)
      case x                            => Console.println("didn't expect to need to convert this expr to a type: " + x); "Object"
    }
  }
*/

  def printEOpt (ex : SJExpression) : String = {
    ex match {
      case (x : SJBinaryExpression) => printE(ex)
      //XXX: actually only if ex is a boolean expression...
      case y => "(eeqbool " + printE(ex) + " true)"
    }
  }

  def printE (ex : SJExpression) : String = {
    ex match {
      case SJVariableAccess(x) => "(var_expr \"" + x + "\")"
      case SJUnaryExpression(op, e) => translateOp(op) + " " + printE(e)
      case SJBinaryExpression(op, l, r) => translateOp(op) + " " + printE(l) + " " + printE(r)
      case SJLiteral(v) =>
        if (v == "null" || v == "true" || v == "false")
          v
        else
          try { "(" + v.toInt.toString + ":expr)" }
          catch { case e => "\"" + e + "\"" }
    }
  }

  def argstring (as : List[SJExpression]) : String = {
    as.map(printE).foldRight("nil")(_ + " :: " + _)
  }

  def printStatement (something : SJStatement, locals : HashMap[String, String]) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case SJAssert(x) =>
        "(cassert " + printEOpt(x) + ")"
      case SJAssignment(l, r) =>
        "(cassign " + printE(l) + " " + printE(r) + ")"
      case SJFieldWrite(v, fi, va) =>
        "(cwrite " + printE(v) + " " + fi + " " + printE(va) + ")"
      case SJFieldRead(va, v, fi) =>
        "(cread " + printE(va) + " " + printE(v) + " " + fi + " )"
      case SJReturn(SJVariableAccess(x)) =>
        ret = "var_expr \"" + x + "\""; ""
      case SJReturn(y : SJLiteral) =>
        ret = printE(y); ""
      case SJCall(v, r, f, a) =>
        val value = v match {
          case None => ""
          case Some(x) => printE(x)
        }
        val (cl, static : Boolean) = r match {
          case SJVariableAccess(x) => (locals(x), false)
          case SJLiteral(y) => (y, true)
        }
        val isstatic =
          if (static)
            true
          else {
            SJTable.findMethodInClass(cl, f) match {
              case None => log.warning("dunno, can't find " + cl + " method " + f); false
              case Some(x) => x.modifiers.contains(Static())
            }
          }
        val (cw, cp) = if (isstatic) ("cscall", cl) else ("cdcall", printE(r))
        "(" + cw + " " + value + " " + cp + " \"" + f + "\" " +  argstring(a) + ")"
      case SJNewExpression(v, t, a) =>
        "(cscall " + printE(v) + " " + t + " " + argstring(a) + ")"
      case SJConditional(test, consequence, alternative) =>
        val te = printStatement(test, locals)
        val tr = optPrintBody(consequence.map(x => printStatement(x, locals)))
        val fa = optPrintBody(alternative.map(x => printStatement(x, locals)))
        "(cif " + te + " " + tr + " " + fa + ")"
      case SJWhile(test, body) =>
        "(cwhile " + printStatement(test, locals) + " " + optPrintBody(body.map(x => printStatement(x, locals))) + ")"
    }
  }

  def optPrintBody (b : List[String]) : String = {
    b match {
      case Nil => "(cskip)"
      case x => printBody(x)
    }
  }

  def printBody (bs : List[String]) : String = {
    bs match {
      case Nil => ""
      case x :: Nil => x
      case x :: y => "(cseq " + x + " " + printBody(y) + ")"
    }
  }

  private var ret : String = "myreturnvaluedummy"

  def getBody (xs : List[SJStatement], myname : String, locals : HashMap[String, String]) : (String, String) = {
    ret = "myreturnvaluedummy"
    val body = xs.map(x => printStatement(x, locals))
    val b = printBody(body)
    val defi = "\nDefinition " + myname + " := " + b + "."
    val res =
      if (ret == "myreturnvaluedummy")
        "0"
      else
        ret
    (defi, res)
  }

  def classMethods (body : List[SJBodyDefinition]) : List[Pair[String,String]] = {
    body.flatMap {
      case SJMethodDefinition(modifiers, name, typ, params, body, lvars) =>
        //Console.println("starting to print method " + name + " with body " + body)
        val bodyref = name + "_body"
        val args = getArgs(params)
        val (bodyp, returnvar) = getBody(body, bodyref, lvars)
        outp ::= bodyp
        val t =
          if (modifiers.contains(Static()))
            args
          else
            "this" :: args
        outp ::= "\nDefinition " + name + "M := Build_Method (" + printArgList(t) + ") " + bodyref + " (" + returnvar + ")."
        Some(("\"" + name + "\"", name + "M"))
      case SJConstructorDefinition(modifiers, typ, params, body, lvars) =>
        val args = getArgs(params)
        val bodi = body.map(x => printStatement(x, lvars))
        val bodip = printBody("(calloc \"this\" \"" + typ + "\")" :: bodi)
        outp ::= "Definition " + typ + " := Build_Method (" + printArgList(getArgs(params)) + ") " + bodip + " (var_expr \"this\")."
        Some(("\"" + typ + "\"" , typ))
      case _ => None
    }
  }

  private var outp : List[String] = null

  private val unique_names : String = """
Lemma unique_method_names :
      forall C m mrec,
      method_lookup Prog C m mrec ->
      NoDup (m_params mrec).
Proof.
  search (search_unique_names Prog).
Qed."""

  private val prelude : String = """
Require Import Tactics.
Require Import LiftOp.
Require Import Frame.

Open Scope string_scope.
Open Scope list_scope.
"""


  def coqoutput (xs : List[SJDefinition], spec : Boolean, name : String) : List[String] = {
    outp = List[String]()
    var cs : List[String] = List[String]()
    if (spec)
      outp ::= prelude
    var interfs : List[String] = List[String]()
    xs.foreach(x => x match {
      case SJInterfaceDefinition(modifiers, id, inters, body) =>
        //Console.println("interfaces are " + inters)
        val (mmethods, methodspecs) = interfaceMethods(id, body)
        val mmeths = if (mmethods.length == 0) "" else mmethods.reduceLeft(_ + " " + _)
        val mspecs = if (methodspecs.length == 0) "" else methodspecs.reduceLeft(_ + "\n [/\\]\n  " + _)
        val (supermethods, interfaces) = interfaceSpec(inters)
        val superms = if (supermethods.length == 0) "" else supermethods.reduceLeft(_ + " " + _)
        val superis = if (interfaces.length == 0) "" else interfaces.reduceLeft(_ + "\n  " + _) + "\n  "
        interfs ::= "Definition " + id + " (C : class) (T : Type) (R : val -> T -> upred heap_alg) " + superms + " " + mmeths + " : spec :=\n  " + superis + mspecs + "."
      case SJClassDefinition(modifiers, "Coq", supers, inters, body, par, fs) =>
      case SJClassDefinition(modifiers, id, supers, inters, body, par, fs) =>
        //let's hope only a single class and interfaces before that!
        cs ::= id
        outp ::= "Module " + name + " <: PROGRAM."
        val fields = fs.keys.toList
        val methods = classMethods(body)
        outp ::= """
Definition """ + id + """ :=
  Build_Class """ + printFiniteSet(fields) + """
              """ + printFiniteMap(methods) + "."
    })
    val classes = printFiniteMap(cs.map(x => ("\"" + x + "\"", x)))
    outp ::= "\nDefinition Prog := Build_Program " + classes + "."
    if (spec)
      outp ::= unique_names
    outp ::= "End " + name + "."
    if (spec) {
      outp ::= "\nImport " + name + "."
      outp ::= "\nSection " + name + "_spec."
    }
/* if (spec) {
      outp ::= "End " + name + "."
      outp ::= "\nImport " + name + "."
      outp = ClassTable.getCoq(myclass, "BEFORESPEC") ++ List("\n") ++ outp
      outp = interfs ++ outp
      outp ::= "\nSection " + name + "_spec."
    }
    //method specs go here
    xs.foreach(x => x match {
      case SJClassDefinition(modifiers, "Coq", supers, inters, body, par, fs) =>
      case SJClassDefinition(modifiers, id, supers, inters, body, par, fs) =>
        myclass = id
        val specs = ClassTable.getSpecs(myclass)
        //filter out empty specs which are provided by an interface
        val inter = inters.map(ClassTable.interfaceFunctions(_)).map(_._1)
        val interf = if (inter.length == 0) List[String]() else inter.reduceLeft(_ ++ _)
        var ems : List[String] = List[String]()
        specs.keys.foreach(x => {
          if ((specs(x)._1 == specs(x)._2) && (specs(x)._1 == null) && interf.contains(x))
            ems ::= x
          else {
            if (spec) {
              outp ::= "Definition " + x + "_pre : hasn :=\n  (" + specs(x)._1 + ")%asn."
              outp ::= "Definition " + x + "_post : hasn := \n  (" + specs(x)._2 + ")%asn."
            }
          }
        })
        val spcs = specs.keys.flatMap(x => {
          if (!ems.contains(x))
            Some("\"" + myclass + "\" :.: \"" + x + "\" |->  (" + printArgList(ClassTable.getArguments(myclass, x).keys.toList) + ") {{ " + x + "_pre }}-{{ \"ret\", " + x + "_post }}")
          else //copy them down from ems!
            None
        })
        val spstr = if (spcs.toList.length == 0) "" else spcs.reduceLeft(_ + "\n    [/\\]\n  " + _)
        val is = inters.map(x => x + " \"" + myclass + "\"" + " val VC (\\v, v) (\\_,\\v,v)") //XXX real instantiation
        val ins =
          if (is.length == 0) ""
          else
            "[E] VC : val -> val -> upred heap_alg, " + is.reduceLeft(_ + " [/\\] " + _)
        val sps = if (ins.length > 0 && spstr.length > 0) " [/\\] " + spstr else ""
        if (spec)
          outp ::= "Definition " + myclass + "_spec := " + ins + sps + "."
      case _ => Console.println("specs for " + x)
    })
    if (spec) {
      outp = ClassTable.getCoq(myclass, "AFTERSPEC") ++ outp
      outp ::= "End " + name + "_spec."
      outp = ClassTable.getCoq("TOP") ++ outp
    } */
    outp ::= "" //we need a newline...
    outp.reverse
  }

  def printArgList (l : List[String]) : String = {
    l.foldRight("nil")("\"" + _ + "\" :: " + _)
  }

  def printFiniteSet (l : List[String]) : String = {
    l.foldRight("(SS.empty)")("(SS.add \"" + _ + "\" " + _ + ")")
  }

  def printFiniteMap (map : List[Pair[String, String]]) : String = {
    map match {
      case Nil => "(SM.empty _)"
      case (k,v) :: b => "(SM.add " + k + " " + v + " " + printFiniteMap(b) + ")"
    }
  }

}
