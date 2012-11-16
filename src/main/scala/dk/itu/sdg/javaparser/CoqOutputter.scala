/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser
import java.io.File

trait CoqOutputter extends JavaToSimpleJava {
  import scala.collection.immutable.HashMap
  import scala.util.parsing.input.Position

  def getArgs (x : List[SJArgument]) : List[String] = {
    x.map(_.id)
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
        //and sm_un / sm_bin / sm_tern / sm_quad depends on arity of representation predicate
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
      case x : Specification => Console.println("interfacemethods of spec: " + x.data); None
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
    if (x == ">") "vgt"
    else if (x == "<") "vlt"
    else if (x == "<=") "vle"
    else if (x == "-") "vminus"
    else if (x == "*") "vtimes"
    else if (x == "+") "vadd"
    else if (x == "==") "veq"
    else if (x == "!") "vnot"
    else { Console.println("translateOp dunno Key " + x); "" }
  }

/*
  def exprtotype (x : SJExpression) : String = {
    x match {
      case SJBinaryExpression(op, l, r) => exprtotype(l) //assumption: typeof(x op y) == typeof(x) == typeof(y)
      case SJUnaryExpression(op, e)     => exprtotype(e) //assumption: typeof(op x) == typeof(x)
      case SJLiteral(x)                 => if (x == "null") "Object" else try { x.toInt.toString; "int" } catch { case e : Throwable => "String" }
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

  def printEVal (ex : SJExpression) : String = {
    val r = printE(ex)
    if (r(0) == '"')
      r + "/V"
    else
      r
  }

  def printEExpr (ex : SJExpression) : String = {
    val r = printE(ex)
    if (r(0) == '"')
      "(" + r + ":expr)"
    else
      r
  }

  def printE (ex : SJExpression) : String = {
    ex match {
      case SJVariableAccess(x) => "\"" + x + "\""
      case SJUnaryExpression(op, e) => "(lift1 " + translateOp(op) + " " + printEVal(e) + ")"
      case SJBinaryExpression(op, l, r) =>
        if (op == "!=")
          //XXX: hack for AMP (list reversal) 11-04-12
          "(enot (lift2 eeq_ptr_up " + printEVal(l) + " " + printEVal(r) + "))"
        else
          "(lift2 " + translateOp(op) + " " + printEVal(l) + " " + printEVal(r) + ")"
      case SJLiteral(v) =>
        if (v == "null" || v == "true" || v == "false")
          "`" + v
        else
          try { "`" + v.toInt.toString }
          catch { case e : Throwable => "\"" + e + "\"" }
    }
  }

  def argstring (as : List[SJExpression]) : String = {
    as.map(printEVal).mkString("[", "; ", "]")
  }

  def printStatement (something : SJStatement, m : SJInvokable) : Option[String] = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case SJAssert(x) =>
        something.setCoqPos(m.getLength, 0)
        Some("(cassert " + printEOpt(x) + ")")
      case SJAssignment(l, r) =>
        something.setCoqPos(m.getLength, 0)
        Some("(cassign " + printE(l) + " " + printE(r) + ")")
      case SJFieldWrite(v, fi, va) =>
        something.setCoqPos(m.getLength, 0)
        Some("(cwrite " + printE(v) + " \"" + fi + "\" " + printE(va) + ")")
      case SJFieldRead(va, v, fi) =>
        something.setCoqPos(m.getLength, 0)
        Some("(cread " + printE(va) + " " + printE(v) + " \"" + fi + "\")")
      case SJReturn(SJVariableAccess(x)) =>
        something.setCoqPos(m.getLength, 0)
        ret = "(var_expr \"" + x +"\")"; None
      case SJReturn(x : SJLiteral) =>
        something.setCoqPos(m.getLength, 0)
        ret = printE(x); None
      case SJCall(v, r, f, a) =>
        something.setCoqPos(m.getLength, 0)
        val value = v match {
          case None => "\"\""
          case Some(x) => printE(x)
        }
        val (cl, static : Boolean) = r match {
          case SJVariableAccess(x) => (m.localvariables(x), false)
          case SJLiteral(y) => (y, true)
        }
        val isstatic =
          if (static)
            true
          else {
            SJTable.getMethodInClass(cl, f) match {
              case None => log.warning("dunno, can't find " + cl + " method " + f); false
              case Some(x) => x.modifiers.contains(Static())
            }
          }
        deps = deps + ((cl, f))
        val (cw, cp) = if (isstatic) ("cscall", "\"" + cl + "\"") else ("cdcall", printE(r))
        Some("(" + cw + " " + value + " " + cp + " \"" + f + "\" (" +  argstring(a) + "))")
      case SJNewExpression(v, t, a) =>
        something.setCoqPos(m.getLength, 0)
        //Some("(cscall " + printE(v) + " \"" + t + "\" \"new\" " + argstring(a) + ")")
        Some("(calloc " + printE(v) + " \"" + t + "\")")
      case SJConditional(test, consequence, alternative) =>
        something.setCoqPos(m.getLength, 0)
        val te = printE(test)
        val tr = optPrintBody(consequence.flatMap(x => printStatement(x, m)))
        val fa = optPrintBody(alternative.flatMap(x => printStatement(x, m)))
        Some("(cif " + te + " " + tr + " " + fa + ")")
      case SJWhile(test, body) =>
        something.setCoqPos(m.getLength, 0)
        Some("(cwhile " + printE(test) + " " + optPrintBody(body.flatMap(x => printStatement(x, m))) + ")")
      case y@Loopinvariant(i, f) =>
        val con = "forward (" + i + ") (" + f + ")."
        something.setCoqPos(m.getLength, con.length)
        m.appendCoqString(con)
        None
      case x : Specification =>
        val con = x.data
        something.setCoqPos(m.getLength, con.length)
        m.appendCoqString(con)
        None
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
  private var deps : Set[Pair[String,String]] = Set[Pair[String,String]]()

  def getBody (xs : List[SJStatement], method : SJMethodDefinition) : Pair[String, Pair[String, Set[Pair[String,String]]]] = {
    val myname = method.id + "_body"
    ret = "myreturnvaluedummy"
    deps = Set[Pair[String,String]]()
    val body = xs.flatMap(x => printStatement(x, method))
    val b = printBody(body)
    val defi = "Definition " + myname + " := " + b + "."
    val res =
      if (ret == "myreturnvaluedummy")
        "`0"
      else
        ret
    (defi, (res, deps))
  }

  //(("$name", $name + M), (valid_ + $name + _ + $class, $name + _spec))
  def classMethods (c : SJClassDefinition, cd : SJClassDefinition) : List[Pair[Pair[String,String], Pair[Option[String],Option[String]]]] = {
    var precon : Option[Precondition] = None
    var postcon : Option[Postcondition] = None
    var quantif : Option[Quantification] = None
    c.body.flatMap {
      case x@SJMethodDefinition(modifiers, name, typ, params, body, lvars) =>
        //Console.println("starting to print method " + name + " with body " + body)
        val bodyref = name + "_body"
        val args = getArgs(params)
        val t =
          if (modifiers.contains(Static()))
            args
          else
            "this" :: args
        precon match {
          case Some(pre) => postcon match {
            case Some(post) => quantif match {
              case Some(quant) =>
                val quant1 = quant.data.split(",")
                val quant2 =
                  if (quant1.length == 0)
                    ""
                  else
                    quant1.mkString("[A] ", ", [A]", "")
                val spec1 = "Definition " + name + """_spec :=
  ("""
                val spec2 = spec1 + quant2 + ", " + "\"" + c.id + "\" :.: \"" + name + "\" |-> [" + printArgListSpec(t) + """]
  {{ """
                val ret =
                  if (typ == "void")
                    "\"\", "
                  else
                    ""
                val spec3 = spec2 + pre.data + " }}-{{ " + ret
                val spec = spec3 + post.data + " }})."
                post.setCoqPos(spec3.length, post.data.length)
                x.addSpec(post)
                post.method = Some(x)
                pre.setCoqPos(spec2.length, pre.data.length)
                x.addSpec(pre)
                pre.method = Some(x)
                quant.setCoqPos(spec1.length, quant2.length)
                x.addSpec(quant)
                quant.method = Some(x)
                x.setSpecOff(cd.getSpec.getOrElse("").length)
                x.setSpecLength(spec.length)
                cd.appendSpec(spec)
              case None => Console.println("no logical variables for method " + name)
            }
            case None => Console.println("pre without post for method " + name);
          }
          case None => postcon match {
            case Some(post) => Console.println("post without pre for method " + name);
            case None => Console.println("No spec for method " + name);
          }
        }
        precon = None
        postcon = None
        quantif = None
        val (bodyp, (returnvar, deps)) = getBody(body, x)
        val rde = deps.map(_._2)
        val rdep = rde.map(_ + "_spec")
        val rdeps =
          if (rdep.size == 0) ""
          else if (rdep.size == 1) "|> " + rdep.last
          else
            "|> " + rdep.mkString("(", " [/\\] ", ")")
        val suff =
          if (rde.contains(name))
            " at 2"
          else
            ""
        val proof = "Lemma valid_" + name + "_" + c.id + ": " + rdeps + " |= " + name + """_spec.
Proof.
  unfold """ + name + "_spec" + suff + "; unfold_spec."
        x.prependCoqString(proof)
        x.setCoqPos(proof.length, 0)
        x.appendCoqString("Qed.")
        cd.appendProgram(bodyp)
        cd.appendProgram("Definition " + name + "M := Build_Method (" + printArgList(t) + ") " + bodyref + " " + returnvar + ".")
        Some((("\"" + name + "\"", name + "M"), (Some("valid_" + name + "_" + c.id), Some(name + "_spec"))))
      case x@SJConstructorDefinition(modifiers, typ, params, body, lvars) =>
        val args = getArgs(params)
        val bodi = body.flatMap(y => printStatement(y, x))
        val bodip = printBody("(calloc \"this\" \"" + typ + "\")" :: bodi)
        val nam = typ + "_new"
        cd.appendProgram("Definition " + nam + " := Build_Method (" + printArgList(getArgs(params)) + ") " + bodip + " (var_expr \"this\").")
        Some((("\"new\"" , nam), (None, None)))
      case x : Precondition =>
        precon match {
          case None => precon = Some(x); None
          case Some(x) => Console.println("wrong! two preconditions for a method"); None
        }
      case x : Postcondition =>
        postcon match {
          case None => postcon = Some(x); None
          case Some(x) => Console.println("wrong! two postconditions for a method"); None
        }
      case x : Quantification =>
        quantif match {
          case None => quantif = Some(x); None
          case Some(x) => Console.println("wrong! multiple logical variables for a method specification"); None
        }
      case x : Specification =>
        x.setCoqPos(cd.getSpec.getOrElse("").length, x.data.length)
        cd.appendSpec(x.data)
        None
      case _ => None
    }
  }

  private val unique_names : String =
"""Definition unique_method_names := option_proof (search_unique_names Prog).
Opaque unique_method_names."""

  private val prelude : String = """
Require Import AbstractAsn.
Require Import Tactics.

Open Scope string_scope.
Open Scope hasn_scope.
"""

  def coqoutput (xs : List[SJDefinition], c : SJClassDefinition) : Unit = {
    var specifications : List[String] = List[String]()
    var proofs : List[String] = List[String]()
    var cs : List[String] = List[String]()
    c.appendProgram(prelude)
    c.appendSpec("""
Module """ + c.id + """_spec.
Import """ + c.id + """.
Import """ + c.id + """_model.
Module Import SC := Tac """ + c.id + """.

Open Scope spec_scope.
Open Scope asn_scope.
""")
    c.appendProgram("Module " + c.id + " <: PROGRAM.")
    xs.foreach(x => x match {
      case SJInterfaceDefinition(modifiers, id, inters, body) =>
        //Console.println("interfaces are " + inters)
/*        val (mmethods, methodspecs) = interfaceMethods(id, body)
        val mmeths = if (mmethods.length == 0) "" else mmethods.reduceLeft(_ + " " + _)
        val mspecs = if (methodspecs.length == 0) "" else methodspecs.reduceLeft(_ + "\n [/\\]\n  " + _)
        val (supermethods, interfaces) = interfaceSpec(inters)
        val superms = if (supermethods.length == 0) "" else supermethods.reduceLeft(_ + " " + _)
        val superis = if (interfaces.length == 0) "" else interfaces.reduceLeft(_ + "\n  " + _) + "\n  "
        interfs ::= "Definition " + id + " (C : class) (T : Type) (R : val -> T -> upred heap_alg) " + superms + " " + mmeths + " : spec :=\n  " + superis + mspecs + "." */
      case cd@SJClassDefinition(modifiers, id, supers, inters, body, par, fs) =>
        cs ::= id
        val fields = fs.keys.toList
        val methods = classMethods(cd, c)
        proofs ++= methods.flatMap(_._2._1)
        specifications ++= methods.flatMap(_._2._2)
        c.appendProgram("Definition " + id + " := Build_Class " + printFiniteSet(fields) + " " + printFiniteMap(methods.map(_._1)) + ".")
    })
    val classes = printFiniteMap(cs.map(x => ("\"" + x + "\"", x)))
    c.appendProgram("Definition Prog := Build_Program " + classes + ".")
    c.appendProgram(unique_names)
    c.appendProgram("End " + c.id + ".")
    c.appendSpec("Definition " + c.id + "_spec : spec := " + specifications.mkString(""" [/\] """) + ".")
    c.setClassCorrectness(Some("Lemma valid_" + c.id + " : |= " + c.id + """_spec.
Proof.
  unfold """ + c.id + """_spec.
  apply lob; repeat rewrite later_and_spec; spec_splits.
""" + proofs.map(x => { "etransitivity; [|apply " + x + "]; spec_solve."}).mkString("\n") +
"""
Qed.
"""))
  }

  def printArgList (l : List[String]) : String = {
    l.foldRight("nil")("\"" + _ + "\" :: " + _)
  }

  def printArgListSpec (l : List[String]) : String = {
    l.map("\"" + _ + "\"").mkString("; ")
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
