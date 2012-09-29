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

  def printE (ex : SJExpression) : String = {
    ex match {
      case SJVariableAccess(x) => "\"" + x + "\""
      case SJUnaryExpression(op, e) => "(" + translateOp(op) + " " + printE(e) + ")"
      case SJBinaryExpression(op, l, r) =>
        if (op == "!=")
          //XXX: hack for AMP (list reversal) 11-04-12
          "(enot (eeq_ptrs (" + printE(l) + ":expr) " + printE(r) + "))"
        else
          "(" + translateOp(op) + " " + printE(l) + " " + printE(r) + ")"
      case SJLiteral(v) =>
        if (v == "null" || v == "true" || v == "false")
          "`" + v
        else
          try { "(" + v.toInt.toString + ":expr)" }
          catch { case e : Throwable => "\"" + e + "\"" }
    }
  }

  def argstring (as : List[SJExpression]) : String = {
    as.map(printE).foldRight("nil")(_ + " :: " + _)
  }

  def printStatement (something : SJStatement, locals : HashMap[String, String]) : Option[String] = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case SJAssert(x) =>
        Some("(cassert " + printEOpt(x) + ")")
      case SJAssignment(l, r) =>
        Some("(cassign " + printE(l) + " " + printE(r) + ")")
      case SJFieldWrite(v, fi, va) =>
        Some("(cwrite " + printE(v) + " \"" + fi + "\" " + printE(va) + ")")
      case SJFieldRead(va, v, fi) =>
        Some("(cread " + printE(va) + " " + printE(v) + " \"" + fi + "\")")
      case SJReturn(SJVariableAccess(x)) =>
        ret = "(var_expr \"" + x +"\")"; None
      case SJReturn(x : SJLiteral) =>
        ret = printE(x); None
      case SJCall(v, r, f, a) =>
        val value = v match {
          case None => "\"\""
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
            SJTable.getMethodInClass(cl, f) match {
              case None => log.warning("dunno, can't find " + cl + " method " + f); false
              case Some(x) => x.modifiers.contains(Static())
            }
          }
        val (cw, cp) = if (isstatic) ("cscall", "\"" + cl + "\"") else ("cdcall", printE(r))
        Some("(" + cw + " " + value + " " + cp + " \"" + f + "\" (" +  argstring(a) + "))")
      case SJNewExpression(v, t, a) =>
        Some("(cscall " + printE(v) + " \"" + t + "\" \"new\" " + argstring(a) + ")")
      case SJConditional(test, consequence, alternative) =>
        val te = printE(test)
        val tr = optPrintBody(consequence.flatMap(x => printStatement(x, locals)))
        val fa = optPrintBody(alternative.flatMap(x => printStatement(x, locals)))
        Some("(cif " + te + " " + tr + " " + fa + ")")
      case SJWhile(test, body) =>
        Some("(cwhile " + printE(test) + " " + optPrintBody(body.flatMap(x => printStatement(x, locals))) + ")")
      case y@Loopinvariant(i, f) =>
        lengths ::= y.pos
        val con = "forward (" + i + ") (" + f + ")."
        coqlengths ::= (proofoutput.reduceLeft(_ + "\n" + _).length, con.length)
        proofoutput ::= con; None
      case x : Specification =>
        lengths ::= x.pos
        coqlengths ::= (proofoutput.reduceLeft(_ + "\n" + _).length, x.data.length)
        proofoutput ::= x.data; None
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
    val body = xs.flatMap(x => printStatement(x, locals))
    val b = printBody(body)
    val defi = "Definition " + myname + " := " + b + "."
    val res =
      if (ret == "myreturnvaluedummy")
        "`0"
      else
        ret
    (defi, res)
  }

  def classMethods (body : List[SJBodyDefinition], clazz : String) : List[Pair[Pair[String,String],Pair[Pair[String,Pair[Position,List[Position]]],Pair[Int,List[Pair[Int,Int]]]]]] = {
    var precon : Option[Precondition] = None
    var postcon : Option[Postcondition] = None
    var quantif : Option[Quantification] = None
    body.flatMap {
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
                specifications ::= name + "_spec"
                specoutput ::= "Definition " + name + """_spec :=
  (""" + quant.data + ", " + "\"" + clazz + "\" :.: \"" + name + "\" |-> [" + printArgListSpec(t) + """]
  {{ """ + pre.data + " }}-{{ " + post.data + " }})."
              case None => Console.println("no quantification for method " + name)
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
        proofs ::= "valid_" + name + "_" + clazz
        proofoutput ::= "Lemma valid_" + name + "_" + clazz + ": |= " + name + """_spec.
Proof.
  unfold """ + name + "_spec" + "; unfold_spec."
        val fst = proofoutput.reduceLeft(_ + "\n" + _).length
        val (bodyp, returnvar) = getBody(body, bodyref, lvars)
        proofoutput ::= "Qed."
        val ls = lengths.reverse
        val cl = coqlengths.reverse
        lengths = List[Position]()
        coqlengths = List[Pair[Int,Int]]()
        outp ::= bodyp
        outp ::= "Definition " + name + "M := Build_Method (" + printArgList(t) + ") " + bodyref + " " + returnvar + "."
        Some((("\"" + name + "\"", name + "M"), ((name, (x.pos, ls)), (fst, cl))))
      case x@SJConstructorDefinition(modifiers, typ, params, body, lvars) =>
        val args = getArgs(params)
        val bodi = body.flatMap(x => printStatement(x, lvars))
        val bodip = printBody("(calloc \"this\" \"" + typ + "\")" :: bodi)
        val nam = typ + "_new"
        val ls = lengths.reverse
        val cl = coqlengths.reverse
        lengths = List[Position]()
        coqlengths = List[Pair[Int,Int]]()
        outp ::= "Definition " + nam + " := Build_Method (" + printArgList(getArgs(params)) + ") " + bodip + " (var_expr \"this\")."
        Some((("\"new\"" , nam), ((nam, (x.pos, ls)), (0, cl))))
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
          case Some(x) => Console.println("wrong! multiple quantifications for a method specification"); None
        }
      case x : Specification => specoutput ::= x.data; None
      case _ => None
    }
  }

  private var outp : List[String] = null
  private var specoutput : List[String] = null
  private var proofoutput : List[String] = null
  private var specifications : List[String] = null
  private var proofs : List[String] = null
  private var lengths : List[Position] = null
  private var coqlengths : List[Pair[Int,Int]] = null

  private val unique_names : List[String] = List("Opaque unique_method_names.", "Definition unique_method_names := option_proof (search_unique_names Prog).")

  private val prelude : String = """
Require Import Tactics.

Open Scope string_scope.
Open Scope hasn_scope.
"""

  def coqoutput (xs : List[SJDefinition], complete : Boolean, name : String) : Pair[List[String], Pair[Int, List[Pair[Pair[String, Pair[Position, List[Position]]],Pair[Int,List[Pair[Int,Int]]]]]]] = {
    outp = List[String]()
    specoutput = List[String]()
    proofoutput = List[String]()
    specifications = List[String]()
    proofs = List[String]()
    lengths = List[Position]()
    coqlengths = List[Pair[Int,Int]]()
    var offs = List[Pair[Pair[String, Pair[Position,List[Position]]],Pair[Int,List[Pair[Int,Int]]]]]()
    var cs : List[String] = List[String]()
    if (complete)
      outp ::= prelude
    var interfs : List[String] = List[String]()
    if (complete)
      specoutput ::= """
Module """ + name + """_spec.
Import """ + name + """.
Import """ + name + """_model.
Module Import SC := Tac """ + name + """.

Open Scope spec_scope.
Open Scope asn_scope.
"""
    outp ::= "Module " + name + " <: PROGRAM."
    //XXX hardcoded for AMP (list reversal) 11-04-12
    outp ::= """Notation "'eeq_ptrs'" :=
  (lift2 eeq_ptr_up) (at level 50) : hasn_scope."""
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
      case SJClassDefinition(modifiers, id, supers, inters, body, par, fs) =>
        //let's hope only a single class and interfaces before that!
        cs ::= id
        val fields = fs.keys.toList
        val methods = classMethods(body, id)
        offs ++= methods.map(_._2)
        outp ::= "Definition " + id + " := Build_Class " + printFiniteSet(fields) + " " + printFiniteMap(methods.map(_._1)) + "."
    })
    val classes = printFiniteMap(cs.map(x => ("\"" + x + "\"", x)))
    outp ::= "Definition Prog := Build_Program " + classes + "."
    outp = unique_names ++ outp
    outp ::= "End " + name + "."
    specoutput ::= "Definition " + name + "_spec : spec := " + specifications.mkString(""" [/\] """) + "."
    proofoutput ::= "Lemma valid_" + name + " : |= " + name + """_spec.
Proof.
  unfold """ + name + """_spec.
""" + proofs.foldRight("\n")("  apply " + _ + ".\n" + _) + "Qed."
    if (complete)
      proofoutput ::= "End " + name + "_spec."
    val prefix = outp.reverse ++ List("") ++ specoutput.reverse
    (prefix ++ proofoutput.reverse ++ List(""), (prefix.reduceLeft(_ + "\n" + _).length + 2, offs))
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
