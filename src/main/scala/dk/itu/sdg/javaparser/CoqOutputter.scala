/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

trait CoqOutputter extends JavaToSimpleJava {
  private var myclass : String = ""
  private var mymethod : String = ""

  def getArgs (x : List[InnerStatement]) : List[String] = {
    x.flatMap {
      case JArgument(id, jtype) =>
        Some(id)
      case (x : JBinaryExpression) =>
        Console.println("in getArgs, didn't expect binary expression (but printing anyways): " + x)
        Some(printStatement(x))
      case y => Console.println("in getArgs, unexpected " + y); None
    }
  }

  def interfaceMethods (interface : String, body : List[JStatement]) : (List[String], List[String]) = {
    //input: (int get(), void set (int v))
    //output: ("get : T -> val", "set : T -> val -> T"),
    //        ([A]t:T, C:.:"get" |-> (params) {{ pre }}-{{ post }}, [A]t:T, C:.:"set" |-> (params) {{ pre }}-{{ post }})
    // where pre and post are lifted get/set thingies, together with magic representation
    // or more concise: pre is sm_bin R ("this":expr) (sm_const t)
    // post is (void): "", sm_bin R ("this":expr) (sm_bin name t args)
    // post is (non-void): "r", sm_bin R (this) (sm_bin name t args) </\> r = name t
    var ms : List[String] = List[String]()
    val results = body.flatMap {
      case JMethodDefinition(modifiers, name, typ, params, body) =>
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
    val funs = results.map(_._1)
    ClassTable.registerInterfaceFunctions(interface, ms, funs)
    (results.map(_._1), results.map(_._2))
  }

  def interfaceSpec (supers : List[String]) : (List[String], List[String]) = {
    //input: List("ICell")
    //output: (List("get : T -> val", "set : T -> val -> T"), List("ICell C T _ get set")
    val results = supers.map({ x =>
      val (ps, pas) = ClassTable.interfaceFunctions(x)
      (ps, x + " C T R " + pas.reduceLeft(_ + " " + _))
    })
    if (results.length == 0)
      (List[String](), List[String]())
    else
      (results.map(_._1).reduceLeft(_ ++ _), results.map(_._2))
  }

  //TODO: that's wrong, since at least == and != depend on the type...
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

  def callword (c : JCall) : (String, String) = {
    c.receiver match {
      case JVariableAccess(v) => {
        val cl = ClassTable.getLocalVar(myclass, mymethod, v)
        //if (ClassTable.isMethodStatic(cl, c.fun, c.arguments.map(exprtotype)))
        //  ("cscall", cl)
        //else
          ("cdcall", v)
      }
      case x => throw new Exception("Expected JVariableAccess but got: " + x)
    }
  }

  def printStatement (something : JBodyStatement) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case JAssignment(name, value : JCall) =>
        val funname = value.fun
        val args = value.arguments
        val (callw, callpre) = callword(value)
        val argstring = args.map(printStatement).foldRight("nil")(_ + " :: " + _)
        "(" + callw + " \"" + name + "\" \"" + callpre + "\" \"" + funname + "\" (" + argstring + "))"
      case JAssignment(name, JNewExpression(typ, arg)) =>
        printStatement(JAssignment(name, JCall(JVariableAccess(typ), "new", arg))) //TODO, no idea if JVariableAccess is correct
      case JAssignment(name, JFieldAccess(variable, field)) =>
        val v = variable match {
          case JVariableAccess(x) => x
          case y =>
            Console.println("FieldAccess to " + y)
            y
        }
        "(cread \"" + name + "\" \"" + v + "\" \"" + field + "\")"
      case JFieldWrite(v, f, n) =>
        var va = v match {
          case JVariableAccess(x) => x
          case y =>
            Console.println("FieldWrite to " + y)
            y
        }
        "(cwrite \"" + va + "\" \"" + f + "\" " + printStatement(n) + ")"
      case JAssignment(name, value) => "(cassign \"" + name + "\" " + printStatement(value) + ")"
      case JBinaryExpression(op, l, r) =>
        "(" + translateOp(op) + " " + printStatement(l) + " " + printStatement(r) + ")"
      case JUnaryExpression(op, v) =>
        "(" + translateOp(op) + " " + printStatement(v) + ")"
      case JLiteral(x) => x
      case JVariableAccess(x) => "(var_expr \"" + x + "\")"
      case JCall(v, fun, arg) =>
        val args = arg.map(printStatement).foldRight("nil")(_ + " :: " + _)
        val (callw, callpre) = callword(something.asInstanceOf[JCall])
        "(" + callw + " \"ignored\" \"" + callpre + "\" \"" + fun + "\" (" + args + "))"
      case JNewExpression(typ, arg) =>
        val t = Gensym.newsym
        printStatement(JAssignment(t, JCall(JVariableAccess(typ), "new", arg))) //TODO: No idea if JVariableAccess is correct
      case y => Console.println("printStatement: no handler for " + y); ""
    }
  }

  def optPrintBody (b : Option[List[String]]) : String = {
    b match {
      case None => ""
      case Some(x) => printBody(x)
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

  def getBS (xs : JBodyStatement) : Option[List[String]] = {
    xs match {
      case JConditional(test, consequence, alternative) =>
        val te = printStatement(test)
        val tr = optPrintBody(getBS(consequence))
        val fa = optPrintBody(getBS(alternative))
        Some(List("(cif " + te + " " + tr + " " + fa + ")"))
      case JBlock(modifier, xs) => Some(xs.map(getBS).flatten.flatten) //TODO: Deal with modifer (Option[Static])
      case JWhile(test, body) =>
        Some(List("(cwhile " + printStatement(test) + " " + optPrintBody(getBS(body)) + ")"))
      case JBinding(x, typ, init) =>
        init match {
          case None => None
          case Some(y) => Some(List(printStatement(JAssignment(x, y))))
        }
      case JReturn(JVariableAccess(r)) => ret = r; None
      case a @ JAssignment(n, x) => Some(List(printStatement(a)))
      case (x : JBodyStatement) => Some(List(printStatement(x)))
      case y => Console.println("no handler for getBS " + y); None
    }
  }

  def getBody (xs : List[JStatement], myname : String) : String = {
    val (ou, ret) = getBodyHelper(xs, myname)
    outp ::= ou
    ret
  }

  def getBodyHelper (xs : List[JStatement], myname : String) : (String, String) = {
    ret = "myreturnvaluedummy"
    //Console.println("getbody, flattening " + xs)
    val body = xs.flatMap {
      case (x : JBodyStatement) => getBS(x)
    }
    //Console.println("getbody, flattened " + body)
    val b = printBody(body.flatten)
    val defi = "\nDefinition " + myname + " := " + b + "."
    val res =
      if (ret == "myreturnvaluedummy")
        "0"
      else
        "var_expr \"" + ret + "\""
    (defi, res)
  }

  def classMethods (body : List[JStatement]) : List[Pair[String,String]] = {
    body.flatMap {
      case JMethodDefinition(modifiers, name, typ, params, body) =>
        //Console.println("starting to print method " + name + " with body " + body)
        mymethod = name
        val bodyref = name + "_body"
        val args = getArgs(params)
        val returnvar =
          if (name == "new") {
            //assert(body.length == 0)
            outp ::= "Definition " + bodyref + " := (calloc \"x\" \"" + typ + "\")."
            "var_expr \"x\""
          } else
            getBody(body, bodyref)
        val t =
          if (ClassTable.isMethodStatic(myclass, mymethod, args) || name == "new")
            args
          else
            "this" :: args
        outp ::= "\nDefinition " + name + "M := Build_Method (" + printArgList(t) + ") " + bodyref + " (" + returnvar + ")."
        Some(("\"" + name + "\"", name + "M"))
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


  def coqoutput (xs : List[JStatement], spec : Boolean, name : String) : List[String] = {
    outp = List[String]()
    if (spec)
      outp = List("\n") ++ ClassTable.getCoq("PRELUDE") ++ outp
    var interfs : List[String] = List[String]()
    xs.foreach(x => x match {
      case JInterfaceDefinition(modifiers, id, inters, body) =>
        //Console.println("interfaces are " + inters)
        val (mmethods, methodspecs) = interfaceMethods(id, body)
        val mmeths = if (mmethods.length == 0) "" else mmethods.reduceLeft(_ + " " + _)
        val mspecs = if (methodspecs.length == 0) "" else methodspecs.reduceLeft(_ + "\n [/\\]\n  " + _)
        val (supermethods, interfaces) = interfaceSpec(inters)
        val superms = if (supermethods.length == 0) "" else supermethods.reduceLeft(_ + " " + _)
        val superis = if (interfaces.length == 0) "" else interfaces.reduceLeft(_ + "\n  " + _) + "\n  "
        interfs ::= "Definition " + id + " (C : class) (T : Type) (R : val -> T -> upred heap_alg) " + superms + " " + mmeths + " : spec :=\n  " + superis + mspecs + "."
      case JClassDefinition(modifiers, "Coq", supers, inters, body, par) =>
      case JClassDefinition(modifiers, id, supers, inters, body, par) =>
        //let's hope only a single class and interfaces before that!
        outp ::= "Module " + name + " <: PROGRAM."
        myclass = id
        val fields = ClassTable.getFields(id).keys.toList
        val methods = classMethods(body)
        outp ::= """
Definition """ + id + """ :=
  Build_Class """ + printFiniteSet(fields) + """
              """ + printFiniteMap(methods) + "."
    })
    val cs = printFiniteMap(ClassTable.getClasses.map(x => ("\"" + x + "\"", x)))
    outp ::= "\nDefinition Prog := Build_Program " + cs + "."
    if (ClassTable.getCoq(myclass, "PROGRAM").length == 0)
      outp ::= unique_names
    else
      outp = ClassTable.getCoq(myclass, "PROGRAM") ++ outp
    if (spec) {
      outp ::= "End " + name + "."
      outp ::= "\nImport " + name + "."
      outp = ClassTable.getCoq(myclass, "BEFORESPEC") ++ List("\n") ++ outp
      outp = interfs ++ outp
      outp ::= "\nSection " + name + "_spec."
    }
    //method specs go here
    xs.foreach(x => x match {
      case JClassDefinition(modifiers, "Coq", supers, inters, body, par) =>
      case JClassDefinition(modifiers, id, supers, inters, body, par) =>
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
    }
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
