package dk.itu.sdg.javaparser

trait CoqOutputter extends JavaToSimpleJava {
  private var myclass : String = ""
  private var mymethod : String = ""

  def getArgs (x : List[InnerStatement]) : List[String] = {
    x.flatMap {
      case JArgument(id, jtype) =>
        Some(id)
      case (x : JBinaryExpression) =>
        Some(printStatement(x))
      case _ => None
    }
  }

  def interfaceMethods (body : List[JStatement]) : List[Pair[String,String]] = {
    body.flatMap {
      case JMethodDefinition(name, params, body) =>
        val ps = getArgs(params)
        val f = "(" + printArgList(ps) + ")"
        Some(("\"" + name + "\"", f))
    }
  }

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

  def printStatement (something : JBodyStatement) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case JAssignment(name, value : JCall) =>
        val funname = value.fun
        val args = value.arguments
        val callpref = value.variable
        val argstring = args.map(printStatement).foldRight("nil")(_ + " :: " + _)
        val typ = ClassTable.getLocalVar(myclass, mymethod, callpref)
        "(ccall \"" + name + "\" \"" + callpref + "\" \"" + funname + "\" (" + argstring + ") " + "(TClass \"" + typ + "\")" + ")"
      case JAssignment(name, JNewExpression(typ, arg)) =>
        val t = typ
        val init = printStatement(JAssignment(name, JCall("this", "<init>", arg)))
        "(cseq (calloc \"" + name + "\" \"" + t + "\")" + init + ")"
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
        val t = ClassTable.getLocalVar(myclass, mymethod, v)
        val args = arg.map(printStatement).foldRight("nil")(_ + " :: " + _)
        "(ccall \"ignored\" \"" + v + "\" \"" + fun + "\" (" + args + ") (TClass \"" + t + "\"))"
      case JNewExpression(typ, arg) =>
        val t = Gensym.newsym
        freevars += t
        val init = printStatement(JAssignment(t, JCall("this", "<init>", arg)))
        "(cseq (calloc \"" + t + "\" \"" + typ + "\")" + init + ")"
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

  import scala.collection.mutable.Set
  private var freevars : Set[String] = Set[String]()
  private var ret : String = "myreturnvaluedummy"

  def getBS (xs : JBodyStatement) : Option[List[String]] = {
    xs match {
      case JConditional(test, consequence, alternative) =>
        val te = printStatement(test)
        val tr = optPrintBody(getBS(consequence))
        val fa = optPrintBody(getBS(alternative))
        Some(List("(cif " + te + " " + tr + " " + fa + ")"))
      case JBlock(xs) => Some(xs.map(getBS).flatten.flatten)
      case JWhile(test, body) =>
        Some(List("(cwhile " + printStatement(test) + " " + optPrintBody(getBS(body)) + ")"))
      case JBinding(x, typ, init) =>
        freevars += x
        init match {
          case None => None
          case Some(y) => Some(List(printStatement(JAssignment(x, y))))
        }
      case JReturn(JVariableAccess(r)) => ret = r; None
      case a @ JAssignment(n, x) => freevars += n; Some(List(printStatement(a)))
      case (x : JBodyStatement) => Some(List(printStatement(x)))
      case y => Console.println("no handler for getBS " + y); None
    }
  }

  def getBody (xs : List[JStatement], myname : String) : (List[String], String) = {
    ret = "myreturnvaluedummy"
    freevars = Set[String]()
    //Console.println("getbody, flattening " + xs)
    val body = xs.flatMap {
      case (x : JBodyStatement) => getBS(x)
    }
    //Console.println("getbody, flattened " + body)
    val b = printBody(body.flatten)
    outp ::= "\nDefinition " + myname + " := " + b + "."
    (freevars.toList, "var_expr \"" + ret + "\"")
  }

  def classMethods (body : List[JStatement]) : List[Pair[String,String]] = {
    body.flatMap {
      case JMethodDefinition(name, params, body) =>
        //Console.println("starting to print method " + name + " with body " + body)
        mymethod = name
        val bodyref = name + "_body"
        val args = getArgs(params)
        val (local, returnvar) = getBody(body, bodyref)
        outp ::= "\nDefinition " + name + "M := Build_Method (" + printArgList(args) + ") (" + printArgList(local) + ") " + bodyref + " (" + returnvar + ")."
        Some(("\"" + name + "\"", name + "M"))
      case _ => None
    }
  }

  private var outp : List[String] = null

  def coqoutput (xs : List[JStatement]) : List[String] = {
    outp = List[String]()
    val name = "Fac" //XXX: that's hardcoded
    outp = ClassTable.getCoq("PRELUDE") ++ outp

    outp ::= "Module " + name + " <: PROGRAM."
    xs.foreach(x => x match {
      case JInterfaceDefinition(id, inters, body) =>
        //Console.println("interfaces are " + inters)
        val methods = interfaceMethods(body)
        outp ::= "Definition " + id + " := Build_Inter " + printFiniteSet(inters) + " " + printFiniteMap(methods) + "."
      case JClassDefinition("Coq", supers, inters, body, par) =>
      case JClassDefinition(id, supers, inters, body, par) =>
        myclass = id
        val fields = ClassTable.getFields(id).keys.toList
        val methods = classMethods(body)
        outp ::= """
Definition """ + id + """ :=
  Build_Class """ + printFiniteSet(inters) + """
              """ + printFiniteSet(fields) + """
              """ + printFiniteMap(methods) + "."
    })
    val cs = printFiniteMap(ClassTable.getClasses.map(x => ("\"" + x + "\"", x)))
    val is = printFiniteMap(ClassTable.getInterfaces.map(x => ("\"" + x + "\"", x)))
    outp ::= """
Definition P :=
  Build_Program """ + cs + """
                """ + is + "."
    outp = ClassTable.getCoq(myclass, "PROGRAM") ++ outp
    outp ::= "End " + name + "."
    outp ::= "Module " + name + "_spec <: PROG_SPEC " + name + ".\nImport " + name + ".\n"
    outp = ClassTable.getCoq(myclass, "BEFORESPEC") ++ outp
    //method specs go here
    val specs = ClassTable.getSpecs(myclass)
    specs.keys.foreach(x =>
      outp ::= "Definition " + x + """_spec :=
  Build_spec unit (fun _ => (""" + specs(x)._1.replace("'", "\"") + ",\n" + specs(x)._2.replace("'", "\"") + ")).")
    //now we need to connect implementation class (methods) to specs
    //Spec: Class/Interface -> Method -> Arguments * specT
    val spcs = printFiniteMap(specs.keys.map(x => ("\"" + x + "\"", "(" + printArgList(ClassTable.getArguments(myclass, x).keys.toList) + ", " + x + "_spec" + ")")).toList)
    outp ::= "Definition Spec := TM.add (TClass \"" + myclass + "\") " + spcs + " (TM.empty _)."
    outp = ClassTable.getCoq(myclass, "AFTERSPEC") ++ outp
    outp ::= "End " + name + "_spec."
    outp = ClassTable.getCoq("TOP") ++ outp
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
