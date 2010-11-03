package dk.itu.sdg.javaparser

trait CoqOutputter extends JavaToSimpleJava with JavaStatements {
  import java.io.PrintWriter

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
      case JLiteral(x) => "\"" + x + "\""
      case JVariableAccess(x) => "\"" + x + "\""
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
    outp.println("Definition " + myname + " :=")
    val b = printBody(body.flatten)
    outp.println(b)
    outp.println(".")
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
        outp.println("Definition " + name + "M :=")
        outp.println("  Build_Method (" + printArgList(args) + ") (" + printArgList(local) + ") " + bodyref + " (" + returnvar + ").")
        Some(("\"" + name + "\"", name + "M"))
      case _ => None
    }
  }

  private var outp : PrintWriter = null

  def coqoutput (xs : List[JStatement], out : PrintWriter) : Unit = {
    outp = out
    xs.foreach(x => x match {
      case JInterfaceDefinition(id, inters, body) =>
        //Console.println("interfaces are " + inters)
        outp.println("Definition " + id + " :=")
        val methods = interfaceMethods(body)
        outp.println("  Build_Inter " + printFiniteSet(inters) + " " + printFiniteMap(methods) + ".")
      case JClassDefinition(id, supers, inters, body, par) =>
        myclass = id
        val fields = ClassTable.getFields(id).keys.toList
        val methods = classMethods(body)
        outp.println("Definition " + id + " :=")
        outp.println("  Build_Class " + printFiniteSet(inters))
        outp.println("              " + printFiniteSet(fields))
        outp.println("              " + printFiniteMap(methods) + ".")
    })
    val cs = printFiniteMap(ClassTable.getClasses.map(x => ("\"" + x + "\"", x)))
    val is = printFiniteMap(ClassTable.getInterfaces.map(x => ("\"" + x + "\"", x)))
    outp.println("Definition P :=")
    outp.println("  Build_Program " + cs)
    outp.println("                " + is + ".")
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
