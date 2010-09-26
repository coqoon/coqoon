import scala.util.parsing.input._

trait JavaAST extends JavaParser
{
  def parse(r: Reader[Char]) : ParseResult[Any] = {
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    p match {
      case Success(x @ ~(_,_), _) => 
        coqoutput(x)
        val cs = if (classes.length > 0)
                   classes.reduceLeft(_ + " " + _)
                 else
                   ""
        val is = if (interfaces.length > 0)
                   interfaces.reduceLeft(_ + " " + _)
                 else
                   ""
        Console.println("Definition P := ")
        Console.println("  Build_Program (SM'.singleton " + cs + ")")
        Console.println("                (SM'.singleton " + is + ").")
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
    }
    p
  }

  // override def qualifiedId = rep1sep(id, ".") ^^ { x => x.reduceLeft("")((a,b) => a + "." + b) } ^^ ida

  def pp(xs: Any, indent: Int): Unit = {
    def iprint(s: String) = {
      for (i <- 1 to indent) Console.print(" ")
      Console.print(s)
    }
    def iprintln(s: String) = iprint(s); Console.print("\n");

    xs match {
      case x1~x2 =>
        pp(x1, indent + 2)
        print(" ~ ")
        pp(x2, indent + 2)
      case xs @ List(_) =>
        for (x <- xs) yield pp(x, indent + 2)
      case xs : JClass =>
        iprintln("Class " + xs.id + " (" + xs.body.length + ")"); for (x <- xs.body) yield pp(x, indent + 2)
      case x @ _ =>
        iprintln(x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
    }
  }

  var classes : List[String] = List[String]()
  var interfaces : List[String] = List[String]()

  def getArgs (as : Option[Any], acc : List[String]) : List[String] = {
    as match {
      case Some(x : List[Any]) => getArgsHelper(x, acc).reverse
      case Some(x : FormalVariable) => getArgsHelper(List(x), acc).reverse
      case x => acc
    }
  }

  def getArgsHelper (x : List[Any], acc : List[String]) : List[String] = {
    x match {
      case FormalVariable(modifier, jtype, Name(id)) :: rt => getArgsHelper(rt, id :: acc)
      case a :: rt => getArgsHelper(rt, acc)
      case y => acc
    }
  }

  def pList (l : List[String], esc : Boolean) : String = {
    if (l.length > 0) {
      val t = if (esc) 
                l.map("\"" + _ + "\"")
              else
                l
      t.reduceLeft(_ + ", " + _)
    } else
      ""
  }

  def interfaceMethods (body : List[Any], acc : List[String]) : List[String] = {
    body match {
      case Nil => acc.reverse
      case MethodDeclaration(Name(name), jtype, params, throws, body) :: rt =>
        val ps = getArgs(params, List[String]())
        val f = "\"" + name + "\" ([" + pList(ps, true) + "])"
        interfaceMethods(rt, f :: acc)
      case x ~ (y : MethodDeclaration) :: rt => interfaceMethods(y :: rt, acc)
      case x :: rt => Console.println("interfacemethods: no handler for " + x); interfaceMethods(rt, acc)
    }
  }

  def unpackR (r : Any) : String = {
    r match {
      case QualId(xs) => xs.map(unpackR).reduceLeft(_ + "." + _)
      case Some(x) => unpackR(x)
      case Expr(x) => unpackR(x)
      case PrimaryExpr(x) => unpackR(x)
      case Name(x) => x
      case Num(x) => x
      case Lit(x) => unpackR(x)
      case x :: rt => unpackR(x) + unpackR(rt)
      case x : String => x
      case Nil => ""
      case None => ""
      case x => Console.println("unpackR dunno " + x + " class " + x.asInstanceOf[AnyRef].getClass.getName); ""
    }
  }

  def translateOp (x : Any) : String = {
    x match {
      case Key(x) => if (x == ">") "egt"
                     else if (x == "-") "eminus"
                     else if (x == "*") "etimes"
                     else if (x == "+") "eplus"
                     else if (x == "=") "cassign"
                     else { Console.println("translateOp dunno Key " + x); "" }
      case x => Console.println("translateOp dunno " + x); ""
    }
  }

  var freevars : List[String] = List[String]()
  var count : Int = 0
  def gensym () : QualId = {
    count += 1
    freevars ::= "tmp_" + count
    return QualId(List("tmp_" + count))
  }

  def getExpr (something : Any) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case Some(x) => getExpr(x)
      case PrimaryExpr(x) => getExpr(x)
      case AnyStatement(x) => getExpr(x)
      case ParExpr(x) => "(" + getExpr(x) + ")"
      case Block(x) => "(" + x.map(getExpr).reduceLeft(_ + " " + _) + ")"
      case Expr(x) => getExpr(x)
      case BinaryExpr(op, l, r) => "(" + translateOp(op) + " " + getExpr(l) + " " + getExpr(r) + ")"
      case Assignment(name, value : Call) =>
        val funname = value.fun
        val args = value.arguments
        val res = unpackR(name)
        val callpref = funname.xs.dropRight(1)
        val p = if (callpref.length == 0)
                  "this"
                else
                  callpref.map(unpackR).reduceLeft(_ + "." + _)
        val mname = if (funname.xs.length == 1) funname.xs else funname.xs.takeRight(1)
        val argstring = if (args.length > 0) args.map(getExpr).reduceLeft(_ + " " + _) else "nil"
        "(ccall \"" + res + "\" \"" + p + "\" \"" + unpackR(mname) + "\" ([" + argstring + "]) " + "(virtualcallinstance)" + ")"
      case Assignment(name, value) => "(cassign " + unpackR(name) + " " + getExpr(value) + ")"
      case Call(funname : QualId, args : List[AnyExpr]) =>
        Console.println("getExpr: Call called, shouldn't happen!")
        val result = gensym
        val callpref = funname.xs.dropRight(1)
        val p = if (callpref.length == 0)
                  "this"
                else
                  callpref.map(unpackR).reduceLeft(_ + "." + _)
        val mname = if (funname.xs.length == 1) funname.xs else funname.xs.takeRight(1)
        "(ccall \"" + result + "\" \"" + p + "\" \"" + unpackR(mname) + "\" ([" + args.map(getExpr).reduceLeft(_ + " " + _) + "]) " + "(virtualcallinstance)" + ")"
      case y => unpackR(y)
    }
  }

  def extractCalls (statements : List[AnyExpr], acc : List[AnyExpr]) : List[AnyExpr] = {
    statements match {
      case Expr(x : AnyExpr) :: xs => //doesn't preserve Expr
        val r = extractCalls(List(x), List[AnyExpr]())
        //assert(r.length == 1)
        extractCalls(xs, r ++ acc)
      case PrimaryExpr(x : AnyExpr) :: xs => //doesn't preserve PrimaryExpr
        val r = extractCalls(List(x), List[AnyExpr]())
        //Console.println("result " + r)
        //assert(r.length == 1)
        extractCalls(xs, r ++ acc)
      case BinaryExpr(op, l, r) :: xs =>
        //l must be simple!?
        val (args, ins) = extractHelper(List(r), List[AnyExpr](), List[AnyExpr]())
        assert(args.length == 1)
        extractCalls(xs, ins ++ (BinaryExpr(op, l, args(0)) :: acc))
      case Nil => acc.reverse
      case Conditional(t, c, a) :: xs =>
        val (ta, ti) = extractHelper(List(t.e), List[AnyExpr](), List[AnyExpr]())
        assert(ta.length == 1)
        val tst = ParExpr(ta(0))
        val con = c match {
          case Block(cs) => Block(extractCalls(cs, List[AnyExpr]()))
          case x =>
            val r = extractCalls(List(x), List[AnyExpr]())
            if (r.length == 1)
              r(0)
            else
              Block(r)
        }
        val alt = a match {
          case None => None
          case Some(w) => w match {
            case Block(as) => Some(Block(extractCalls(as, List[AnyExpr]())))
            case x =>
              val r = extractCalls(List(x), List[AnyExpr]())
              if (r.length == 1)
                Some(r(0))
              else
                Some(Block(r))
          }
        }
        extractCalls(xs, Conditional(tst, con, alt) :: ti ++ acc)
      case Call(name, args) :: xs => 
        val (ars, ins) = extractHelper(args, List[AnyExpr](), List[AnyExpr]())
        //Console.println("results from extracthelper: " + ars + " ins " + ins)
        extractCalls(xs, Call(name, ars) :: ins ++ acc)
      case x :: xs => extractCalls(xs, x :: acc)
    }
  }

  def extractHelper (xs : List[AnyExpr], acca : List[AnyExpr], acci : List[AnyExpr]) : (List[AnyExpr], List[AnyExpr]) = {
    //Console.println("extracthelper called with " + xs + " acca " + acca + " acci " + acci)
    xs match {
      case Nil => (acca.reverse, acci)
      case Expr(x : AnyExpr) :: xs => //doesn't preserve Expr!
        val (a, i) = extractHelper(List(x), acca, acci)
        extractHelper(xs, a, i)
      case PrimaryExpr(x : AnyExpr) :: xs => //doesn't preserve PrimaryExpr
        val (a, i) = extractHelper(List(x), acca, acci)
        extractHelper(xs, a, i)
      case BinaryExpr(op, l, r) :: xs =>
        val (ri, rii) = extractHelper(List(r), List[AnyExpr](), acci)
        val (le, lei) = extractHelper(List(l), List[AnyExpr](), rii)
        //Console.println("HELP le " + le + " ri " + ri + " is " + lei)
        assert(le.length == 1 && ri.length == 1)
        extractHelper(xs, BinaryExpr(op, le(0), ri(0)) :: acca, lei)
      case Call(name, args) :: rt =>
        val (as, ins) = extractHelper(args, List[AnyExpr](), List[AnyExpr]())
        val t = gensym
        extractHelper(rt, Expr(t) :: acca, Assignment(t, Call(name, as)) :: ins ++ acci)
/*      case Conditional(test, c, Some(a)) :: rt =>
        val t = gensym
        val (ta, ti) = extractHelper(List(test.e), List[AnyExpr](), List[AnyExpr]())
        val (ca, ci) = extractHelper(List(c), List[AnyExpr](), List[AnyExpr]())
        val (aa, ai) = extractHelper(List(a), List[AnyExpr](), List[AnyExpr]())
        assert(ca.length == aa.length == 0)
        extractHelper(rt, t :: acca, ti ++ Conditional(ParExpr(ta(0)),
                                                       ci :: Assignment(t, last),
                                                       ai :: Assignment(t, last))) */
      case x :: rt => extractHelper(rt, x :: acca, acci)
    }
  }

  def getBody (xs : List[AnyExpr], myname : String) : (List[String], String) = {
    var ret = "myreturnvaluedummy"
    Console.println("Definition " + myname + " :=")
    xs.foreach(x => x match {
      case AnyStatement(x) => x match {
        case Primitive(x)~(names : List[Name]) =>
          freevars ++= names.map(_.name) //localvars (of primitive type)!
        case x => Console.println(getExpr(x))
      }
      case Conditional(test, consequence, alternative) =>
        val te = getExpr(test)
        val tr = getExpr(consequence)
        val fa = getExpr(alternative)
        Console.println("cif " + te)
        Console.println("    " + tr)
        Console.println("    " + fa)
      case Return(r) => ret = unpackR(r)
      case x => Console.println(getExpr(x))
    })
    Console.println(".")
    (freevars, "var_expr \"" + ret + "\"")
  }

  def classMethods (body : List[Any], acc : List[String]) : List[String] = {
    body match {
      case Nil => acc.reverse
      case MethodDeclaration(Name(name), jtype, params, throws, Block(body)) :: rt =>
        freevars = List[String]()
        val bodyref = name + "_body"
        val cbody = extractCalls(body, List[AnyExpr]())
        val (local, returnvar) = getBody(cbody, bodyref)
        val args = getArgs(params, List[String]())
        Console.println("Definition " + name + "M :=")
        Console.println("  Build_Method ([" + pList(args, true) + "]) ([" + pList(local, true) + "]) " + bodyref + " (" + returnvar + ").")
        classMethods(rt, "\"" + name + "\"" :: name + "M" :: acc)
      case x ~ (y : MethodDeclaration) :: rt => classMethods(y :: rt, acc)
      case x :: rt => Console.println("classmethods: no handler for " + x); classMethods(rt, acc)      
    }
  }

  def getInterfaces (x : List[Any], acc : List[String]) : List[String] = {
    x match {
      case Nil => acc
      case Name(a) :: rt => getInterfaces(rt, a :: acc)
      case a :: rt => Console.println("interfaces: dunno about " + a + " (class) " + a.asInstanceOf[AnyRef].getClass.toString + " rest " + rt); getInterfaces(rt, acc)
    }
  }

  def coqoutput (xs : Any) : String = {
    xs match {
      case Nil =>
      case x1~x2 => coqoutput(x1); coqoutput(x2)
      case JInterface(Name(id), typ, inters, body) =>
        //Console.println("interfaces are " + inters)
        interfaces ::= id
        interfaces ::= "\"" + id + "\""
        Console.println("Definition " + id + " :=")
        val supers = "SS.empty" //inters
        val methods = interfaceMethods(body, List[String]())
        Console.println("  Build_Inter (" + supers + ") (SM'.singleton " + pList(methods, false) + ").")
      case JClass(Name(id), typ, supers, inters, body) =>
        classes ::= id
        classes ::= "\"" + id + "\""
        val ints = inters match {
          case Some(x : List[List[Any]]) => getInterfaces(x.flatten, List[String]())
          case Some(x : List[Any]) => getInterfaces(x, List[String]())
          case None => List[String]()
        }
        val methods = classMethods(body, List[String]())
        val fields = "SS.empty"
        val ms = if (methods.length > 0)
                   methods.reverse.reduceLeft(_ + " " + _)
                 else
                   ""
        Console.println("Definition " + id + " :=")
        Console.println("  Build_Class (SS.singleton " + pList(ints.reverse, true) + ")")
        Console.println("              (" + fields + ")")
        Console.println("              (SM'.singleton " + ms + ").")
      case x :: tl => coqoutput(x); tl.foreach(coqoutput(_))
      case Some(x) => coqoutput(x)
      case None =>
      case x => Console.println("leaf " + x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
    }
    ""
  }
}
