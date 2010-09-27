package dk.itu.sdg.javaparser

import scala.util.parsing.input._
import java.io.PrintWriter

trait JavaAST extends JavaParser {
  def parse(r: Reader[Char], out : PrintWriter) : ParseResult[Any] = {
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    p match {
      case Success(x @ ~(_,_), _) => CoqOutputter.output(x, out)
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
    }
    p
  }

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
}

object Gensym {
  var freevars : List[String] = List[String]()
  var count : Int = 0
  def newsym () : String = {
    count += 1
    freevars ::= "tmp_" + count
    return "tmp_" + count
  }

  def getfree () : List[String] = {
    val tmp = freevars
    freevars = List[String]()
    tmp
  }
}


trait JavaToSimpleJava extends JavaTerms {

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
        val t = Gensym.newsym
        extractHelper(rt, Expr(t) :: acca, Assignment(QualId(List(t)), Call(name, as)) :: ins ++ acci)
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
}

import scala.util.parsing.combinator.Parsers
object CoqOutputter extends JavaTerms with Parsers with JavaToSimpleJava {
  import scala.collection.mutable.HashMap
  var symboltable : HashMap[String,String] = new HashMap[String,String]()

  var classes : List[Pair[String,String]] = List[Pair[String,String]]()
  var interfaces : List[Pair[String,String]] = List[Pair[String,String]]()

  def getArgs (as : Option[Any], acc : List[String]) : List[String] = {
    as match {
      case Some(x : List[Any]) => getArgsHelper(x, acc).reverse
      case Some(x : FormalVariable) => getArgsHelper(List(x), acc).reverse
      case x => acc
    }
  }

  def getArgsHelper (x : List[Any], acc : List[String]) : List[String] = {
    x match {
      case FormalVariable(modifier, jtype, Name(id)) :: rt =>
        symboltable += id -> jtype.toString
        getArgsHelper(rt, id :: acc)
      case a :: rt => getArgsHelper(rt, acc)
      case y => acc
    }
  }

  def printArgList (l : List[String]) : String = {
    l match {
      case Nil => "nil"
      case a :: b => "\"" + a + "\" :: " + printArgList(b)  //or [foo, bar, baz]?
    }
  }

  def interfaceMethods (body : List[Any], acc : List[Pair[String,String]]) : List[Pair[String,String]] = {
    body match {
      case Nil => acc.reverse
      case MethodDeclaration(Name(name), jtype, params, throws, body) :: rt =>
        val ps = getArgs(params, List[String]())
        val f = "(" + printArgList(ps) + ")"
        interfaceMethods(rt, ("\"" + name + "\"", f) :: acc)
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

  def getExpr (something : Any) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case Some(x) => getExpr(x)
      case PrimaryExpr(x) => getExpr(x)
      case AnyStatement(x) => getExpr(x)
      case ParExpr(x) => "(" + getExpr(x) + ")"
      case Block(x) => "(" + printBody(x.map(getExpr)) + ")"
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
        val argstring = if (args.length > 0) args.map(getExpr).reduceLeft(_ + " " + _) else "nil" //XXX: so wrong!
        val typ = symboltable(p)
        "(ccall \"" + res + "\" \"" + p + "\" \"" + unpackR(mname) + "\" ([" + argstring + "]) " + "(TClass \"" + typ + "\")" + ")"
      case Assignment(name, value) => "(cassign " + unpackR(name) + " " + getExpr(value) + ")"
      case y => unpackR(y)
    }
  }

  def printBody (bs : List[String]) : String = {
    bs match {
      case Nil => ""
      case x :: Nil => x
      case x :: y => "(cseq " + x + " " + printBody(y) + ")"
    }
  }

  def getBody (xs : List[AnyExpr], myname : String) : (List[String], String) = {
    var ret = "myreturnvaluedummy"
    var freevars = List[String]()
    val body = xs.map(x => x match {
      case AnyStatement(x) => x match {
        case Primitive(x)~(names : List[Name]) =>
          freevars ++= names.map(_.name); None //localvars (of primitive type)!
        case x => Some(getExpr(x))
      }
      case Conditional(test, consequence, alternative) =>
        val te = getExpr(test)
        val tr = getExpr(consequence)
        val fa = getExpr(alternative)
        Some("cif " + te + "\n    " + tr + "\n    " + fa)
      case Return(r) => ret = unpackR(r); None
      case x => Some(getExpr(x))
    })
    outp.println("Definition " + myname + " :=")
    val b = printBody(body.flatten)
    outp.println(b)
    outp.println(".")
    (freevars ++ Gensym.getfree, "var_expr \"" + ret + "\"")
  }

  def classMethods (body : List[Any], acc : List[Pair[String,String]]) : List[Pair[String,String]] = {
    body match {
      case Nil => acc.reverse
      case MethodDeclaration(Name(name), jtype, params, throws, Block(body)) :: rt =>
        val bodyref = name + "_body"
        val cbody = extractCalls(body, List[AnyExpr]())
        val args = getArgs(params, List[String]())
        val (local, returnvar) = getBody(cbody, bodyref)
        outp.println("Definition " + name + "M :=")
        outp.println("  Build_Method (" + printArgList(args) + ") (" + printArgList(local) + ") " + bodyref + " (" + returnvar + ").")
        classMethods(rt, ("\"" + name + "\"", name + "M") :: acc)
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

  def coqoutput (xs : Any) : Unit = {
    xs match {
      case Nil =>
      case x1~x2 => coqoutput(x1); coqoutput(x2)
      case JInterface(Name(id), typ, inters, body) =>
        //Console.println("interfaces are " + inters)
        interfaces ::= ("\"" + id + "\"", id)
        outp.println("Definition " + id + " :=")
        val supers = List[String]() //XXX: super-inters
        val methods = interfaceMethods(body, List[Pair[String,String]]())
        outp.println("  Build_Inter " + printList(supers, "_") + " " + printMap(methods, "_") + ".")
      case JClass(Name(id), typ, supers, inters, body) =>
        classes ::= ("\"" + id + "\"", id)
        val ints = inters match {
          case Some(x : List[List[Any]]) => getInterfaces(x.flatten, List[String]())
          case Some(x : List[Any]) => getInterfaces(x, List[String]())
          case None => List[String]()
        }
        symboltable = new HashMap[String, String]()
        symboltable += "this" -> id
        val fields = List[String]() //XXX
        val methods = classMethods(body, List[Pair[String,String]]())
        outp.println("Definition " + id + " :=")
        outp.println("  Build_Class " + printList(ints.reverse, "_"))
        outp.println("              " + printList(fields, "_"))
        outp.println("              " + printMap(methods, "Method") + ".")
      case x :: tl => coqoutput(x); tl.foreach(coqoutput(_))
      case Some(x) => coqoutput(x)
      case None =>
      case x => Console.println("leaf " + x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
    }
  }

  def printList (l : List[String], mtype : String) : String = {
    l match {
      case Nil => "(SS.empty " + mtype + ")"
      case k :: b => "(SS.add " + k + " " + printList(b, mtype) + ")"
    }
  }

  def printMap (map : List[Pair[String, String]], mtype : String) : String = {
    map match {
      case Nil => "(SM.empty " + mtype + ")"
      case (k,v) :: b => "(SM.add " + k + " " + v + " " + printMap(b, mtype) + ")"
    }
  }

  var outp : PrintWriter = null

  def output (x : Any, out : PrintWriter) : Unit = {
    outp = out
    coqoutput(x)
    val cs = printMap(classes, "Class")
    val is = printMap(interfaces, "Inter")
    outp.println("Definition P :=")
    outp.println("  Build_Program " + cs)
    outp.println("                " + is + ".")
  }
}
