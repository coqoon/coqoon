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
                     else if (x == "=") "cassign"
                     else { Console.println("translateOp dunno Key " + x); "" }
      case x => Console.println("translateOp dunno " + x); ""
    }
  }

  var count : Int = 0
  def gensym () : String = {
    count += 1
    return "tmp_" + count
  }

  def getExpr (something : Any) : String = {
    //Console.println("getexpr called with " + something + " class " + something.asInstanceOf[AnyRef].getClass.getName)
    something match {
      case Some(x) => getExpr(x)
      case PrimaryExpr(x) => getExpr(x)
      case Stmt(x) => getExpr(x)
      case ParExpr(x) => "(" + getExpr(x) + ")"
      case Block(x) => "(" + x.map(getExpr).reduceLeft(_ + " " + _) + ")"
      case BlockStmt(x) => getExpr(x)
      case Expr(x) => getExpr(x)
      case BinaryExpr(op, l, r) => "(" + translateOp(op) + " " + getExpr(l) + " " + getExpr(r) + ")"
      case (x:QualId)~(y:List[Any]) =>
        val result = gensym
        val callpref = x.xs.dropRight(1)
        val p = if (callpref.length == 0)
                  "this"
                else
                  callpref.map(unpackR).reduceLeft(_ + "." + _)
        val mname = if (x.xs.length == 1) x.xs else x.xs.takeRight(1)
        "(ccall \"" + result + "\" \"" + p + "\" \"" + unpackR(mname) + "\" ([" + y.map(getExpr).reduceLeft(_ + " " + _) + "]) " + "(virtualcallinstance)" + ")"
      case y => unpackR(y)
    }
  }

  def getBody (xs : List[BlockStmt], myname : String) : (List[String], String) = {
    var vars : List[String] = List[String]()
    var ret = "myreturnvaluedummy"
    Console.println("Definition " + myname + " :=")
    xs.foreach(x => x match {
      case BlockStmt(x) => x match {
        case Primitive(x)~(names : List[Name]) => vars ++= names.map(_.name) //localvars (of primitive type)!
        case Conditional(test, consequence, alternative) =>
          val te = getExpr(test)
          val tr = getExpr(consequence)
          val fa = getExpr(alternative)
          Console.println("cif " + te)
          Console.println("    " + tr)
          Console.println("    " + fa)
        case ReturnStmt(r) => ret = unpackR(r)
        case x => Console.println("getBody dunno " + x)
      }
    })
    Console.println(".")
    (vars, "var_expr \"" + ret + "\"")
  }

  def classMethods (body : List[Any], acc : List[String]) : List[String] = {
    body match {
      case Nil => acc.reverse
      case MethodDeclaration(Name(name), jtype, params, throws, Block(body)) :: rt =>
        val bodyref = name + "_body"
        val (local, returnvar) = getBody(body, bodyref)
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
