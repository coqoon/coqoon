package dk.itu.sdg.javaparser

import scala.util.parsing.input._
import java.io.PrintWriter

trait JavaAST extends JavaParser {
  def parse(r: Reader[Char], out : PrintWriter) : ParseResult[Any] = {
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    p match {
      case Success(x @ ~(_,_), _) =>
        val ast = FinishAST.doit(x, out);
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
  var count : Int = 0
  def newsym () : String = {
    count += 1
    return "tmp_" + count
  }
}


trait JavaStatements {
  trait JStatement
  case class JClassDefinition (id : String, superclass : String, interfaces : List[String], body : List[JStatement], outerclass : Option[String]) extends JStatement
  case class JInterfaceDefinition (id : String, interfaces : List[String], body : List[JStatement]) extends JStatement

  trait InnerStatement extends JStatement

  case class JFieldDefinition (id : String, jtype : String) extends InnerStatement
  case class JMethodDefinition (id : String, parameters : List[JArgument], body : List[JBodyStatement]) extends InnerStatement

  case class JArgument (id : String, jtype : String) extends InnerStatement

  trait JBodyStatement extends InnerStatement
  case class JBlock (body : List[JBodyStatement]) extends JBodyStatement
  case class JAssignment (left : String, right : JExpression) extends JBodyStatement
  case class JFieldWrite (variable : JExpression, field : String, value : JExpression) extends JBodyStatement
  case class JReturn (ret : JExpression) extends JBodyStatement
  case class JBinding (name : String, jtype : String, init : Option[JExpression]) extends JBodyStatement
  case class JWhile (test : JExpression, body : JBlock) extends JBodyStatement

  trait JExpression extends JBodyStatement
  case class JConditional (test : JExpression, consequent : JBodyStatement, alternative : JBodyStatement) extends JExpression
  case class JBinaryExpression (operation : String, left : JExpression, right : JExpression) extends JExpression
  case class JUnaryExpression (operation : String, expr : JExpression) extends JExpression
  case class JPostfixExpression (operation : String, expr : JExpression) extends JExpression
  case class JCall (variable : String, fun : String, arguments : List[JExpression]) extends JExpression
  case class JNewExpression (jtype : String, arguments : List[JExpression]) extends JExpression
  case class JLiteral (value : String) extends JExpression
  case class JVariableAccess (variable : String) extends JExpression
  case class JFieldAccess (variable : JExpression, field : String) extends JExpression
}

object ClassTable {
  import scala.collection.mutable.HashMap
  var classtable : HashMap[String,(Boolean, Option[String], HashMap[String,String], HashMap[String,(String,HashMap[String,String])])] = new HashMap[String,(Boolean, Option[String], HashMap[String,String], HashMap[String,(String,HashMap[String,String])])]()

  def registerClass (id : String, outer : Option[String], interface : Boolean) = {
    if (classtable.contains(id))
      Console.println("overwriting of " + id + " in CT not allowed")
    else
      classtable += id -> (interface, outer, new HashMap[String,String](), new HashMap[String,(String,HashMap[String,String])]())
  }

  def checkkey (id : String, key : String) = {
    assert(! classtable(id)._3.contains(key))
    assert(! classtable(id)._4.contains(key))
  }

  def addField (id : String, key : String, value : String) = {
    checkkey(id, key)
    classtable(id)._3 += key -> value
  }

  def addMethod (id : String, key : String, value : String, lvars : HashMap[String,String]) = {
    checkkey(id, key)
    classtable(id)._4 += key -> (value, lvars)
  }

  def getOuter (id : String) : Option[String] = {
    if (classtable.contains(id))
      classtable(id)._2
    else
      None
  }

  def getFields (id : String) : HashMap[String,String] = {
    classtable(id)._3
  }

  def getType (classname : String, key : String) : String = {
    if (classtable.contains(classname)) {
      val tup = classtable(classname)
      if (tup._3.contains(key))
        tup._3(key)
      else
        tup._4(key)._1
    } else {
      Console.println("CT doesn't contain " + classname)
      "Object"
    }
  }

  def getLocalVar(id : String, method : String, name : String) : String = {
    if (name == "this")
      id
    else
      if (classtable(id)._4(method)._2.contains(name))
        classtable(id)._4(method)._2(name)
      else
        classtable(id)._3(name)
  }

  def isInterface (name : String) : Boolean = {
    classtable(name)._1
  }

  def getClasses () : List[String] = {
    classtable.keys.filterNot(isInterface).toList
  }

  def getInterfaces () : List[String] = {
    classtable.keys.filter(isInterface).toList
  }
}

import scala.util.parsing.combinator.Parsers
object FinishAST extends JavaTerms with Parsers with JavaStatements with JavaToSimpleJava with CoqOutputter {
  import scala.collection.mutable.HashMap

  private var classid : String = ""
  private var lvars : HashMap[String,String] = new HashMap[String,String]()

  def unpackR (r : Any) : String = {
    r match {
      case x~y => unpackR(x) + unpackR(y)
      case QualId(xs) => "\"" + xs.map(unpackR).reduceLeft(_ + "." + _) + "\"" //XXX: ???
      case Some(x) => unpackR(x)
      case Expr(x) => unpackR(x)
      case PrimaryExpr(x) => unpackR(x)
      case PostFixExpression(x) => unpackR(x)
      case Name(x) => x
      case Num(x) => x
      case Lit(x) => unpackR(x)
      case Str(x) => x
      case x :: rt => unpackR(x) + unpackR(rt)
      case x : String => x
      case Primitive(x) => unpackR(x)
      case Key(x) => unpackR(x)
      case Nil => ""
      case None => ""
      case x => Console.println("unpackR dunno " + x + " class " + x.asInstanceOf[AnyRef].getClass.getName); ""
    }
  }

  def transformOLF (xs : Option[Any]) : List[String] = {
    xs match {
      case Some(xs : List[List[Any]]) => xs.flatten.map(unpackR)
      case None => List[String]()
    }
  }

  def transformOL [T](xs : Option[Any], x : List[T]) : List[T] = {
    xs match {
      case None => List[T]()
      case Some(x : List[Any]) =>
        transformList(x).map(x => if (x.isInstanceOf[T]) Some(x.asInstanceOf[T]) else None).flatten
      case Some(y) => Console.println("transformOL dunno " + y); List[T]()
    }
  }

  def transformList (xs : List[Any]) : List[JStatement] = {
    xs.map(transform).flatten
  }

  def transformBlock (cs : JStatement) : JBlock = {
    cs match {
      case (x : JBlock) => x
      case (xs : JBodyStatement) => JBlock(List(xs))
      case x => Console.println("transformBlock, dunno about " + x); null
    }
  }

  def transformExpression (x : Any) : JExpression = {
    val y = transformStatement(x)
    if (y.isInstanceOf[JExpression])
      y.asInstanceOf[JExpression]
    else {
      Console.println("transformExpression: wanted an JExpression, got " + y);
      null
    }
  }

  def transformStatement (x : Any) : JStatement = {
    //Console.println("transformstatement " + x)
    x match {
      case (x : PrimaryExpr)~(y : List[Any]) =>
        val rx = transformExpression(x)
        val rest = unpackR(y).drop(1)
        rx match {
          case JVariableAccess(y) =>
            if (y == "this")
              if (ClassTable.getFields(classid).contains(rest))
                JFieldAccess(rx, rest)
              else {
                Console.println("dunno what you mean with this (not a field): " + rx + " rest " + rest)
                null
              }
            else {
              Console.println("access to non-this field (unchecked): " + rx + " fieldname " + rest)
              JFieldAccess(rx, rest)
            }
          case y =>
            //now we have to lookup type of rx, and look whether rest is a field inside there
            Console.println("unsafe field access (maybe non-existant?): " + rx + " rest " + rest)
            JFieldAccess(rx, rest)
        }
      case Expr(x) => transformStatement(x)
      case PostFixExpression(x) => transformStatement(x)
      case AnyStatement(x) => transformStatement(x)
      case PrimaryExpr(x) => transformStatement(x)
      case ParExpr(x) => transformStatement(x)
      case Some(x) => transformStatement(x)
      case "this" => JVariableAccess("this")
      case PostExpr(k, x) => JPostfixExpression(unpackR(k), transformExpression(x))
      case UnaryExpr(op, v) => JUnaryExpression(unpackR(op), transformExpression(v))
      case NewExpression(ntype, args) => JNewExpression(unpackR(ntype), args.map(transformExpression))
      case Conditional(test, consequence, alternative) =>
        val cs = transformBlock(transformStatement(consequence))
        val alt = alternative match {
          case None => JBlock(List[JBodyStatement]())
          case Some(x) => transformBlock(transformStatement(x))
        }
        JConditional(transformExpression(test), cs, alt)
      case While(test, body) =>
        JWhile(transformExpression(test), transformBlock(transformStatement(body)))
      case BinaryExpr(op, le, ri) =>
        val oper = unpackR(op)
        if (oper == "=") { //assign -- but there are other assignments (+=, -=, ++,...)
          val lef = transformStatement(le) //will be either FieldAccess or VariableAccess
          lef match {
            case JFieldAccess(cnx, nam) => JFieldWrite(cnx, nam, transformExpression(ri))
            case JVariableAccess(nam) => JAssignment(nam, transformExpression(ri))
          }
        } else
          JBinaryExpression(oper, transformExpression(le), transformExpression(ri))
      case Call(QualId(fun), args) =>
        //Console.println("fun of call is " + fun)
        val funs = fun.map(unpackR)
        val (varia, rst) =
          if (funs.length == 1)
            ("this", funs)
          else
            (funs(0), funs.drop(1))
        JCall(varia, rst.reduceLeft(_ + "." + _), args.map(transformExpression)) //XXX: recurse
      case Return(x) => JReturn(transformExpression(x))
      case QualId(x) =>
        //Console.println("inspecting qualid: " + x)
        val vs = x.map(unpackR)
        if (vs.length == 1) {
          val v = vs(0)
          if (lvars.contains(v))
            JVariableAccess(v)
          else if (ClassTable.getFields(classid).contains(v))
            JFieldAccess(JVariableAccess("this"), v)
          else {
            Console.println("got a qualid which isn't in lvars or fields " + vs + " (fields: " + ClassTable.getFields(classid) + ")(lvar: " + lvars + ")")
            JVariableAccess(vs.reduceLeft(_ + "." + _)) //XXX: recurse
          }
        } else
          //XXX: recurse
          JFieldAccess(JVariableAccess(vs.dropRight(1).reduceLeft(_ + "." + _)), vs.takeRight(1)(0))
      case Lit(x) => JLiteral(unpackR(x))
      case Block(xs) =>
        val res = walk(xs)
        if (res.forall(_.isInstanceOf[JBodyStatement]))
          JBlock(res.map(_.asInstanceOf[JBodyStatement]))
        else {
          Console.println("transform: not valid block elements: " + res)
          null
        }
      case y => Console.println("transformStatement dunno " + x); null
    }
  }

  def transform (x : Any) : Option[JStatement] = {
    //Console.println("transform " + x)
    x match {
      case Modifier(_) => None
      case None => None
      case Some(x) => transform(x)
      case LocalVar(x) => x match {
        case (mod~jtype)~(decls : List[Any]) =>
          val typ = unpackR(jtype)
          val vars = decls.map(_ match {
            case (name : Name)~b~Some(y) =>
              //Console.println("WORKING on lv name " + name + " init is " + y)
              val init = transformExpression(y)
              val realn = unpackR(name)
              lvars += realn -> typ
              //Console.println("lv " + realn + " init is " + init)
              Some(JBinding(realn, typ, Some(init)))
            case (name : Name)~b~None =>
              val realn = unpackR(name)
              lvars += realn -> typ
              Some(JBinding(realn, typ, None))
            case x => Console.println("dunno about decl " + x); None
          })
          assert(vars.length == 1)
          vars(0)
        case y => Console.println("dunno about lvar " + y); None
      }
      case FormalVariable(mods, jtype, name) => Some(JArgument(unpackR(name), unpackR(jtype)))
      case FieldDeclaration(id, jtype, rest) =>
        val name = unpackR(id)
        val typ = unpackR(jtype)
        ClassTable.addField(classid, name, typ)
        Some(JFieldDefinition(name, typ))
      case ConstructorDeclaration(id, parameters, throws, body) =>
        lvars = new HashMap[String,String]()
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => lvars += x.id -> x.jtype }
        //Console.println("walking over body " + body)
        val realbody = walk(body)
        //Console.println("walked over body " + realbody)
        val realrealbody : List[JBodyStatement] =
          if (realbody.forall(_.isInstanceOf[JBodyStatement]))
            realbody.map(_.asInstanceOf[JBodyStatement])
          else {
            Console.println("cannot convert constructor body " + realbody)
            List[JBodyStatement]()
          }
        //Console.println("MethodDeclaration done, lvars " + lvars)
        ClassTable.addMethod(classid, "<init>", unpackR(id), lvars)
        Some(JMethodDefinition("<init>", args, realrealbody))        
      case MethodDeclaration(id, jtype, parameters, throws, body) =>
        lvars = new HashMap[String,String]()
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => lvars += x.id -> x.jtype }
        //Console.println("walking over body " + body)
        val realbody = walk(body)
        //Console.println("walked over body " + realbody)
        val realrealbody : List[JBodyStatement] =
          if (realbody.forall(_.isInstanceOf[JBodyStatement]))
            realbody.map(_.asInstanceOf[JBodyStatement])
          else {
            Console.println("cannot convert method body " + realbody)
            List[JBodyStatement]()
          }
        //Console.println("MethodDeclaration done, lvars " + lvars) 
        val name = unpackR(id)
        val typ = unpackR(jtype)
        ClassTable.addMethod(classid, name, typ, lvars)
        Some(JMethodDefinition(name, args, realrealbody))
      case JInterface(id, jtype, interfaces, body) =>
        val is = transformOLF(interfaces)
        val myclass = unpackR(id)
        val outer = if (classid == "") None else Some(classid)
        classid = myclass
        ClassTable.registerClass(classid, outer, true)
        val mybody = walk(body)
        classid = ClassTable.getOuter(myclass) match { case None => ""; case Some(x) => x }
        Some(JInterfaceDefinition(myclass, is, mybody))
      case JClass(id, jtype, superclass, interfaces, bodyp) =>
        val is = transformOLF(interfaces)
        val cs = unpackR(superclass)
        val myclassid = unpackR(id)
        val outer = if (classid == "") None else Some(classid)
        classid = myclassid
        ClassTable.registerClass(classid, outer, false)
        val mybody = walk(bodyp)
        classid = ClassTable.getOuter(myclassid) match { case None => ""; case Some(x) => x }
        Some(JClassDefinition(myclassid, cs, is, mybody, outer))
      case Expr(y) => Some(transformStatement(y))
      case (x : Conditional) => Some(transformStatement(x))
      case ";" => None
      case x => 
        val r = transformStatement(x)
        if (r == null) {
          Console.println("transform: dunno about [" + x.asInstanceOf[AnyRef].getClass.toString + "] " + x)
          None
        } else
          Some(r)
    }
  }

  def walk (statements : Any) : List[JStatement] = {
    statements match {
      case Nil => List[JStatement]()
      case x ~ y => walk(x) ++ walk(y)
      case x :: xs => walk(x) ++ walk(xs)
      case x =>
        val res = transform(x)
        //Console.println("walk return value (for " + x + ") is " + res)
        res match { case None => List[JStatement](); case Some(x) => List(x) }
    }
  }

  def doit (a : Any, out : PrintWriter) = {
    //Console.println("walking over " + a)
    val x = walk(a)
    //Console.println("doit, walked " + x)
    var innerclasses : List[JClassDefinition] = List[JClassDefinition]()
    val convert = (x : List[JStatement]) =>
      x.map(y => y match {
        case JClassDefinition(name, supers, interf, body, outer) => 
          val nb = body.flatMap(z => z match {
            case (x : JClassDefinition) =>
              innerclasses ::= x; None
            case JMethodDefinition(name, params, body) =>
              //Console.println("body: " + body)
              val tb = body.foldLeft(List[JBodyStatement]())((b,a) => b ++ extractCalls(a))
              //Console.println("body translated: " + tb)
              Some(JMethodDefinition(name, params, tb))
            case x => Some(x)
          })
          JClassDefinition(name, supers, interf, nb, outer)
        case x => x
      })
    val main = convert(x)
    var workset : List[JClassDefinition] = List[JClassDefinition]()
    while (innerclasses.length > 0) {
      val ci = innerclasses
      innerclasses = List[JClassDefinition]()
      workset = ci.map(i => convert(List(i)).head.asInstanceOf[JClassDefinition]) ++ workset
    }
    //Console.println("outputting " + main + " inners " + workset)
    coqoutput(workset ++ main, out)
  }
}

trait JavaToSimpleJava extends JavaStatements {

  def extractCalls (statement : JBodyStatement) : List[JBodyStatement] = {
    //Console.println("calling extract for " + statement)
    statement match {
      case JBlock(xs) => List(JBlock(xs.foldLeft(List[JBodyStatement]())((b, a) => b ++ extractCalls(a))))
      case JAssignment(x, r) =>
        val (arg, ins) = extractHelper(r)
        ins ++ List(JAssignment(x, arg))
      case JBinding(n, t, i) =>
        //Console.println("extracting jbinding " + n + " type " + t + " init " + i)
        i match {
          case None => List(JBinding(n, t, i))
          case Some(x) =>
            val (ih, ins) = extractHelper(x)
            ins ++ List(JBinding(n, t, Some(ih)))
        }
      case JBinaryExpression(op, l, r) =>
        val (larg, lins) = extractHelper(l)
        val (rarg, rins) = extractHelper(r)
        lins ++ rins ++ List(JBinaryExpression(op, larg, rarg))
      case JUnaryExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        vis ++ List(JUnaryExpression(op, va))
      case JPostfixExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        val oper = op match {
          case "++" => "+"
          case "--" => "-"
          case x => Console.println("dunno about postfixop " + op); op
        }
        vis ++ List(JBinaryExpression(oper, va, JLiteral("1")))
      case JFieldWrite(x, f, v) =>
        val (a, i) = extractHelper(v)
        val (va, vi) = extractHelper(x)
        vi ++ i ++ List(JFieldWrite(va, f, a))
      case JConditional(t, c, a) =>
        val (ta, ti) = extractHelper(t)
        val con = extractCalls(c)
        val con2 = if (con.length == 1)
                     con(0)
                   else
                     JBlock(con)
        val r = extractCalls(a)
        val alt =
          if (r.length == 1)
            r(0)
          else
            JBlock(r)
        ti ++ List(JConditional(ta, con2, alt))
      case JCall(variable, name, args) => 
        val (ars, ins) = exL(args)
        //Console.println("results from extracthelper: " + ars + " ins " + ins)
        ins ++ List(JCall(variable, name, ars))
      case JNewExpression(name, arguments) =>
        val (ars, ins) = exL(arguments)
        ins ++ List(JNewExpression(name, ars))
      case JWhile(test, body) =>
        val (ta, ti) = extractHelper(test)
        //Console.println("old body for while is " + body)
        val newbody = extractCalls(body)
        assert(newbody.length == 1)
        val nn = newbody(0)
        assert(nn.isInstanceOf[JBlock])
        //Console.println("new body for while is " + nn)
        ti ++ List(JWhile(ta, nn.asInstanceOf[JBlock]))
      case JReturn(e : JVariableAccess) =>
        List(JReturn(e))
      case JReturn(exxx) =>
        val t = Gensym.newsym
        val (ra, ri) = extractHelper(exxx)
        val res : List[JBodyStatement] =
          ri ++ List(JAssignment(t, ra))
        res ++ List(JReturn(JVariableAccess(t)))
      case x => {
        Console.println("extract default case encountered " + x);
        List(x)
      }
    }
  }

  def exL (xs : List[JExpression]) : (List[JExpression], List[JBodyStatement]) = {
    val vals = xs.map(extractHelper)
    (vals.map(z => z._1), vals.map(z => z._2).flatten)
  }

  def extractHelper (x : JExpression) : (JExpression, List[JBodyStatement]) = {
    //Console.println("extracthelper called with " + xs + " acca " + acca + " acci " + acci)
    x match {
      case JBinaryExpression(op, l, r) =>
        val (ri, rii) = extractHelper(r)
        val (le, lei) = extractHelper(l)
        //Console.println("HELP le " + le + " ri " + ri + " is " + lei)
        (JBinaryExpression(op, le, ri), rii ++ lei)
      case JUnaryExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        (JUnaryExpression(op, va), vis)
      case JPostfixExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        val t = Gensym.newsym
        val oper = if (op == "++") "+" else if (op == "--") "-" else { Console.println("dunno postfix " + op); op }
        (JVariableAccess(t), JAssignment(t, va) :: List(JBinaryExpression(oper, va, JLiteral("1"))))
      case JFieldAccess(con, f) =>
        val t = Gensym.newsym
        val (a, i) = extractHelper(con)
        (JVariableAccess(t), i ++ List(JAssignment(t, JFieldAccess(a, f))))
      case JCall(variable, name, args) =>
        val (as, ins) = exL(args)
        //List[pair[JExpression,List[JBodyStatement]]]
        val t = Gensym.newsym
        (JVariableAccess(t), ins ++ List(JAssignment(t, JCall(variable, name, as))))
      case JNewExpression(name, args) =>
        val (as, ins) = exL(args)
        val t = Gensym.newsym
        (JVariableAccess(t), ins ++ List(JAssignment(t, JNewExpression(name, as))))
      case JConditional(test, c, a) =>
        val t = Gensym.newsym
        val (ta, ti) = extractHelper(test)
        val getb = (x : JBodyStatement) => {
          //Console.println("extracting conditional, body is " + x)
          assert(x.isInstanceOf[JBlock])
          val ps = x.asInstanceOf[JBlock].body
          ps.foreach(x => assert(x.isInstanceOf[JExpression]))
          val bs = ps.map(x => x.asInstanceOf[JExpression])
          val (ca, ci) = exL(bs)
          //Console.println("extracted ca: " + ca + "\nci: " + ci)
          val lastc = ca.takeRight(1)(0)
          if (ca.length == 1)
            JBlock(ci ++ List(JAssignment(t, lastc)))
          else
            JBlock(ci ++ ca.dropRight(1) ++ List(JAssignment(t, lastc)))
        }
        val newc = getb(c)
        val newa = getb(a)
        (JVariableAccess(t), ti ++ List(JConditional(ta, newc, newa)))
      case x => (x, List[JBodyStatement]())
    }
  }
}

trait CoqOutputter extends JavaToSimpleJava with JavaStatements {
  import scala.collection.mutable.HashMap

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
        val typ = ClassTable.getType(ClassTable.getLocalVar(myclass, mymethod, callpref), funname)
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
        "(cwrite \"" + v + "\" \"" + f + "\" " + printStatement(n) + ")"
      case JAssignment(name, value) => "(cassign \"" + name + "\" " + printStatement(value) + ")"
      case JBinaryExpression(op, l, r) =>
        "(" + translateOp(op) + " " + printStatement(l) + " " + printStatement(r) + ")"
      case JUnaryExpression(op, v) =>
        "(" + translateOp(op) + " " + printStatement(v) + ")"
      case JLiteral(x) => "\"" + x + "\""
      case JVariableAccess(x) => "\"" + x + "\""
      case JCall(v, fun, arg) => "(ccall \"ignored\" \"" + v + "\" \"" + fun + "\" (" + arg.map(printStatement).foldRight("nil")(_ + " :: " + _) + ") (TClass \"" + "XXXtypeXXX" + "\"))"
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
