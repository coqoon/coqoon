package dk.itu.sdg.javaparser

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
  import java.io.PrintWriter


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
