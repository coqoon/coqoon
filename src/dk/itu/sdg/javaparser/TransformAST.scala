/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

object ClassTable {
  import scala.collection.mutable.HashMap
  val classtable = new HashMap[String,(Boolean, Option[String], HashMap[String,String], HashMap[String,(String,HashMap[String,String],HashMap[String,String])])]()
  //Class -> (Interface?, Outer, FieldName -> Type, MethodName -> (Returntype, LocalVar -> Type, argument -> type)

  import scala.collection.mutable.ListBuffer
  val spectable = new HashMap[String, (HashMap[String,(String,String)], HashMap[String,ListBuffer[String]])]()
  //class -> (method -> (pre, post), module -> coq)
  val gspecs = new HashMap[String, ListBuffer[String]]()
  //TOP -> topcoq; PRELUDE -> preludecoq

  def empty () : Unit = {
    classtable.clear
    spectable.clear
    gspecs.clear
    gspecs += "TOP" -> new ListBuffer[String]()
    gspecs += "PRELUDE" -> new ListBuffer[String]()
  }
  
  def registerClass (id : String, outer : Option[String], interface : Boolean) = {
    assert(! classtable.contains(id))
    classtable += id -> (interface, outer, new HashMap[String,String](), new HashMap[String,(String,HashMap[String,String],HashMap[String,String])]())
    spectable += id -> (new HashMap[String,(String,String)](), new HashMap[String,ListBuffer[String]]())
  }

  def checkkey (id : String, key : String) = {
    assert(! classtable(id)._3.contains(key))
    assert(! classtable(id)._4.contains(key))
  }

  def addField (id : String, key : String, value : String) = {
    checkkey(id, key)
    classtable(id)._3 += key -> value
  }

  def addLocals (id : String, method : String, lvars : HashMap[String,String]) {
    classtable(id)._4(method)._2 ++= lvars
  }

  def addLocal (id : String, method : String, lvar : String, typ : String) {
    classtable(id)._4(method)._2 += lvar -> typ
  }

  def addMethod (id : String, key : String, value : String, args : HashMap[String,String]) = {
    checkkey(id, key)
    classtable(id)._4 += key -> (value, new HashMap[String,String](), args)
    spectable(id)._1 += key -> (null, null)
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

  import java.lang.{Class,ClassNotFoundException}
  import java.lang.reflect._

  def getJClass (name : String) : Class[_] = {
    name match {
      case "byte" => java.lang.Byte.TYPE
      case "short" => java.lang.Short.TYPE
      case "int" => java.lang.Integer.TYPE
      case "long" => java.lang.Long.TYPE
      case "float" => java.lang.Float.TYPE
      case "double" => java.lang.Double.TYPE
      case "boolean" => java.lang.Boolean.TYPE
      case "char" => java.lang.Character.TYPE
      case x => try Class.forName(name) catch {
        case e : ClassNotFoundException =>
          try Class.forName("java.lang." + name) catch {
            case e : ClassNotFoundException =>
              try Class.forName("java.util." + name) catch {
                case e =>
                  Console.println("Can't find class for " + name)
                  null
              }
          }
      }
    }
  }

  def getMethodType (classname : String, methodname : String, variable : String, mname: String) : String = {
    //class and methodname are the scope, whereas variable is the local var on which mname is called
    val mclass = getLocalVar(classname, methodname, variable)
    if (classtable.contains(mclass))
      classtable(mclass)._4(mname)._1
    else
      getJClass(mclass).getMethod(mname).getReturnType.getName
  }

  def getFieldType (classname : String, fieldname : String) : String = {
    if (classtable.contains(classname))
      classtable(classname)._3(fieldname)
    else
      getJClass(classname).getField(fieldname).getType.getName
  }

  def getLocalVar(id : String, method : String, name : String) : String = {
    if (name == "this")
      id
    else
      if (classtable(id)._4(method)._2.contains(name)) //local variable
        classtable(id)._4(method)._2(name)
      else if (classtable(id)._4(method)._3.contains(name)) //argument
        classtable(id)._4(method)._3(name)
      else if (classtable(id)._3.contains(name)) //field
        classtable(id)._3(name)
      //XXX: (static?) field of outer class
      else {
        Console.println("assuming static class " + name + ", since I couldn't find id " + id + " m " + method + " n " + name + " in CT")
        name
      }
  }

  def setPrecondition (id : String, method : String, precon : String) : Unit = {
    assert(spectable(id)._1(method)._1 == null)
    spectable(id)._1(method) = (precon, spectable(id)._1(method)._2)
  }

  def setPostcondition (id : String, method : String, postcon : String) : Unit = {
    assert(spectable(id)._1(method)._2 == null)
    spectable(id)._1(method) = (spectable(id)._1(method)._1, postcon)
  }

  def addCoq (id : String, module : String, data : String) : Unit = {
    if (module == "PRELUDE" || module == "TOP")
      gspecs(module).append(data)
    else {
      if (! spectable(id)._2.contains(module))
        spectable(id)._2(module) = new ListBuffer[String]()
      val lb = spectable(id)._2(module)
      lb.append(data)
    }
  }

  def getSpecs (id : String) : HashMap[String,(String,String)] = {
    val res = new HashMap[String,(String,String)]()
    val ctc = spectable(id)._1
    ctc.keys.foreach { x => res += x -> (ctc(x)._1, ctc(x)._2) }
    res
  }

  def getCoq (id : String, module : String) : List[String] = {
    if (spectable(id)._2.contains(module))
      spectable(id)._2(module).toList.reverse
    else
      List()
  }

  def getCoq (module : String) : List[String] = {
    gspecs(module).toList.reverse
  }

  def getArguments (id : String, method : String) : HashMap[String,String] = {
    classtable(id)._4(method)._3
  }

  def isInterface (name : String) : Boolean = {
    classtable(name)._1
  }

  def getClasses () : List[String] = {
    classtable.keys.filterNot(isInterface).toList.filterNot(_ == "Coq")
  }

  def getInterfaces () : List[String] = {
    classtable.keys.filter(isInterface).toList
  }
}

import scala.util.parsing.combinator.Parsers
object FinishAST extends JavaTerms with Parsers with JavaToSimpleJava with CoqOutputter {
  import scala.collection.mutable.HashMap

  private var classid : String = ""
  private var methodid : String = ""
  private var lvars : HashMap[String,String] = new HashMap[String,String]()
  private var argmap : HashMap[String,String] = new HashMap[String,String]()

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

  def transformBlock (cs : Option[JStatement]) : JBlock = {
    cs match {
      case Some(x : JBlock) => x
      case Some(xs : JBodyStatement) => JBlock(List(xs))
      case x => Console.println("transformBlock, dunno about " + x); null
    }
  }

  def transformExpression (x : Any) : JExpression = {
    val y = transformStatement(x)
    y match {
      case Some(e : JExpression) => e
      case _ =>
        Console.println("transformExpression: wanted an JExpression, got " + y);
        null
    }
  }

  def transformStatement (x : Any) : Option[JStatement] = {
    //Console.println("transformstatement " + x)
    x match {
      case (x : PrimaryExpr)~(y : List[Any]) =>
        val rx = transformExpression(x)
        val rest = unpackR(y).drop(1)
        rx match {
          case JVariableAccess(y) =>
            if (y == "this")
              if (ClassTable.getFields(classid).contains(rest))
                Some(JFieldAccess(rx, rest))
              else {
                Console.println("dunno what you mean with this (not a field): " + rx + " rest " + rest)
                None
              }
            else {
              Console.println("access to non-this field (unchecked): " + rx + " fieldname " + rest)
              Some(JFieldAccess(rx, rest))
            }
          case y =>
            //now we have to lookup type of rx, and look whether rest is a field inside there
            Console.println("unsafe field access (maybe non-existant?): " + rx + " rest " + rest)
            Some(JFieldAccess(rx, rest))
        }
      case Expr(x) => transformStatement(x)
      case PostFixExpression(x) => transformStatement(x)
      case AnyStatement(x) => transformStatement(x)
      case PrimaryExpr(x) => transformStatement(x)
      case ParExpr(x) => transformStatement(x)
      case Some(x) => transformStatement(x)
      case "this" => Some(JVariableAccess("this"))
      case PostExpr(k, x) => Some(JPostfixExpression(unpackR(k), transformExpression(x)))
      case UnaryExpr(op, v) => Some(JUnaryExpression(unpackR(op), transformExpression(v)))
      case NewExpression(ntype, args) => Some(JNewExpression(unpackR(ntype), args.map(transformExpression)))
      case Conditional(test, consequence, alternative) =>
        val cs = transformBlock(transformStatement(consequence))
        val alt = alternative match {
          case None => JBlock(List[JBodyStatement]())
          case Some(x) => transformBlock(transformStatement(x))
        }
        Some(JConditional(transformExpression(test), cs, alt))
      case While(test, body) =>
        Some(JWhile(transformExpression(test), transformBlock(transformStatement(body))))
      case BinaryExpr(op, le, ri) =>
        val oper = unpackR(op)
        if (oper == "=") { //assign -- but there are other assignments (+=, -=, ++,...)
          val lef = transformStatement(le) //will be either FieldAccess or VariableAccess
          lef match {
            case Some(JFieldAccess(cnx, nam)) => Some(JFieldWrite(cnx, nam, transformExpression(ri)))
            case Some(JVariableAccess(nam)) => Some(JAssignment(nam, transformExpression(ri)))
            case x => Console.println("dunno about left hand side " + x); None
          }
        } else
          Some(JBinaryExpression(oper, transformExpression(le), transformExpression(ri)))
      case Call(QualId(fun), args) =>
        //Console.println("fun of call is " + fun)
        val funs = fun.map(unpackR)
        val (varia, rst) =
          if (funs.length == 1)
            ("this", funs)
          else
            (funs(0), funs.drop(1))
        if (varia == "Coq") {
          assert(rst.length == 1)
          if (rst(0) == "requires" || rst(0) == "ensures") {
            assert(args.length == 1)
            val arg = exString(transformExpression(args(0)))
            if (rst(0) == "requires")
              ClassTable.setPrecondition(classid, methodid, arg)
            else if (rst(0) == "ensures")
              ClassTable.setPostcondition(classid, methodid, arg)
          } else {
            assert(rst(0) == "def")
            assert(args.length == 2)
            val mod : JFieldAccess = transformExpression(args(0)).asInstanceOf[JFieldAccess] //Coq.M.FOO -> JFieldAccess("Coq", "M.FOO")
            val module = mod.field //get rid of "Coq.M."
            val arg = exString(transformExpression(args(1)))
            ClassTable.addCoq(classid, module, arg)
          }
          None
        } else
          Some(JCall(varia, rst.reduceLeft(_ + "." + _), args.map(transformExpression))) //XXX: recurse
      case Return(x) => Some(JReturn(transformExpression(x)))
      case QualId(x) =>
        //Console.println("inspecting qualid: " + x)
        val vs = x.map(unpackR)
        if (vs.length == 1) {
          val v = vs(0)
          if (lvars.contains(v) | argmap.contains(v))
            Some(JVariableAccess(v))
          else if (ClassTable.getFields(classid).contains(v))
            Some(JFieldAccess(JVariableAccess("this"), v))
          else {
            Console.println("got a qualid which isn't in lvars or fields " + vs + " (fields: " + ClassTable.getFields(classid) + ")(lvar: " + lvars + ", args: " + argmap + ")")
            Some(JVariableAccess(vs.reduceLeft(_ + "." + _))) //XXX: recurse
          }
        } else
          //XXX: recurse
          Some(JFieldAccess(JVariableAccess(vs.dropRight(1).reduceLeft(_ + "." + _)), vs.takeRight(1)(0)))
      case Lit(x) => Some(JLiteral(unpackR(x)))
      case Block(xs) =>
        val res = walk(xs)
        if (res.forall(_.isInstanceOf[JBodyStatement]))
          Some(JBlock(res.map(_.asInstanceOf[JBodyStatement])))
        else {
          Console.println("transform: not valid block elements: " + res)
          None
        }
      case y => Console.println("transformStatement dunno " + x); None
    }
  }

  def exString (x : JExpression) : String =
    x match {
      case JLiteral(s) => s
      case JBinaryExpression("+", l, r) => exString(l) + "\n" + exString(r)
      case y => Console.println("dunno how to extract from " + y); ""
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
        Console.println("field pos info " + x.asInstanceOf[FieldDeclaration].pos)
        val name = unpackR(id)
        val typ = unpackR(jtype)
        ClassTable.addField(classid, name, typ)
        Some(JFieldDefinition(name, typ))
      case ConstructorDeclaration(id, parameters, throws, body) =>
        Console.println("constructor pos info: " + x.asInstanceOf[ConstructorDeclaration].pos)
        lvars = new HashMap[String,String]()
        argmap = new HashMap[String,String]()
        methodid = "init_"
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => argmap += x.id -> x.jtype }
        ClassTable.addMethod(classid, methodid, unpackR(id), argmap)
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
        ClassTable.addLocals(classid, methodid, lvars)
        Some(JMethodDefinition(methodid, classid, args, realrealbody))        
      case MethodDeclaration(id, jtype, parameters, throws, body) =>
        Console.println("method pos info: " + x.asInstanceOf[MethodDeclaration].pos)
        lvars = new HashMap[String,String]()
        argmap = new HashMap[String,String]()
        val name = unpackR(id)
        methodid = name
        val typ = unpackR(jtype)
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => argmap += x.id -> x.jtype }
        ClassTable.addMethod(classid, name, typ, argmap)
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
        ClassTable.addLocals(classid, name, lvars)
        Some(JMethodDefinition(name, typ, args, realrealbody))
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
        Console.println("transforming a JClass, ranging (pos) " + x.asInstanceOf[JClass].pos)
        val is = transformOLF(interfaces)
        val cs = unpackR(superclass)
        val myclassid = unpackR(id)
        val outer = if (classid == "") None else Some(classid)
        classid = myclassid
        ClassTable.registerClass(classid, outer, false)
        val mybody = walk(bodyp)
        classid = ClassTable.getOuter(myclassid) match { case None => ""; case Some(x) => x }
        Some(JClassDefinition(myclassid, cs, is, mybody, outer))
      case Expr(y) => transformStatement(y)
      case (x : Conditional) => transformStatement(x)
      case ";" => None
      case x => 
        val r = transformStatement(x)
        r match {
          case None =>
            Console.println("transform: dunno about [" + x.asInstanceOf[AnyRef].getClass.toString + "] " + x)
            None
          case Some(x) => r
        }
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
  
  def doitHelper (a : Any) : List[JStatement] = {
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
            case (x : JMethodDefinition) =>
              Some(translate(name, x))
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
    //Console.println("outputting Class in Java\n" + JavaOutputter.out(main(1)))
    //Console.println("outputting " + main + " inners " + workset)
    workset ++ main
  }

  def doit (a : Any) : String = {
    val w = doitHelper(a)
    coqoutput(w).reduceLeft(_ + "\n" + _)
    //workset ++ main
  }
}
