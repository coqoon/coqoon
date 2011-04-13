/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import dk.itu.sdg.util.{ KopitiamLogger }

object ClassTable extends KopitiamLogger {
  import scala.collection.mutable.HashMap
  val classtable = new HashMap[String,(Boolean, Option[String], HashMap[String,String], HashMap[String,(String,HashMap[String,String],HashMap[String,String])])]()
  //Class -> (Interface?, Outer, FieldName -> Type, MethodName -> (Returntype, LocalVar -> Type, argument -> type)
  val modtable = new HashMap[(String,String),List[String]]()
  //(Class, id) -> mods
  def setModifiers (classid : String, id : String, mod : List[String]) = {
    modtable += (classid, id) -> mod
  }

  def getModifiers (classid : String, id : String) : List[String] = {
    if (modtable.contains((classid, id)))
      modtable((classid, id))
    else
      List[String]()
  }

  import scala.collection.mutable.ListBuffer
  val spectable = new HashMap[String, (HashMap[String,(String,String)], HashMap[String,ListBuffer[String]])]()
  //class -> (method -> (pre, post), module -> coq)
  val gspecs = new HashMap[String, ListBuffer[String]]()
  //TOP -> topcoq; PRELUDE -> preludecoq

  val interfaceToFunctions = new HashMap[String,(List[String],List[String])]()

  def empty () : Unit = {
    classtable.clear
    spectable.clear
    modtable.clear
    interfaceToFunctions.clear
    gspecs.clear
    gspecs += "TOP" -> new ListBuffer[String]()
    gspecs += "PRELUDE" -> new ListBuffer[String]()
  }

  def registerInterfaceFunctions (id : String, ms : List[String], funs : List[String]) = {
    assert(! interfaceToFunctions.contains(id))
    interfaceToFunctions += id -> (ms, funs)
  }

  def interfaceFunctions (id : String) : (List[String], List[String]) = {
    assert(interfaceToFunctions.contains(id))
    interfaceToFunctions(id)
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
      log.warning("CT doesn't contain " + classname)
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
                  if (name.contains("<")) //poor Java, no generics at RT
                    getJClass(name.substring(0, name.indexOf("<")))
                  else {
                    log.warning("Can't find class for " + name)
                    getJClass("Object")
                  }
              }
          }
      }
    }
  }

  def getMethodType (classname : String, methodname : String, variable : String, mname : String, args : List[String]) : String = {
    //class and methodname are the scope, whereas variable is the local var on which mname is called
    val mclass = getLocalVar(classname, methodname, variable)
    if (classtable.contains(mclass))
      classtable(mclass)._4(mname)._1
    else {
      val meth = getJClass(mclass).getMethod(mname, args.map(getJClass) : _*)
      if (mclass.contains("<")) {
        val ty = mclass.substring(mclass.indexOf("<") + 1, mclass.lastIndexOf(">"))
        assert(ty.contains(",") == false) //not multiple TVs
        val tvs = meth.getDeclaringClass.getTypeParameters
        assert(tvs.length == 1)
        if (meth.getGenericReturnType.toString == tvs(0).toString)
          ty
        else {
          log.warning("dunno how to deal with type parameters in here: " + meth.getGenericReturnType.toString)
          meth.getGenericReturnType.toString
        }
      } else
        meth.getReturnType.getName
    }
  }

  def isMethodStatic (classname : String, methodname : String, args : List[String]) : Boolean = {
    if (classtable.contains(classname)) {
      log.info("checking static of " + getModifiers(classname, methodname))
      getModifiers(classname, methodname).contains("static")
    } else
      Modifier.isStatic(getJClass(classname).getMethod(methodname, args.map(getJClass) : _*).getModifiers)
  }

  def getFieldType (classname : String, fieldname : String) : String = {
    log.info("getfieldtype of " + fieldname + " in class " + classname)
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
        log.warning("assuming static class " + name + ", since I couldn't find id " + id + " method " + method + " name " + name + " in CT")
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
object FinishAST extends JavaTerms
                    with Parsers
                    with JavaToSimpleJava
                    with CoqOutputter
                    with KopitiamLogger{
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
      case Modifier(x) => unpackR(x)
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
      case x => log.warning("wanted a string, but got " + x + " class: " + x.asInstanceOf[AnyRef].getClass.getName); ""
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
      case Some(y) => log.info("transformOL dunno " + y); List[T]()
    }
  }

  def transformList (xs : List[Any]) : List[JStatement] = {
    xs.map(transform).flatten
  }

  def transformBlock (cs : Option[JStatement]) : JBlock = {
    cs match {
      case Some(x : JBlock) => x
      case Some(xs : JBodyStatement) => JBlock(List(xs))
      case x => log.info("transformBlock, dunno about " + x); null
    }
  }

  def transformExpression (x : Any) : JExpression = {
    val y = transformStatement(x)
    y match {
      case Some(e : JExpression) => e
      case _ =>
        log.warning("transformExpression: wanted an JExpression, got " + y);
        null
    }
  }

  def transformExpressions (x : Any) : List[JExpression] = {
    x match {
      case a~b => transformExpressions(a) ++ transformExpressions(b)
      case hd :: tl => transformExpressions(hd) ++ transformExpressions(tl)
      case a => List(transformExpression(a))
    }
  }

  def transformStatement (x : Any) : Option[JStatement] = {
    //log.info("transformstatement " + x)
    x match {
      case Name(x) => Some(JVariableAccess(x))
      case (x : PrimaryExpr)~(y : List[Any]) =>
        log.info("transformStatement with primary " + x + " and arguments " + y)
        val rx = transformExpression(x)
        log.info("  transformed primary is " + rx)
        val rest = unpackR(y).drop(1)
        log.info("  rest (unpacked arguments, dropped first ('.')) " + rest)
        rx match {
          case JVariableAccess(z) =>
            if (z == "this")
              if (ClassTable.getFields(classid).contains(rest))
                Some(JFieldAccess(rx, rest))
              else { //fields might be declared after use...
                //we've most likely a call!?
                log.warning("dunno what you mean with this (not a field): " + rx + " rest " + rest)
                //y consists of .~(Name(addCount)~List(Expr(Prim(Post(Prim(Call(Qual(List(Name(c), Name(
                //gather arguments from y
                val args = transformExpressions(y).filterNot(_ == null).drop(1) //drop methodname
                log.info(" Call, args are " + args)
                Some(JCall(z, rest, args))
              }
            else {
              log.info("access to non-this field (unchecked): " + rx + " fieldname " + rest)
              Some(JFieldAccess(rx, rest))
            }
          case y =>
            //now we have to lookup type of rx, and look whether rest is a field inside there
            log.warning("unsafe field access (maybe non-existant?): " + rx + " rest " + rest)
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
            case x => log.warning("dunno about left hand side " + x); None
          }
        } else
          Some(JBinaryExpression(oper, transformExpression(le), transformExpression(ri)))
      case Call(QualId(fun), args) =>
        //log.info("fun of call is " + fun)
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
        } else {
          if (rst.length > 1)
            log.info("should have recursed for methodname " + rst + "(in call:" + varia + ", arguments:" + args.map(transformExpression) + ")")
          Some(JCall(varia, rst.reduceLeft(_ + "." + _), args.map(transformExpression))) //XXX: recurse
        }
      case Return(x) => Some(JReturn(transformExpression(x)))
      case QualId(x) =>
        //log.info("inspecting qualid: " + x)
        val vs = x.map(unpackR)
        if (vs.length == 1) {
          val v = vs(0)
          if (lvars.contains(v) | argmap.contains(v))
            Some(JVariableAccess(v))
          else if (ClassTable.getFields(classid).contains(v))
            Some(JFieldAccess(JVariableAccess("this"), v))
          else {
            //might be defined later in the source as field
            //or might be a field of an outer class
            log.warning("got a qualid which isn't in lvars or fields " + vs + " (fields: " + ClassTable.getFields(classid) + ")(lvar: " + lvars + ", args: " + argmap + ")")
            Some(JVariableAccess(vs.reduceLeft(_ + "." + _))) //XXX: recurse
          }
        } else if (vs.length == 2) {
          //XXX: check whether typeof vs(0) has a field vs(1)!
          Some(JFieldAccess(JVariableAccess(vs(0)), vs(1)))
        } else {
          log.info("all I got was this qualid, I'll try to make a fieldaccess out of it " + vs + " field: " + vs.takeRight(1)(0))
          //XXX: recurse
          val pre = transformExpression(QualId(x.dropRight(1)))
          log.info("  managed to find out pre: " + pre)
          Some(JFieldAccess(pre, vs.takeRight(1)(0)))
        }
      case Lit(x) => Some(JLiteral(unpackR(x)))
      case Block(xs) =>
        val res = walk(xs)
        if (res.forall(_.isInstanceOf[JBodyStatement]))
          Some(JBlock(res.map(_.asInstanceOf[JBodyStatement])))
        else {
          log.warning("transform: not valid block elements: " + res)
          None
        }
      case y => log.warning("transformStatement dunno " + x); None
    }
  }

  def exString (x : JExpression) : String =
    x match {
      case JLiteral(s) => s
      case JBinaryExpression("+", l, r) => exString(l) + "\n" + exString(r)
      case y => log.warning("dunno how to extract from " + y); ""
    }

  def transform (x : Any) : Option[JStatement] = {
    //log.info("transform " + x)
    x match {
      case BodyDeclaration(mods, x) =>
        //log.info("bodydecl " + mods)
        val ted = transform(x)
        ted match {
          case Some(y) =>
            val mod = mods.map(unpackR)
            y match {
              case JFieldDefinition(id, jtype) => ClassTable.setModifiers(classid, id, mod)
              case JMethodDefinition(id, clasid, args, body) => ClassTable.setModifiers(classid, id, mod)
              case x => log.warning("unexpected " + x + " during transformation of a bodydecl")
            }
          case None =>
        }
        ted
      case Modifier(_) => None
      case None => None
      case Some(x) => transform(x)
      case LocalVar(x) => x match {
        case (mod~jtype)~(decls : List[Any]) =>
          val typ = unpackR(jtype)
          val vars = decls.map(_ match {
            case (name : Name)~b~Some(y) =>
              //log.info("WORKING on lv name " + name + " init is " + y)
              val init = transformExpression(y)
              val realn = unpackR(name)
              lvars += realn -> typ
              //log.info("lv " + realn + " init is " + init)
              Some(JBinding(realn, typ, Some(init)))
            case (name : Name)~b~None =>
              val realn = unpackR(name)
              lvars += realn -> typ
              Some(JBinding(realn, typ, None))
            case x => log.warning("dunno about decl " + x); None
          })
          assert(vars.length == 1)
          vars(0)
        case y => log.warning("dunno about lvar " + y); None
      }
      case FormalVariable(mods, jtype, name) => Some(JArgument(unpackR(name), unpackR(jtype)))
      case FieldDeclaration(id, jtype, rest) =>
        log.info("field pos info " + x.asInstanceOf[FieldDeclaration].pos)
        val name = unpackR(id)
        val typ = unpackR(jtype)
        ClassTable.addField(classid, name, typ)
        Some(JFieldDefinition(name, typ))
      case ConstructorDeclaration(id, parameters, throws, body) =>
        log.info("constructor pos info: " + x.asInstanceOf[ConstructorDeclaration].pos)
        lvars = new HashMap[String,String]()
        argmap = new HashMap[String,String]()
        methodid = "init_"
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => argmap += x.id -> x.jtype }
        ClassTable.addMethod(classid, methodid, unpackR(id), argmap)
        //log.info("walking over body " + body)
        val realbody = walk(body)
        //log.info("walked over body " + realbody)
        val realrealbody : List[JBodyStatement] =
          if (realbody.forall(_.isInstanceOf[JBodyStatement]))
            realbody.map(_.asInstanceOf[JBodyStatement])
          else {
            log.warning("cannot convert constructor body " + realbody)
            List[JBodyStatement]()
          }
        //log.info("MethodDeclaration done, lvars " + lvars)
        ClassTable.addLocals(classid, methodid, lvars)
        Some(JMethodDefinition(methodid, classid, args, realrealbody))
      case MethodDeclaration(id, jtype, parameters, throws, body) =>
        log.info("method pos info: " + x.asInstanceOf[MethodDeclaration].pos)
        lvars = new HashMap[String,String]()
        argmap = new HashMap[String,String]()
        val name = unpackR(id)
        methodid = name
        val typ = unpackR(jtype)
        val args = transformOL(parameters, List[JArgument]())
        args.foreach { x => argmap += x.id -> x.jtype }
        ClassTable.addMethod(classid, name, typ, argmap)
        //log.info("walking over body " + body)
        val realbody = walk(body)
        //log.info("walked over body " + realbody)
        val realrealbody : List[JBodyStatement] =
          if (realbody.forall(_.isInstanceOf[JBodyStatement]))
            realbody.map(_.asInstanceOf[JBodyStatement])
          else {
            log.warning("cannot convert method body " + realbody)
            List[JBodyStatement]()
          }
        //log.info("MethodDeclaration done, lvars " + lvars)
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
        log.info("transforming a JClass, ranging (pos) " + x.asInstanceOf[JClass].pos)
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
            log.warning("transform: dunno about [" + x.asInstanceOf[AnyRef].getClass.toString + "] " + x)
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
        //log.info("walk return value (for " + x + ") is " + res)
        res match { case None => List[JStatement](); case Some(x) => List(x) }
    }
  }

  def doitHelper (a : Any) : List[JStatement] = {
    //log.info("walking over " + a)
    classid = ""
    methodid = ""
    lvars = new HashMap[String,String]()
    argmap = new HashMap[String,String]()
    val x = walk(a)
    //log.info("doit, walked " + x)
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
    //log.info("outputting Class in Java\n" + JavaOutputter.out(main(1)))
    //log.info("outputting " + main + " inners " + workset)
    workset ++ main
  }

  def doit (a : Any, name : String) : String = {
    val w = doitHelper(a)
    coqoutput(w, true, name).reduceLeft(_ + "\n" + _)
    //workset ++ main
  }

  def doitNoSpec (a : Any, name : String) : (String, String) = {
    val w = doitHelper(a)
    val re = coqoutput(w, false, name)
    val prog = re.takeWhile(! _.contains("_spec.\nImport ")).reduceLeft(_ + "\n" + _)
    val spec = re.dropWhile(! _.contains("_spec.\nImport ")).drop(1).reduceLeft(_ + "\n" + _)
    (prog, spec)
  }
}
