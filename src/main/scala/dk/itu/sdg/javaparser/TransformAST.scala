/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import dk.itu.sdg.util.{ KopitiamLogger }

object ClassTable extends KopitiamLogger {
  import scala.collection.mutable.HashMap
  val classtable = new HashMap[String, (Boolean, Option[String], HashMap[String, String], HashMap[String, (String, HashMap[String, String], HashMap[String, String])])]()
  //Class -> (Interface?, Outer, FieldName -> Type, MethodName -> (Returntype, LocalVar -> Type, argument -> type)
  val modtable = new HashMap[(String, String), List[String]]()
  //(Class, id) -> mods
  def setModifiers(classid : String, id : String, mod : List[String]) = {
    modtable += (classid, id) -> mod
  }

  def getModifiers(classid : String, id : String) : List[String] = {
    if (modtable.contains((classid, id)))
      modtable((classid, id))
    else
      List[String]()
  }

  import scala.collection.mutable.ListBuffer
  val spectable = new HashMap[String, (HashMap[String, (String, String)], HashMap[String, ListBuffer[String]])]()
  //class -> (method -> (pre, post), module -> coq)
  val gspecs = new HashMap[String, ListBuffer[String]]()
  //TOP -> topcoq; PRELUDE -> preludecoq

  val interfaceToFunctions = new HashMap[String, (List[String], List[String])]()

  def empty() : Unit = {
    classtable.clear
    spectable.clear
    modtable.clear
    interfaceToFunctions.clear
    gspecs.clear
    gspecs += "TOP" -> new ListBuffer[String]()
    gspecs += "PRELUDE" -> new ListBuffer[String]()
  }

  def registerInterfaceFunctions(id : String, ms : List[String], funs : List[String]) = {
    assert(!interfaceToFunctions.contains(id))
    interfaceToFunctions += id -> (ms, funs)
  }

  def interfaceFunctions(id : String) : (List[String], List[String]) = {
    assert(interfaceToFunctions.contains(id))
    interfaceToFunctions(id)
  }

  def registerClass(id : String, outer : Option[String], interface : Boolean) = {
    assert(!classtable.contains(id))
    classtable += id -> (interface, outer, new HashMap[String, String](), new HashMap[String, (String, HashMap[String, String], HashMap[String, String])]())
    spectable += id -> (new HashMap[String, (String, String)](), new HashMap[String, ListBuffer[String]]())
  }

  def checkkey(id : String, key : String) = {
    assert(!classtable(id)._3.contains(key))
    assert(!classtable(id)._4.contains(key))
  }

  def addField(id : String, key : String, value : String) = {
    checkkey(id, key)
    classtable(id)._3 += key -> value
  }

  def addLocals(id : String, method : String, lvars : HashMap[String, String]) {
    classtable(id)._4(method)._2 ++= lvars
  }

  def addLocal(id : String, method : String, lvar : String, typ : String) {
    classtable(id)._4(method)._2 += lvar -> typ
  }

  def addMethod(id : String, key : String, value : String, args : HashMap[String, String]) = {
    checkkey(id, key)
    classtable(id)._4 += key -> (value, new HashMap[String, String](), args)
    spectable(id)._1 += key -> (null, null)
  }

  def getOuter(id : String) : Option[String] = {
    if (classtable.contains(id))
      classtable(id)._2
    else
      None
  }

  def getFields(id : String) : HashMap[String, String] = {
    classtable(id)._3
  }

  def getType(classname : String, key : String) : String = {
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

  import java.lang.{ Class, ClassNotFoundException }
  import java.lang.reflect._

  def getJClass(name : String) : Class[_] = {
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

  def getMethodType(classname : String, methodname : String, variable : String, mname : String, args : List[String]) : String = {
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

  def isMethodStatic(classname : String, methodname : String, args : List[String]) : Boolean = {
	if (methodname == "new")
	  true
    else if (classtable.contains(classname)) {
      log.info("checking static of " + getModifiers(classname, methodname))
      getModifiers(classname, methodname).contains("static")
    } else
      Modifier.isStatic(getJClass(classname).getMethod(methodname, args.map(getJClass) : _*).getModifiers)
  }

  def getFieldType(classname : String, fieldname : String) : String = {
    log.info("getfieldtype of " + fieldname + " in class " + classname)
    if (classtable.contains(classname))
      classtable(classname)._3(fieldname)
    else
      getJClass(classname).getField(fieldname).getType.getName
  }

  def getLocalVar(id : String, method : String, name : String) : String = {
    if (name == "this")
      id
    else if (classtable(id)._4(method)._2.contains(name)) //local variable
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

  def setPrecondition(id : String, method : String, precon : String) : Unit = {
    assert(spectable(id)._1(method)._1 == null)
    spectable(id)._1(method) = (precon, spectable(id)._1(method)._2)
  }

  def setPostcondition(id : String, method : String, postcon : String) : Unit = {
    assert(spectable(id)._1(method)._2 == null)
    spectable(id)._1(method) = (spectable(id)._1(method)._1, postcon)
  }

  def addCoq(id : String, module : String, data : String) : Unit = {
    if (module == "PRELUDE" || module == "TOP")
      gspecs(module).append(data)
    else {
      if (!spectable(id)._2.contains(module))
        spectable(id)._2(module) = new ListBuffer[String]()
      val lb = spectable(id)._2(module)
      lb.append(data)
    }
  }

  def getSpecs(id : String) : HashMap[String, (String, String)] = {
    val res = new HashMap[String, (String, String)]()
    val ctc = spectable(id)._1
    ctc.keys.foreach { x => res += x -> (ctc(x)._1, ctc(x)._2) }
    res
  }

  def getCoq(id : String, module : String) : List[String] = {
    if (spectable(id)._2.contains(module))
      spectable(id)._2(module).toList.reverse
    else
      List()
  }

  def getCoq(module : String) : List[String] = {
    gspecs(module).toList.reverse
  }

  def getArguments(id : String, method : String) : HashMap[String, String] = {
    classtable(id)._4(method)._3
  }

  def isInterface(name : String) : Boolean = {
    classtable(name)._1
  }

  def getClasses() : List[String] = {
    classtable.keys.filterNot(isInterface).toList.filterNot(_ == "Coq")
  }

  def getInterfaces() : List[String] = {
    classtable.keys.filter(isInterface).toList
  }
}

import scala.util.parsing.combinator.Parsers

/*
 * Object that can transform an AST expressed in the first intermediate language
 * (JavaTerms) into an AST expressed in the second intermediate language (JavaAST).
 */
object FinishAST extends JavaTerms
                  with Parsers
                  with JavaToSimpleJava
                  with CoqOutputter
                  with KopitiamLogger {

  import scala.collection.mutable.HashMap

  private var classid  : String = ""
  private var methodid : String = ""
  private var lvars    : HashMap[String, String] = new HashMap[String, String]()
  private var argmap   : HashMap[String, String] = new HashMap[String, String]()

  /*
   *  ==============================
   *  The public api
   *  ==============================
   */

  def doitHelper(a : Any) : List[JStatement] = {

    classid  = ""
    methodid = ""
    lvars    = new HashMap[String, String]()
    argmap   = new HashMap[String, String]()

    // At this point we only expect classes or interfaces to be declared
    val x = transform(a)

    var innerclasses : List[JClassDefinition] = List[JClassDefinition]()
    val convert = (x : List[JStatement]) =>
      x.map(y => y match {
        case JClassDefinition(modifiers, name, supers, interf, body, outer) =>
          val nb = body.flatMap(z => z match {
            case (x : JClassDefinition) =>
              innerclasses ::= x; None
            case (x : JMethodDefinition) =>
              Some(translate(name, x))
            case x => Some(x)
          })
          JClassDefinition(modifiers, name, supers, interf, nb, outer)
        case x => x
      })
    val main = convert(x)
    var workset : List[JClassDefinition] = List[JClassDefinition]()
    while (innerclasses.length > 0) {
      val ci = innerclasses
      innerclasses = List[JClassDefinition]()
      workset = ci.map(i => convert(List(i)).head.asInstanceOf[JClassDefinition]) ++ workset
    }
    workset ++ main
  }

  def doit(a : Any, name : String) : String = {
    val w = doitHelper(a)
    coqoutput(w, true, name).reduceLeft(_ + "\n" + _)
    //workset ++ main
  }

  def doitNoSpec(a : Any, name : String) : (String, String) = {
    val w = doitHelper(a)
    val re = coqoutput(w, false, name)
    val prog = re.takeWhile(!_.contains("_spec.\nImport ")).reduceLeft(_ + "\n" + _)
    val spec = re.dropWhile(!_.contains("_spec.\nImport ")).drop(1).mkString("\n")
    (prog, spec)
  }

 /*
  *  ========================================================
  *  The methods that take care of the actual transformations
  *  ========================================================
  */

  /*
   * Method for unpacking some of the very nested structure where we're only
   * interested in the contents of the "wrappers"
   */
  def unpackR(r : Any) : String = {
    r match {
      case x ~ y                => unpackR(x) + unpackR(y)
      case QualId(xs)           => "\"" + xs.map(unpackR).reduceLeft(_ + "." + _) + "\"" //XXX: ???
      case Some(x)              => unpackR(x)
      case Modifier(x)          => unpackR(x)
      case Expr(x)              => unpackR(x)
      case PrimaryExpr(x)       => unpackR(x)
      case PostFixExpression(x) => unpackR(x)
      case Name(x)              => x
      case Num(x)               => x
      case Lit(x)               => unpackR(x)
      case Str(x)               => x
      case x :: rt              => unpackR(x) + unpackR(rt)
      case x : String           => x
      case Primitive(x)         => unpackR(x)
      case Key(x)               => unpackR(x)
      case Nil                  => ""
      case None                 => ""
      case x                    =>
        log.warning("wanted a string, but got " + x + " class: " + x.asInstanceOf[AnyRef].getClass.getName)
        ""
    }
  }

  /*
   * Main entry point for transforming the AST.
   */
  def transform( x : Any) : List[JStatement] = {
    x match {
      case Nil     => Nil
      case x ~ y   => transform(x) ++ transform(y)
      case x :: xs => transform(x) ++ transform(xs)
      case x       => {
        x match {
          case i : Import => Nil // we don't care about imports for now.
          case SomethingWithModifiers(modifiers, classOrInterface) => 
            val jmodifiers = modifiers.flatMap { case Modifier(Key(mod)) => JModifier(mod) }.toSet
            List(transformClassOrInterface(classOrInterface, jmodifiers))
          case other => 
            List(transformClassOrInterface(other))
        }
      }
    }
  }

  /*
   * Transform a JClass or JInterface to a JClassDefinition or JInterfaceDefinition
   * respectively. Anything else will throw an exception
   */
  def transformClassOrInterface(x : Any, modifiers: Set[JModifier] = Set[JModifier]()) : JStatement = {
    x match {
      case JClass(id, jtype, superclass, interfaces, bodyp) => {
        log.info("transforming a JClass, ranging (pos) " + x.asInstanceOf[JClass].pos)
        val is        = transformOLF(interfaces)
        val cs        = unpackR(superclass)
        val myclassid = unpackR(id)
        val outer     = if (classid == "") None else Some(classid)
        classid       = myclassid
        ClassTable.registerClass(classid, outer, false)
        val mybody    = transformClassOrInterfaceBody(bodyp)
        classid       = ClassTable.getOuter(myclassid) match { case None => ""; case Some(x) => x }
        JClassDefinition(modifiers, myclassid, cs, is, mybody, outer)
      }
      case JInterface(id, jtype, interfaces, body) => {
        val is      = transformOLF(interfaces)
        val myclass = unpackR(id)
        val outer   = if (classid == "") None else Some(classid)
        classid     = myclass
        ClassTable.registerClass(classid, outer, true)
        val mybody  = transformClassOrInterfaceBody(body)
        classid     = ClassTable.getOuter(myclass) match { case None => ""; case Some(x) => x }
        JInterfaceDefinition(/*accessModifier,*/ myclass, is, mybody)
      }
      case x => throw new Exception("Expected JInterface or JClass but got: " + x)
    }
  }

  /*
   * Transforms the body of a class or interface into a List of JStatements. This assumes
   * that a class or interface body will only contain:
   *
   * - methods
   * - constructors
   * - fields
   * - classes or interfaces
   *
   * If anything else is passed as an argument it will throw an exception
   *
   * NOTE: This method can't return InnterStatement because class & interfaces
   *       doesn't inherit from InnerStatement.
   */
  def transformClassOrInterfaceBody(body: List[Any]) : List[JStatement] = {

    // little helper function to take care of modifiers (Setting global state)
    def setModifier(modifiers: List[Modifier], declarations: List[JStatement]): Unit = {
      val mod = modifiers.map(unpackR)
      declarations foreach {
        case JFieldDefinition(id, jtype)               => ClassTable.setModifiers(classid, id, mod)
        case JMethodDefinition(id, clasid, args, body) => ClassTable.setModifiers(classid, id, mod)
        case _                                         => log.warning("Setting modifier of unkown") //TODO
      }
    }

    body flatMap { _ match {
      case BodyDeclaration(mods, x)              =>
        val transformed = transformClassOrInterfaceBody(List(x))
        setModifier(mods,transformed);
        transformed
      case jclass       : JClass                 => transformClassOrInterface(jclass)     :: Nil
      case jinterface   : JInterface             => transformClassOrInterface(jinterface) :: Nil
      case jmethod      : MethodDeclaration      => transformMethodDeclaration(jmethod)   :: Nil
      case jconstructor : ConstructorDeclaration => transformConstructor(jconstructor)    :: Nil
      case jfield       : FieldDeclaration       => transformFieldDeclaration(jfield)     :: Nil
      // things I would love the remove in some other way at some point
      case y ~ (x: MethodDeclaration)            => transformMethodDeclaration(x)         :: Nil
      case ";"                                   => Nil
      case x                                     => throw new Exception("Can't have the following in a class/interface body"+x)
    }}
  }

  /*
   * Transforms a MethodDeclaration into a JMethodDefinition
   */
  def transformMethodDeclaration(method: MethodDeclaration): JMethodDefinition = {
    val (mid, cid, args, body) = extractMethodOrConstructorInfo(method)
    JMethodDefinition(mid, unpackR(method.jtype), args, body)
  }

  /*
   * Transforms a ConstructorDeclaration into a JConstructorDefinition
   */
  def transformConstructor(constructor: ConstructorDeclaration): JConstructorDefinition = {
    val (_, cid, args, body) = extractMethodOrConstructorInfo(constructor)
    JConstructorDefinition(cid, args, body)
  }

  /*
   * Given a ConstructorDeclaration or MethodDeclaration it will extract the information needed
   * to be transformd into a JMethodDefinition. This will also add the methods and locals to the
   * global ClassTable.
   */
  def extractMethodOrConstructorInfo( term : Term ) : (String, String, List[JArgument], List[JBodyStatement]) = {

    val (id, mid, parameters, throws, body) = term match {
      case ConstructorDeclaration(id, parameters, throws, bdy)   => (unpackR(id), "new", parameters, throws, bdy)
      case MethodDeclaration(id, jtype, parameters, throws, bdy) => (unpackR(jtype), unpackR(id), parameters, throws, bdy)
    }

    methodid = mid
    lvars    = new HashMap[String, String]()
    argmap   = new HashMap[String, String]()

    val args = parameters.getOrElse(Nil).asInstanceOf[List[Any]].flatMap {
      case p : FormalVariable => Some(transformArgument(p))
      case _ => None
    }
    args.foreach { x => argmap += x.id -> x.jtype }
    ClassTable.addMethod(classid, methodid, id, argmap)

    val transformedBody = body match {
      case xs : List[Any] => transformMethodBody(xs)
      case b  : Block     => JBlock(transformMethodBody(b.xs)) :: Nil
      case None           => Nil
    }

    ClassTable.addLocals(classid, methodid, lvars)
    (methodid, classid, args, transformedBody)
  }

  /*
   * Transforms a FieldDeclaration into a JFieldDefinition
   */
  def transformFieldDeclaration(field: FieldDeclaration): JFieldDefinition = {
    val FieldDeclaration(id, jtype, rest) = field
    log.info("field pos info " + field.pos)
    val name = unpackR(id)
    val typ = unpackR(jtype)
    ClassTable.addField(classid, name, typ)
    JFieldDefinition(name, typ)
  }

  /*
   * Transforms the body of method into a list of JBodyStatement. This assumes that
   * a body only contains AnyExpr.
   */
  def transformMethodBody(body : List[Any]) : List[JBodyStatement] = {
    body map { case expr: AnyExpr => transformAnyExpr(expr) } flatten
  }

  /*
   * Transforms AnyExpr (or well, Any) to a List of JBodyStatements. It's a list because
   * some statements in JavaTerms may turn into multiple statements in JavaAST.
   *
   * TODO: Isn't it possible to NOT use Any here.
   */
  def transformAnyExpr( expr: Any) : List[JBodyStatement] = {
    expr match {
      // statements
      case AnyStatement(x)                     => transformAnyExpr(x)
      case variable: LocalVar                  => transformLocalVariable(variable)
      case Return(x)                           => JReturn(transformExpression(x))                                                :: Nil
      case While(test, body)                   => JWhile(transformExpression(test), transformBlock(transformAnyExpr(body).head)) :: Nil
      case Block(xs)                           => JBlock(xs map transformAnyExpr flatten)                                        :: Nil
      case cond: Conditional                   => transformConditional(cond)                                                     :: Nil
      // expressions
      case binaryExpr: BinaryExpr              => transformBinaryExpr(binaryExpr)                                                :: Nil
      case PrimaryExpr(x)                      => transformAnyExpr(x)
      case ParExpr(x)                          => transformAnyExpr(x)
      case "this"                              => JVariableAccess("this")                                                        :: Nil
      case PostExpr(k, x)                      => JPostfixExpression(unpackR(k), transformExpression(x))                         :: Nil
      case UnaryExpr(op, v)                    => JUnaryExpression(unpackR(op), transformExpression(v))                          :: Nil
      case NewExpression(ntype, args)          => JNewExpression(unpackR(ntype), args.map(transformExpression))                  :: Nil
      case Name(x)                             => JVariableAccess(x)                                                             :: Nil
      case Expr(x)                             => transformAnyExpr(x)
      case PostFixExpression(x)                => transformAnyExpr(x)
      case Lit(x)                              => JLiteral(unpackR(x))                                                           :: Nil
      case Some(x)                             => transformAnyExpr(x)
      // TODO: This is a bit hacky. transformCall is called twice IFF it is a JCall and thus has no sideeffect
      case c: Call if transformCall(c).isRight => transformCall(c).right.get                                                     :: Nil
      case qualid : QualId                     => transformQualId(qualid)                                                        :: Nil
      case (x : PrimaryExpr) ~ (y : List[Any]) => transformPrimaryExprFollowedByList(x,y)                                        :: Nil
      case ("." ~ expr)                        => transformAnyExpr(expr)
      // I have no idea what could come instead of None so it's best to crash it if something else comes up.  
      case (("assert" ~ x) ~ None)             => JAssert(transformAnyExpr(x).head)                                              :: Nil
    }
  }

  /*
   * Transforms a Conditional into a JConditional.
   */
  def transformConditional(cond: Conditional) : JBodyStatement = {
    val Conditional(test, consequence, alternative) = cond
    val cs = transformBlock(transformAnyExpr(consequence).head)
    val alt = alternative match {
      case None => JBlock(List[JBodyStatement]())
      case Some(x) => transformBlock(transformAnyExpr(x).head)
    }
    JConditional(transformExpression(test), cs, alt)
  }

  /*
   * Transforms a PrimaryExpr and a List of Any into a JBodyStatement. The List of Any is
   * the result from the parser. This happens when it parses a chained expression.
   */
  def transformPrimaryExprFollowedByList(x: PrimaryExpr, y: List[Any]): JBodyStatement = {

    /**
     * Dear reader,
     *
     * y.map(transformAnyExpr) will convert the parse-result into SimpleJavaAST however
     * because of lack of information Call(...) and Name(...) will be turned into JCall(this,_,_)
     * and JVariableAccess(...) - this is wrong as we want JFieldAccess and JCall(expr,name,args) to
     * be called on the result of the call and field accesses before it. To fix this we reverse the list
     * and use foldRgiht to build up the proper AST and do the transformations.
     *
     * /Mads
     */

    val x_ = transformExpression(x)
    val y_ = y.map(transformAnyExpr).flatten
    y_.reverse.foldRight(x_) { (cur,acc) =>
      cur match {
        case JVariableAccess(name) => JFieldAccess(acc,name)
        case JCall(_,name, args)   => JCall(acc, name, args)
      }
    }
  }

  /*
   * Transforms a BinaryExpr to a JBodyStatement
   */
  def transformBinaryExpr(expr: BinaryExpr) : JBodyStatement = {
    
    // Checks an operator as an assignemnt operator. 
    val isAssignmentOperator: String => Boolean = (operator: String) =>
      List("+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", "<<=", ">>=", ">>>=").contains(operator)
    
    val BinaryExpr(op, le, ri) = expr
    val oper = unpackR(op)
    oper match {
      case "=" => {
        val lef = transformAnyExpr(le).head //will be either FieldAccess or VariableAccess
        lef match {
          case JFieldAccess(cnx, nam) => JFieldWrite(cnx, nam, transformExpression(ri))
          case JVariableAccess(nam)   => JAssignment(nam, transformExpression(ri))
      }}
      case assignmentOperator if isAssignmentOperator(assignmentOperator) => {
        val binOp = assignmentOperator.replace("=","") // all assignment operators are just the normal operator with '=' appended
        transformAnyExpr(le).head match {
          case va @ JVariableAccess(name)  => JAssignment(name, JBinaryExpression(binOp, va ,transformExpression(ri)))
          case fa @ JFieldAccess(cnx, nam) => JFieldWrite(cnx, nam, JBinaryExpression(binOp, fa,transformExpression(ri)))
      }}
      case _ => JBinaryExpression(oper, transformExpression(le), transformExpression(ri))
    }
  }

  /*
   * Small helper method to check if a string is a varible or field access
   */
  def isFieldAccess(x: String) = ClassTable.getFields(classid).contains(x)

  /*
   * Transforms a Call into a Either[Unit, JBodyStatement]. We're using a disjoint set here
   * because it might be Coq related and in that case we simply modify the class table (side effect!)
   * If the call isn't cog related we simply return a Right(JCall) (no side effect)
   */
  def transformCall(call: Call) : Either[Unit, JBodyStatement] = {

    val Call(QualId(fun), args) = call
    val funs = fun.map(unpackR)
    val (varia, rst) = {
      if (funs.length == 1) {
        ("this", funs)
      } else {
        (funs(0), funs.drop(1))
      }
    }
    if (varia == "Coq") {
      assert(rst.length == 1)
      if (rst(0) == "requires" || rst(0) == "ensures") {
        assert(args.length == 1)
        val arg = exString(transformExpression(args(0)))
        if (rst(0) == "requires") {
          Left(ClassTable.setPrecondition(classid, methodid, arg))
        } else if (rst(0) == "ensures") {
          Left(ClassTable.setPostcondition(classid, methodid, arg))
        } else {
          Left({})
        }
      } else {
        assert(rst(0) == "def")
        assert(args.length == 2)
        val mod : JFieldAccess = transformExpression(args(0)).asInstanceOf[JFieldAccess] //Coq.M.FOO -> JFieldAccess("Coq", "M.FOO")
        val module = mod.field //get rid of "Coq.M."
        val arg = exString(transformExpression(args(1)))
        Left(ClassTable.addCoq(classid, module, arg))
      }
    } else {

      // TODO:  Somehow it doesn't get the full invocation chain (an in transformPrimaryExprFollowedByList)

      log.warning(varia)
      log.warning(rst.toString)
      log.warning(args.toString)

      val fst = if (isFieldAccess(varia)) JFieldAccess(JVariableAccess("this"), varia) else JVariableAccess(varia)

      val result = rst.foldRight(fst) { (current, acc) =>
        JCall(acc, current, args.map(transformExpression))
      }

      Right(result) //XXX: recurse
    }
  }

  /*
   * Transforms a QuailId to a JBodyStatement. This also takes care of making all calls to class varibles explicit
   * by prepending 'this' if needed.
   */
  def transformQualId(qualid: QualId) : JBodyStatement = {

    // recursive method - used to transform nested qualids like: a.a.a.a.b
    def recTransform(xs: List[String]) : JExpression = { // TODO. Can't this be expressed as a fold1?
      xs match {
        case Nil      => throw new Exception("Don't know what to do about an empty qualid list")
        case x :: Nil => if (isFieldAccess(x)) JFieldAccess(JVariableAccess("this"), x) else JVariableAccess(x)
        case x :: xs  => JFieldAccess(recTransform(xs),x)
      }
    }
    recTransform(qualid.xs.map(unpackR).reverse)
  }

  /*
   * Transforms a FormalVariable into a JArgument.
   */
  def transformArgument(param: FormalVariable): JArgument = {
    val FormalVariable(mods, jtype, name) = param
    JArgument(unpackR(name), unpackR(jtype))
  }

  /*
   * Transforms a LocalVar into (possibly multiple) JBindings.
   */
   def transformLocalVariable(variable : LocalVar): List[JBinding] = {
    variable.x match {
      case (mod ~ jtype) ~ (decls : List[Any]) => {
        val typ = unpackR(jtype)
        decls.map {
          case (name : Name) ~ b ~ Some(y) => {
            val init  = transformExpression(y)
            val realn = unpackR(name)
            lvars    += realn -> typ
            JBinding(realn, typ, Some(init))
          }
          case (name : Name) ~ b ~ None => {
            val realn = unpackR(name)
            lvars    += realn -> typ
            JBinding(realn, typ, None)
          }
          case x => throw new Exception("dunno about lvar " + x)
        }
      }
      case y => throw new Exception("dunno about lvar " + y)
    }
  }

  /*
   * Transforms a JStatement into a JSBloack. If it is a JBlock already simply
   * pass it.
   */
  def transformBlock(cs : JStatement) : JBlock = {
    cs match {
      case x  : JBlock         => x
      case xs : JBodyStatement => JBlock(List(xs))
      case x                   => throw new Exception("This isn't a block: " + x)
    }
  }

 /*
   * ====================================
   * Are these methods still used at all?
   * ====================================
   */

  def transformOLF(xs : Option[Any]) : List[String] = {
    xs match {
      case Some(xs : List[List[Any]]) => xs.flatten.map(unpackR)
      case None => List[String]()
    }
  }

  def transformExpression(x : Any) : JExpression = {
    val y = transformAnyExpr(x)
    y match {
      case  e : JExpression => e
      case (e : JExpression) :: Nil => e
      case _ =>
        log.warning("transformExpression: wanted an JExpression, got " + y);
        null
    }
  }

  def transformExpressions(x : Any) : List[JExpression] = {
    x match {
      case a ~ b    => transformExpressions(a) ++ transformExpressions(b)
      case hd :: tl => transformExpressions(hd) ++ transformExpressions(tl)
      case a        => List(transformExpression(a))
    }
  }

  def exString(x : JExpression) : String = {
    x match {
      case JLiteral(s)                  => s
      case JBinaryExpression("+", l, r) => exString(l) + "\n" + exString(r)
      case y                            => log.warning("dunno how to extract from " + y); ""
    }
  }
}
