/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import dk.itu.sdg.util.KopitiamLogger
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

  private var lvars : Set[String] = Set[String]()

  /*
   *  ==============================
   *  The public api
   *  ==============================
   */

  /*
    Given an AST in JavaTerms it will convert it to an AST in JavaAST
  */
  def javaTermsToJavaAST (ast : Any) : List[JStatement] = {
    transform(ast, None)
  }

  def doitHelper (a : Any) : List[SJDefinition] = {
    SJTable.reset
    javaTermsToJavaAST(a).map(translate).flatten
  }

  import dk.itu.sdg.parsing.LengthPosition
  def doit (a : Any, model : String, name : String) : Pair[String,List[Pair[String, Pair[LengthPosition, List[LengthPosition]]]]] = {
    val w = doitHelper(a)
    val (r, offs) = coqoutput(w, model, true, name)
    (r.reduceLeft(_ + "\n" + _), offs)
  }

  def doitNoSpec (a : Any, name : String) : (String, String) = {
    val w = doitHelper(a)
    val (re, offs) = coqoutput(w, "", false, name)
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
  def unpackR (r : Any) : String = {
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
  def transform (x : Any, outer : Option[String]) : List[JStatement] = {
    lvars = Set[String]() // Resetting local variables because we're now converting a new class.
    x match {
      case Nil     => Nil
      case x ~ y   => transform(x, outer) ++ transform(y, outer)
      case x :: xs => transform(x, outer) ++ transform(xs, outer)
      case x       => {
        x match {
          case i : Import => Nil // we don't care about imports for now.
          case SomethingWithModifiers(modifiers, classOrInterface) => 
            val jmodifiers = modifiers.flatMap { case Modifier(Key(mod)) => JModifier(mod) }.toSet
            List(transformClassOrInterface(classOrInterface, outer, jmodifiers))
          case other => 
            List(transformClassOrInterface(other, outer))
        }
      }
    }
  }

  /*
   * Transform a JClass or JInterface to a JClassDefinition or JInterfaceDefinition
   * respectively. Anything else will throw an exception
   */
  def transformClassOrInterface (x : Any, outer : Option[String], modifiers : Set[JModifier] = Set()) : JStatement = {
    x match {
      case JClass(id, jtype, superclass, interfaces, bodyp) => {
        log.info("transforming a JClass, ranging (pos) " + x.asInstanceOf[JClass].pos)
        val is = transformOLF(interfaces)
        val cs = unpackR(superclass)
        val myclassid = unpackR(id)
        val mybody = transformClassOrInterfaceBody(bodyp, Some(myclassid))
        JClassDefinition(modifiers, myclassid, cs, is, mybody, outer)
      }
      case JInterface(id, jtype, interfaces, body) => {
        val is = transformOLF(interfaces)
        val myclass = unpackR(id)
        val mybody = transformClassOrInterfaceBody(body, Some(myclass))
        JInterfaceDefinition(modifiers, myclass, is, mybody)
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
   * NOTE: This method can't return InnerStatement because class & interfaces
   *       doesn't inherit from InnerStatement.
   */
  def transformClassOrInterfaceBody (body : List[Any], outer : Option[String], modifiers : Set[JModifier] = Set()) : List[JStatement] = {
    val transformModifiers : List[Modifier] => Set[JModifier] = (mods) => mods.flatMap { 
      case Modifier(Key(str)) => JModifier(str)
    }.toSet

    body.flatMap {
      case List() ~ (x@SpecStmt(content)) =>
        val r = JSpecExpression(content)
        r.setPos(x.pos)
        r :: Nil
      case x@SpecStmt(content) =>
        val r = JSpecExpression(content)
        r.setPos(x.pos)
        r :: Nil
      case BodyDeclaration(mods, x) => transformClassOrInterfaceBody(List(x), outer, transformModifiers(mods))
      case jclass : JClass => transformClassOrInterface(jclass, outer, modifiers) :: Nil
      case jinterface : JInterface => transformClassOrInterface(jinterface, outer, modifiers) :: Nil
      case jmethod : MethodDeclaration => transformMethodDeclaration(jmethod, modifiers) :: Nil
      case jconstructor : ConstructorDeclaration => transformConstructor(jconstructor, modifiers) :: Nil
      case jfield : FieldDeclaration => transformFieldDeclaration(jfield, modifiers) :: Nil
      case xs: List[Any] => xs.flatMap( (x : Any) => transformClassOrInterfaceBody(List(x), outer, modifiers) )
      // TODO: It should be possible to remove these by improving the parser so they're turned into BodyDeclaration's
      case (ys: List[Modifier]) ~ (i : JInterface) => transformClassOrInterface(i, outer, transformModifiers(ys)) :: Nil
      case Some("static") ~ (x : Block) => transformBlock(x, Some(Static())) :: Nil
      case y ~ (x : MethodDeclaration) => transformMethodDeclaration(x, modifiers) :: Nil
      case ";" => Nil
      case x => throw new Exception("Can't have the following in a class/interface body "+x)
    }
  }

  def transformBlock (block : Block, modifier : Option[Static] = None) : JBlock = {
    JBlock(modifier, block.xs.map(transformAnyExpr(_)))
  }

  /*
   * Transforms a MethodDeclaration into a JMethodDefinition
   */
  def transformMethodDeclaration (method : MethodDeclaration, modifiers : Set[JModifier]) : JMethodDefinition = {
    val (mid, args, body) = extractMethodOrConstructorInfo(method)
    val res = JMethodDefinition(modifiers, mid, unpackR(method.jtype), args, body)
    Console.println("transforming method at " + method.pos)
    res.setPos(method.pos)
    res
  }

  /*
   * Transforms a ConstructorDeclaration into a JConstructorDefinition
   */
  def transformConstructor (constructor : ConstructorDeclaration, modifiers : Set[JModifier] = Set()) : JConstructorDefinition = {
    val (mid, args, body) = extractMethodOrConstructorInfo(constructor)
    val r = JConstructorDefinition(modifiers, mid, args, body)
    r.setPos(constructor.pos)
    r
  }

  /*
   * Given a ConstructorDeclaration or MethodDeclaration it will extract the information needed
   * to be transformd into a JMethodDefinition.
   */
  def extractMethodOrConstructorInfo (term : Term) : (String, List[JArgument], List[JBodyStatement]) = {
    lvars = Set[String]()

    val (mid, parameters, throws, body) = term match {
      case ConstructorDeclaration(id, parameters, throws, bdy)   => (unpackR(id), parameters, throws, bdy)
      case MethodDeclaration(id, jtype, parameters, throws, bdy) => (unpackR(id), parameters, throws, bdy)
    }

    val args = parameters.getOrElse(Nil).asInstanceOf[List[Any]].flatMap {
      case p : FormalVariable => Some(transformArgument(p))
      case _ => None
    }
    //add args to lvars!
    args.map(x => x match {
      case JArgument(n, t) => lvars += n
    })

    val transformedBody = body match {
      case xs : List[Any] => transformMethodBody(xs)
      case b  : Block     => JBlock(None, transformMethodBody(b.xs)) :: Nil
      case None           => Nil
    }
    (mid, args, transformedBody)
  }

  /*
   * Transforms a FieldDeclaration into a JFieldDefinition
   */
  def transformFieldDeclaration (field : FieldDeclaration, modifiers : Set[JModifier] = Set()) : JFieldDefinition = {
    val FieldDeclaration(id, jtype, rest) = field
    log.info("field pos info " + field.pos)

    val initializer = rest match {
      case x ~ Some("=" ~ y) => Some(transformAnyExpr(y).asInstanceOf[JExpression])
      case _ => None
    }

    val name = unpackR(id)
    val typ = unpackR(jtype)
    JFieldDefinition(modifiers, name, typ, initializer)
  }

  /*
   * Transforms the body of method into a list of JBodyStatement. This assumes that
   * a body only contains AnyExpr.
   */
  def transformMethodBody (body : List[Any]) : List[JBodyStatement] = {
    body.map {
      case vars : LocalVar => transformLocalVariable(vars)
      case AnyStatement(";") => Nil
      case (x : SpecStmt) =>
        val newspec = JSpecExpression(x.e)
        newspec.setPos(x.pos)
        newspec :: Nil
      case expr : AnyExpr => transformAnyExpr(expr) :: Nil
    }.flatten
  }

  /*
   * Transforms AnyExpr (or well, Any) to a List of JBodyStatements. It's a list because
   * some statements in JavaTerms may turn into multiple statements in JavaAST.
   *
   * TODO: Isn't it possible to NOT use Any here.
   */
  def transformAnyExpr (expr : Any) : JBodyStatement = {
    expr match {
      // statements
      case AnyStatement(x) => transformAnyExpr(x)
      case Return(x) => JReturn(transformExpression(x))
      case While(test, body) => JWhile(transformExpression(test), transformJBlock(transformAnyExpr(body)))
      case Block(xs) => JBlock(None, transformMethodBody(xs))
      case cond : Conditional => transformConditional(cond)
      // expressions
      case binaryExpr : BinaryExpr => transformBinaryExpr(binaryExpr)
      case PrimaryExpr(x) => transformAnyExpr(x)
      case ParExpr(x) => transformAnyExpr(x)
      case "this" => JVariableAccess("this")
      case PostExpr(k, x) => JPostfixExpression(unpackR(k), transformExpression(x))
      case UnaryExpr(op, v) => JUnaryExpression(unpackR(op), transformExpression(v))
      case NewExpression(ntype, args) => JNewExpression(unpackR(ntype), args.map(transformExpression))
      case Name(x) => JVariableAccess(x)
      case Expr(x) => transformAnyExpr(x)
      case PostFixExpression(x) => transformAnyExpr(x)
      case Lit(x) => JLiteral(unpackR(x))
      case Some(x) => transformAnyExpr(x)
      case c : Call => transformCall(c)
      case qualid : QualId => transformQualId(qualid)
      case (x : PrimaryExpr) ~ (y : List[Any]) => transformPrimaryExprFollowedByList(x,y)
      case ("." ~ expr) => transformAnyExpr(expr)
      case (("assert" ~ x) ~ None) => JAssert(transformAnyExpr(x))
    }
  }

  /*
   * Transforms a Conditional into a JConditional.
   */
  def transformConditional (cond : Conditional) : JBodyStatement = {
    val Conditional(test, consequence, alternative) = cond
    val cs = transformJBlock(transformAnyExpr(consequence))
    val alt = alternative match {
      case None => JBlock(None, List[JBodyStatement]())
      case Some(x) => transformJBlock(transformAnyExpr(x))
    }
    JConditional(transformExpression(test), cs, alt)
  }

  /*
   * Transforms a PrimaryExpr and a List of Any into a JBodyStatement. The List of Any is
   * the result from the parser. This happens when it parses a chained expression.
   */
  def transformPrimaryExprFollowedByList (x : PrimaryExpr, y : List[Any]) : JBodyStatement = {

    /**
     * Dear reader,
     *
     * y.map(transformAnyExpr) will convert the parse-result into JavaAST however
     * because of lack of information Call(...) and Name(...) will be turned into JCall(this,_,_)
     * and JVariableAccess(...) - this is wrong as we want JFieldAccess and JCall(expr,name,args) to
     * be called on the result of the call and field accesses before it. To fix this we reverse the list
     * and use foldRight to build up the proper AST and do the transformations.
     *
     * /Mads
     */
    val x_ = transformExpression(x)
    val y_ = y.map(transformAnyExpr)
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
  def transformBinaryExpr (expr : BinaryExpr) : JBodyStatement = {

    // Checks an operator as an assignemnt operator.
    val isAssignmentOperator: String => Boolean = (operator: String) =>
      List("+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", "<<=", ">>=", ">>>=").contains(operator)

    val BinaryExpr(op, le, ri) = expr
    val oper = unpackR(op)
    oper match {
      case "=" => {
        transformAnyExpr(le) match {
          case JFieldAccess(cnx, nam) => JFieldWrite(cnx, nam, transformExpression(ri))
          case JVariableAccess(nam)   => JAssignment(nam, transformExpression(ri))
      }}
      case assignmentOperator if isAssignmentOperator(assignmentOperator) => {
        val binOp = assignmentOperator.replace("=","") // all assignment operators are just the normal operator with '=' appended
        transformAnyExpr(le) match {
          case va @ JVariableAccess(name)  => JAssignment(name, JBinaryExpression(binOp, va, transformExpression(ri)))
          case fa @ JFieldAccess(cnx, nam) => JFieldWrite(cnx, nam, JBinaryExpression(binOp, fa, transformExpression(ri)))
      }}
      case _ => JBinaryExpression(oper, transformExpression(le), transformExpression(ri))
    }
  }

  def fieldOrVar (s : String) : JExpression = {
    if (lvars.contains(s))
      JVariableAccess(s)
    else
      JFieldAccess(JVariableAccess("this"), s)
  }

  /*
   * Transforms a Call into a JBodyStatement.
   */
  def transformCall (call : Call) : JBodyStatement = {
    val Call(QualId(fun), args) = call
    val funs = fun.map(unpackR)
    val (varia, rst) =
      if (funs.length == 1)
        (JVariableAccess("this"), funs)
      else
        (fieldOrVar(funs(0)), funs.drop(1))

    rst.foldRight(varia) { (current, acc) =>
      JCall(acc, current, args.map(transformExpression))
    }
  }

  /*
   * Transforms a QuailId to a JBodyStatement.
   */
  def transformQualId (qualid : QualId) : JBodyStatement = {

    // recursive method - used to transform nested qualids like: a.a.a.a.b
    def recTransform (xs : List[String]) : JExpression = { // TODO. Can't this be expressed as a fold1?
      xs match {
        case Nil      => throw new Exception("Don't know what to do about an empty qualid list")
        case x :: Nil => fieldOrVar(x)
        case x :: xs  => JFieldAccess(recTransform(xs),x)
      }
    }
    recTransform(qualid.xs.map(unpackR).reverse)
  }

  /*
   * Transforms a FormalVariable into a JArgument.
   */
  def transformArgument (param : FormalVariable): JArgument = {
    val FormalVariable(mods, jtype, name) = param
    JArgument(unpackR(name), unpackR(jtype))
  }

  /*
   * Transforms a LocalVar into (possibly multiple) JBindings.
   */
   def transformLocalVariable (variable : LocalVar) : List[JBinding] = {
    variable.x match {
      case (mod ~ jtype) ~ (decls : List[Any]) => {
        val typ = unpackR(jtype)
        decls.map {
          case (name : Name) ~ b ~ Some(y) => {
            val init  = transformExpression(y)
            val realn = unpackR(name)
            lvars += realn
            JBinding(realn, typ, Some(init))
          }
          case (name : Name) ~ b ~ None => {
            val realn = unpackR(name)
            lvars += realn
            JBinding(realn, typ, None)
          }
          case x => throw new Exception("dunno about lvar " + x)
        }
      }
      case y => throw new Exception("dunno about lvar " + y)
    }
  }

  /*
   * Transforms a JStatement into a JBlock. If it is a JBlock already simply
   * pass it.
   */
  def transformJBlock (cs : JStatement, modifier : Option[Static] = None) : JBlock = {
    cs match {
      case x  : JBlock         => x
      case xs : JBodyStatement => JBlock(modifier, List(xs))
      case x                   => throw new Exception("This isn't a block: " + x)
    }
  }

  def transformOLF (xs : Option[Any]) : List[String] = {
    xs match {
      case Some(xs : List[List[Any]]) => xs.flatten.map(unpackR)
      case None => List[String]()
    }
  }

  def transformExpression (x : Any) : JExpression = {
    val y = transformAnyExpr(x)
    y match {
      case  e : JExpression => e
      case _ => throw new Exception("transformExpression: wanted an JExpression, got " + y);
    }
  }
}
