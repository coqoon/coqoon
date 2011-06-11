/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashMap }
import dk.itu.sdg.util.{ KopitiamLogger }

object Gensym {
  var count : Int = 0
  def newsym () : String = {
    count += 1
    return "tmp_" + count
  }
}

/*
  This trait provides methods for parsing an AST expressed in JavaAST into an AST expressed in 
  SimpleJavaAst. 
*/
trait JavaToSimpleJava extends KopitiamLogger {
  
  private var cname : String = ""
  private var mname : String = ""

  /*
    Translates a JMethodDefinition into a SJMethodDefinition
  */
  def translate (classname : String, x : JMethodDefinition) : SJMethodDefinition = {
    mname = x.id
    val mtype = x.jtype
    val mbody = x.body
    val margs = x.parameters.map( param => SJArgument(param.id, param.jtype) )
    val modifiers = x.modifiers
    cname = classname
    //Console.println("body: " + body)
    Gensym.count = 0
    val tb = mbody.foldLeft(List[SJStatement]())((b,a) => b ++ extractCalls(a))
    
    // for {
    //   statement <- mbody 
    //   (translatedStatements, localVariables) <- extractCalls(statement)
    // } yield 
    
    /*
      TODO: Need to extract the local variables 
    */
    val localVariables = new HashMap[String, String]()
    //Console.println("body translated: " + tb)
    SJMethodDefinition(modifiers, mname, mtype, margs, tb, localVariables)
  }
  
  /*
    Rewrite a JClassDefinition such that all initializers of JFieldDefinition's are moved into the
    constructor of the class. If a constructor exists the statements are prepended, otherwise a 
    default constructor is created.
  */
  def rewriteConstructor( clazz: JClassDefinition): JClassDefinition = {
    
    val defaultConstructor = JConstructorDefinition(Set(Public()), clazz.id, Nil, Nil)
    
    // fold the list extracting all initializers and statements in constructors 
    val constructorStatements = clazz.body.foldLeft(defaultConstructor){ (folded, current) => current match {
      case JFieldDefinition(_,id,_,Some(initializer)) => folded.copy(body = folded.body :+ JFieldWrite(JVariableAccess("this"),id,initializer))
      case JConstructorDefinition(mods,jtype,parameters,body) => 
        val flattened = body.flatMap { 
          case JBlock(_,stms) => stms
          case other => List(other)
        }
      folded.copy(mods,jtype,parameters, folded.body ::: flattened)
      case _ => folded
    }}

    val newBody = clazz.body.flatMap {
      case jf @ JFieldDefinition(_,_,_,_) => Some(jf.copy( initializer = None))
      case jcd : JConstructorDefinition => None
      case other => Some(other)
    } ::: List(constructorStatements)
    clazz.copy( body = newBody)
  }

  def exprtotype (x : SJExpression) : String = {
    x match {
      case SJBinaryExpression(op, l, r) => exprtotype(l) //assumption: typeof(x op y) == typeof(x) == typeof(y)
      case SJUnaryExpression(op, e)     => exprtotype(e) //assumption: typeof(op x) == typeof(x)
      case SJLiteral(x)                 => if (x == "null") "Object" else try { x.toInt.toString; "int" } catch { case e => "String" }
      case SJVariableAccess(v)          => ClassTable.getLocalVar(cname, mname, v)
      case x                            => Console.println("didn't expect to need to convert this expr to a type: " + x); "Object"
    }
  }

  def getUsedVars (x : JExpression) : List[String] = {
    x match {
      case JConditional(t, c, a) => getUsedVars(t)
      case JBinaryExpression(op, l, r) => getUsedVars(l) ++ getUsedVars(r)
      case JUnaryExpression(op, e) => getUsedVars(e)
      case JPostfixExpression(op, e) => getUsedVars(e)
      case JCall(JVariableAccess(v), f, arg) => List(v) ++ arg.map(getUsedVars).flatten
      case JNewExpression(t, a) => a.map(getUsedVars).flatten
      case JLiteral(x) => List[String]()
      case JVariableAccess(v) => List(v)
      case JFieldAccess(v, f) => getUsedVars(v)
    }
  }

  var tmp : String = null

  /*
    Transforms a JBodyStatement into a List[SJStatement] and HashMap[String, String] the latter being the local 
    variables in the body of the method.
  */
  def extractCalls (statement : JBodyStatement) : (List[SJStatement], HashMap[String, String]) = statement match {

      case JBlock(modifier, xs) => 
        log.warning("Converting a JBlock into a List[SJStatement]")
        xs.foldLeft(List[SJStatement]())((b, a) => b ++ extractCalls(a))

      // case JAssignment(x, r) =>
      //   if (! getUsedVars(r).contains(x))
      //     tmp = x
      //   val (arg, ins) = extractHelper(r)
      //   tmp = null
      //   if (arg.isInstanceOf[SJVariableAccess] && arg.asInstanceOf[SJVariableAccess].variable == x)
      //     ins
      //   else
      //     ins ++ List(SJAssignment(SJVariableAccess(x), arg))

      // case JBinding(n, t, i) =>
      //   i match {
      //     case None => List(JBinding(n, t, i)) // TODO: JBinding? 
      //     case Some(x) =>
      //       val (ih, ins) = extractHelper(x)
      // 
      //       if (ih.isInstanceOf[SJVariableAccess] && ins.length > 0 && ins.takeRight(1)(0).isInstanceOf[JBinding]) { // TODO, JBinding
      //         val tmpvar = ih.asInstanceOf[SJVariableAccess].variable
      //         val lb = ins.takeRight(1)(0).asInstanceOf[JBinding] // TODO, JBinding
      //         if (lb.name == tmpvar && lb.jtype == t)
      //           ins.dropRight(1) ++ List(JBinding(n, t, lb.init)) // TODO, JBinding
      //         else
      //           ins ++ List(JBinding(n, t, Some(ih))) // TODO, JBinding
      //       } else
      //         ins ++ List(JBinding(n, t, Some(ih))) // TODO, JBinding
      //   }

      case JBinaryExpression(op, l, r) =>
        val (larg, lins) = extractHelper(l)
        val (rarg, rins) = extractHelper(r)
        lins ++ rins ++ List(SJBinaryExpression(op, larg, rarg))

      case JUnaryExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        vis ++ List(SJUnaryExpression(op, va))

      // case JPostfixExpression(op, v) =>
      //   val oper = op match {
      //     case "++" => "+"
      //     case "--" => "-"
      //   }
      //   val res = SJBinaryExpression(oper, v, SJLiteral("1")) //TODO is it right to convert it here? 
      //   val ass = v match {
      //     case JFieldAccess(variable, value) => JFieldWrite(variable, value, res)
      //     case JVariableAccess(x) => JAssignment(x, res)
      //   }
      //   extractCalls(ass)

      case JFieldWrite(x, f, v) =>
        val (a, i) = extractHelper(v)
        val (va, vi) = extractHelper(x)
        
        val variable = va match {
          case sjv @ SJVariableAccess(name) => sjv
          // TODO: Not sure what to do here if we don't get a SJVariableAccess
        }
        
        vi ++ i ++ List(SJFieldWrite(variable, f, a))

      case JConditional(t, c, a) =>
        val (ta, ti) = extractHelper(t)
        val con = extractCalls(c)
        val alt = extractCalls(a)
        ti ++ List(SJConditional(ta, con, alt))

      // case JCall(receiver, name, args) =>
      //   val (temp, lines) = extractHelper(receiver)
      //   val (ars, ins)    = exL(args)
      //   /*
      //     TODO: Using None here for now. The option of SJCall seems out of place to me, I thought that 
      //           SJ should always use temp variables. Ask Hannes 
      //   */
      //   lines ++ ins ++ List(SJCall(None, temp, name, ars)) 

      // case JNewExpression(name, arguments) =>
      //   val (ars, ins) = exL(arguments)
      //   ins ++ List(SJNewExpression(name, ars)) // TODO: How to I find the type? 

      case JWhile(test, body) =>
        val (ta, ti) = extractHelper(test)
        val newbody = extractCalls(body)
        ti ++ List(SJWhile(ta, newbody))

      case JReturn(e : JVariableAccess) => 
        List(SJReturn(SJVariableAccess(e.variable)))

      case JReturn(e : JLiteral) =>
        List(SJReturn(SJLiteral(e.value)))

      case JReturn(exxx) =>
        val (ra, ri) = extractHelper(exxx)
        ri ++ List(SJReturn(ra))

      // NB: We don't want a default case, we want it to crash early 
  }

  def exL (xs : List[JExpression]) : (List[SJExpression], List[SJStatement]) = {
    val vals = xs.map(extractHelper)
    (vals.map(z => z._1), vals.map(z => z._2).flatten)
  }

  def sym (t : String) : (String, Boolean) = {
    if (tmp != null && ClassTable.getLocalVar(cname, mname, tmp).equals(t))
      (tmp, false)
    else {
      val nt = Gensym.newsym
      ClassTable.addLocal(cname, mname, nt, t)
      (nt, true)
    }
  }

  /*
    TODO: Explain what it does
  */
  def extractHelper (x : JExpression) : (SJExpression, List[SJStatement]) = x match {

      case JBinaryExpression(op, l, r) =>
        val (ri, rii) = extractHelper(r)
        val (le, lei) = extractHelper(l)
        (SJBinaryExpression(op, le, ri), rii ++ lei)

      case JUnaryExpression(op, v) =>
        val (va, vis) = extractHelper(v)
        (SJUnaryExpression(op, va), vis)

      // case JPostfixExpression(op, v) =>
      //   val (va, vis) = extractHelper(v)
      //   val typ = "int"
      //   val (t, fresh) = sym(typ)
      //   val oper = if (op == "++") "+" else if (op == "--") "-" else { Console.println("dunno postfix " + op); op }
      //   if (fresh)
      //     (SJVariableAccess(t),
      //      JBinding(t, typ, Some(va)) :: List(SJBinaryExpression(oper, va, SJLiteral("1")))) // TODO JBinding
      //   else
      //     (SJVariableAccess(t),
      //      SJAssignment(SJVariableAccess(t), va) :: List(SJBinaryExpression(oper, va, SJLiteral("1"))))

      // TODO: Shoul also only be part of JBinding or JAssignment I think
      // case JFieldAccess(con, f) =>
      //   val (a, i) = extractHelper(con)
      //   val tclass = ClassTable.getFieldType(ClassTable.getLocalVar(cname, mname, exprtotype(a)), f)
      //   val (t, fresh) = sym(tclass)
      //   if (fresh)
      //     (SJVariableAccess(t), i ++ List(JBinding(t, tclass, Some(SJFieldAccess(a, f))))) // TODO JBinding
      //   else
      //     (SJVariableAccess(t), i ++ List(SJAssignment(SJVariableAccess(t), SJFieldAccess(a, f))))
      
      // TODO: We'll never hit a JNewExpression on it's own, always as part of JBinding or JAssignment 
      // case JCall(receiver, name, args) =>
      //   val (a,i)      = extractHelper(receiver)
      //   val (as, ins)  = exL(args)
      //   val ttype      = ClassTable.getMethodType(cname, mname, exprtotype(a), name, as.map(exprtotype))
      //   val (t, fresh) = sym(ttype)
      //   if (fresh)
      //     (SJVariableAccess(t), i ++ ins ++ List(JBinding(t, ttype, Some(SJCall(a, name, as))))) // TODO JBinding
      //   else
      //     (SJVariableAccess(t), i ++ ins ++ List(SJAssignment(SJVariableAccess(t), SJCall(a, name, as))))
      
      // TODO: We'll never hit a JNewExpression on it's own, always as part of JBinding or JAssignment 
      // case JNewExpression(name, args) =>
      //   val (as, ins) = exL(args)
      //   val (t, fresh) = sym(name)
      //   if (fresh)
      //     (SJVariableAccess(t), ins ++ List(JBinding(t, name, Some(SJNewExpression(name, as))))) // TODO JBinding
      //   else
      //     (SJVariableAccess(t), ins ++ List(SJAssignment(SJVariableAccess(t), SJNewExpression(name, as))))

      // case JConditional(test, c, a) => Nil
        // val (t, fresh) = sym("Object")
        //         val (ta, ti) = extractHelper(test)
        //         var ttyp : String = ""
        //   
        //         val getb = (x : JBodyStatement) => {
        //           assert(x.isInstanceOf[SJBodyBlock])
        //           val ps = x.asInstanceOf[SJBodyBlock].body
        //           ps.foreach(x => assert(x.isInstanceOf[SJExpression]))
        //           val bs = ps.map(x => x.asInstanceOf[SJExpression])
        //           val (ca, ci) = exL(bs)
        //           //Console.println("extracted ca: " + ca + "\nci: " + ci)
        //           val lastc = ca.takeRight(1)(0)
        //           ttyp = lowerBound(ttyp, ClassTable.getLocalVar(cname, mname, exprtotype(lastc)))
        //           if (ca.length == 1)
        //             SJBodyBlock(None, ci ++ List(SJAssignment(SJVariableAccess(t), lastc)))
        //           else
        //             SJBodyBlock(None, ci ++ ca.dropRight(1) ++ List(SJAssignment(SJVariableAccess(t), lastc)))
        //}
        // val newc = getb(c)
        // val newa = getb(a)
        // if (fresh) {
        //   ClassTable.addLocal(cname, mname, t, ttyp)
        //   (SJVariableAccess(t), ti ++ List(JBinding(t, ttyp, None), SJConditional(ta, newc, newa))) // TODO JBinding
        // } else
        //   (SJVariableAccess(t), ti ++ List(SJConditional(ta, newc, newa)))

      // No default case - We want it to crash early! 
  }

  def lowerBound (typea : String, typeb : String) : String = {
    if (typea.length == 0) typeb
    else if (typeb.length == 0) typea
    else if (typea.matches(typeb)) typea
    else {
      //do the real comparison at some point
      typea
    }
  }
}
