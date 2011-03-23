/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

object Gensym {
  var count : Int = 0
  def newsym () : String = {
    count += 1
    return "tmp_" + count
  }
}

trait JavaToSimpleJava {
  private var cname : String = ""
  private var mname : String = ""
  private var mtype : String = ""

  def translate (classname : String, x : JMethodDefinition) : JMethodDefinition = {
    mname = x.id
    mtype = x.jtype
    val mbody = x.body
    val margs = x.parameters
    cname = classname
    //Console.println("body: " + body)
    Gensym.count = 0
    val tb = mbody.foldLeft(List[JBodyStatement]())((b,a) => b ++ extractCalls(a))
    //Console.println("body translated: " + tb)
    JMethodDefinition(mname, mtype, margs, tb)
  }

  def exprtotype (x : JExpression) : String = {
    x match {
      case JBinaryExpression(op, l, r) => exprtotype(l) //assumption: typeof(x op y) == typeof(x) == typeof(y)
      case JUnaryExpression(op, e) => exprtotype(e) //assumption: typeof(op x) == typeof(x)
      case JPostfixExpression(op, e) => exprtotype(e)
      case JLiteral(x) => if (x == "null") "Object" else try { x.toInt.toString; "int" } catch { case e => "String" }
      case JVariableAccess(v) => ClassTable.getLocalVar(cname, mname, v)
      case x => Console.println("didn't expect to need to convert this expr to a type: " + x); "Object"
    }
  }

  def getUsedVars (x : JExpression) : List[String] = {
    x match {
      case JConditional(t, c, a) => getUsedVars(t)
      case JBinaryExpression(op, l, r) => getUsedVars(l) ++ getUsedVars(r)
      case JUnaryExpression(op, e) => getUsedVars(e)
      case JPostfixExpression(op, e) => getUsedVars(e)
      case JCall(v, f, arg) => List(v) ++ arg.map(getUsedVars).flatten
      case JNewExpression(t, a) => a.map(getUsedVars).flatten
      case JLiteral(x) => List[String]()
      case JVariableAccess(v) => List(v)
      case JFieldAccess(v, f) => getUsedVars(v)
    }
  }

  var tmp : String = null

  def extractCalls (statement : JBodyStatement) : List[JBodyStatement] = {
    //Console.println("calling extract for " + statement)
    statement match {
      case JBlock(xs) => List(JBlock(xs.foldLeft(List[JBodyStatement]())((b, a) => b ++ extractCalls(a))))
      case JAssignment(x, r) =>
        if (! getUsedVars(r).contains(x))
          tmp = x
        val (arg, ins) = extractHelper(r)
        tmp = null
        if (arg.isInstanceOf[JVariableAccess] && arg.asInstanceOf[JVariableAccess].variable == x)
          ins
        else
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
      case JReturn(e : JLiteral) =>
        List(JReturn(e))
      case JReturn(exxx) =>
        Console.println("return of an expression " + exxx)
        val (t, fresh) = sym(mtype)
        val (ra, ri) = extractHelper(exxx)
        val res : List[JBodyStatement] = ri ++ List(JBinding(t, mtype, Some(ra)))
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

  def sym (t : String) : (String, Boolean) = {
    if (tmp != null)
      (tmp, false)
    else {
      val nt = Gensym.newsym
      ClassTable.addLocal(cname, mname, nt, t)
      (nt, true)
    }
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
        val typ = "int"
        val (t, fresh) = sym(typ)
        val oper = if (op == "++") "+" else if (op == "--") "-" else { Console.println("dunno postfix " + op); op }
        if (fresh)
          (JVariableAccess(t),
           JBinding(t, typ, Some(va)) :: List(JBinaryExpression(oper, va, JLiteral("1"))))
        else
          (JVariableAccess(t),
           JAssignment(t, va) :: List(JBinaryExpression(oper, va, JLiteral("1"))))
      case JFieldAccess(con, f) =>
        val (a, i) = extractHelper(con)
        val tclass = ClassTable.getFieldType(ClassTable.getLocalVar(cname, mname, exprtotype(a)), f)
        val (t, fresh) = sym(tclass)
        if (fresh)
          (JVariableAccess(t), i ++ List(JBinding(t, tclass, Some(JFieldAccess(a, f)))))
        else
          (JVariableAccess(t), i ++ List(JAssignment(t, JFieldAccess(a, f))))
      case JCall(variable, name, args) =>
        val (as, ins) = exL(args)
        val ttype = ClassTable.getMethodType(cname, mname, variable, name)
        val (t, fresh) = sym(ttype)
        if (fresh)
          (JVariableAccess(t), ins ++ List(JBinding(t, ttype, Some(JCall(variable, name, as)))))
        else
          (JVariableAccess(t), ins ++ List(JAssignment(t, JCall(variable, name, as))))
      case JNewExpression(name, args) =>
        val (as, ins) = exL(args)
        val (t, fresh) = sym(name)
        if (fresh)
          (JVariableAccess(t), ins ++ List(JBinding(t, name, Some(JNewExpression(name, as)))))
        else
          (JVariableAccess(t), ins ++ List(JAssignment(t, JNewExpression(name, as))))
      case JConditional(test, c, a) =>
        val (t, fresh) = sym("Object")
        val (ta, ti) = extractHelper(test)
        var ttyp : String = ""
        val getb = (x : JBodyStatement) => {
          //Console.println("extracting conditional, body is " + x)
          assert(x.isInstanceOf[JBlock])
          val ps = x.asInstanceOf[JBlock].body
          ps.foreach(x => assert(x.isInstanceOf[JExpression]))
          val bs = ps.map(x => x.asInstanceOf[JExpression])
          val (ca, ci) = exL(bs)
          //Console.println("extracted ca: " + ca + "\nci: " + ci)
          val lastc = ca.takeRight(1)(0)
          ttyp = lowerBound(ttyp, ClassTable.getLocalVar(cname, mname, exprtotype(lastc)))
          if (ca.length == 1)
            JBlock(ci ++ List(JAssignment(t, lastc)))
          else
            JBlock(ci ++ ca.dropRight(1) ++ List(JAssignment(t, lastc)))
        }
        val newc = getb(c)
        val newa = getb(a)
        if (fresh) {
          ClassTable.addLocal(cname, mname, t, ttyp)
          (JVariableAccess(t), ti ++ List(JBinding(t, ttyp, None), JConditional(ta, newc, newa)))
        } else
          (JVariableAccess(t), ti ++ List(JConditional(ta, newc, newa)))
      case x => (x, List[JBodyStatement]())
    }
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
