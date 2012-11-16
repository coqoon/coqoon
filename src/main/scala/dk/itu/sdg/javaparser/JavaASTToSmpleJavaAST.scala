/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.collection.immutable.HashMap
import dk.itu.sdg.util.KopitiamLogger

object Gensym {
  var count : Int = 0
  def newsym () : String = {
    count += 1
    return "tmp_" + count
  }
}

/*
  This trait provides methods for transforming a JavaAST into a SimpleJavaAST.
*/
trait JavaToSimpleJava extends KopitiamLogger {
  /*
    Expects a JStatement, either a class or interface definition;
    returns the SimpleJavaAST translation
   */
  import scala.collection.mutable.Stack
  def translate (s : JStatement) : List[SJDefinition] = {
    //defer real work
    val w = new Stack[Pair[JStatement, Option[String]]]
    w.push((s, None))
    var res : List[SJDefinition] = List[SJDefinition]()
    while (w.length > 0) {
      val t = w.pop
      val (tr, todo) = translateInner(t._1, t._2)
      res = tr :: res
      w.pushAll(todo)
    }
    res
  }


  def translateInner (s : JStatement, out : Option[String]) : (SJDefinition, List[Pair[JStatement, Option[String]]]) = {
    var working = List[JStatement]()
    val res : SJDefinition = s match {
      case JClassDefinition(mods, name, supers, interf, body, outer) =>
        var fs = HashMap[String,String]()
        var ms = HashMap[String,String]()
        var hasc : Boolean = false
        val fields = body.flatMap(x => x match {
          case (x : JFieldDefinition) => fs += x.id -> x.jtype; Some(x)
          case (x : JConstructorDefinition) => hasc = true; None
          case JMethodDefinition(m, n, t, o, b) => ms += n -> t; None
          case y => None
        })
        val bdy =
          if (hasc == false)
             body ++ List(JConstructorDefinition(Set(Public()), name, List(), List()))
           else
             body
        val (nbody, rt) = translateCIBody(bdy, fs, ms, HashMap("this" -> name), fields)
        working ++= rt
        SJClassDefinition(mods, name, supers, interf, nbody, out, fs)
      case JInterfaceDefinition(mods, name, interf, body) =>
        val (nbody, rt) = translateIBody(body)
        working ++= rt
        SJInterfaceDefinition(mods, name, interf, nbody)
    }
    SJTable.addClass(res)
    (res, working.map(x => (x, Some(res.id))))
  }

  def tArgs (a : List[JArgument], locals : HashMap[String, String]) : (List[SJArgument], HashMap[String, String]) = {
    var ls : HashMap[String, String] = locals;
    (a.map(x => x match { case JArgument(n, t) => ls += n -> t; SJArgument(n, t)}), ls)
  }

  def translateIBody (body : List[JStatement]) : (List[SJBodyDefinition], List[JStatement]) = {
    var rest = List[JStatement]()
    (body.flatMap(x => x match {
      case (x : JInterfaceDefinition) => rest ::= x; None
      case y@JMethodDefinition (m, n, t, a, b) =>
        assert(b.length == 0)
        val (x, l) = tArgs(a, HashMap[String, String]())
        val meth = SJMethodDefinition(m, n, t, x, List(), l)
        meth.setPos(y.pos)
        Some(meth)
      case JFieldDefinition (m, n, t, i) =>
        assert(m.contains(Final()))
        //we should remember initializer somewhere
        Some(SJFieldDefinition(m, n, t))
      case y@JSpecExpression (x) =>
        val r = ParseSpecification.parse(x)
        r.setPos(y.pos)
        Some(r)
    }), rest)
  }

  /*
   translates class body, returns translated AST plus inner classes and interfaces
  */
  def translateCIBody (body : List[JStatement], fs : HashMap[String, String], ms : HashMap[String, String], locals : HashMap[String, String], fields : List[JFieldDefinition]) : (List[SJBodyDefinition], List[JStatement]) = {
    var rest = List[JStatement]()
    (body.flatMap(x => x match {
      case (x : JClassDefinition) => rest ::= x; None //only true for static inner classes, others need ptr to class (not in context here :/)
      case (x : JInterfaceDefinition) => rest ::= x; None
      case JFieldDefinition(mods, name, jtype, init) => Some(SJFieldDefinition(mods, name, jtype))
      case x@JMethodDefinition(mods, name, jtype, args, body) =>
        val (targs, ls0) = tArgs(args, locals)
        val (nbody, ls1) = translateBody(body, fs, ms, ls0)
        val meth = SJMethodDefinition(mods, name, jtype, targs, nbody, ls1)
        meth.setPos(x.pos)
        Some(meth)
      case x@JConstructorDefinition(mods, jtype, args, body) =>
        val (targs, ls0) = tArgs(args, locals)
        val (nbody, ls1) = translateCBody(body, fields, fs, ms, ls0)
        val cons = SJConstructorDefinition(mods, jtype, targs, nbody, ls1)
        cons.setPos(x.pos)
        Some(cons)
      case JBlock(mods, body) =>
        Some(SJBodyBlock(mods, translateBody(body, fs, HashMap(), HashMap())._1))
      case y@JSpecExpression(x) =>
        val r = ParseSpecification.parse(x)
        r.setPos(y.pos)
        Some(r)
    }), rest)
  }

  def translateCBody (x : List[JBodyStatement], fdefs : List[JFieldDefinition], fields : HashMap[String, String], ms : HashMap[String, String], ls : HashMap[String,String]) : (List[SJStatement], HashMap[String, String]) = {
    val fs = fdefs.flatMap(x => x match {
      case JFieldDefinition(m, a, b, None) => None
      case JFieldDefinition(m, a, b, Some(c)) => Some(JFieldWrite(JVariableAccess("this"), a, c))
    })
    //how is the initialization? first fields or first constructor?
    translateBody(fs ++ x, fields, ms, ls)
  }

  /*
    Translates a list of body statements into a list of SJStatement, using
    fields and arguments to find out about bindings, returning map of local variables
  */
  def translateBody (xs : List[JBodyStatement], fields : HashMap[String, String], ms : HashMap[String, String], ls : HashMap[String,String]) : (List[SJStatement], HashMap[String, String]) = {
    Gensym.count = 0
    translateBodyInner(xs, fields, ms, ls)
  }

  def translateBodyInner (xs : List[JBodyStatement], fields : HashMap[String, String], ms : HashMap[String, String], ls : HashMap[String,String]) : (List[SJStatement], HashMap[String, String]) = {
    var hm : HashMap[String, String] = ls
    var r : List[SJStatement] = List[SJStatement]()
    var i = 0
    while (i < xs.length) {
      val y = xs(i)
      //log.warning("translating y " + y)
      val (re : List[SJStatement], h : HashMap[String, String]) = translateStatement(y, fields, ms, hm)
      //log.warning("result is re " + re)
      hm = h
      r = r ++ re
      i += 1
    }
    (r, hm)
  }
  
  def translateStatement (statement : JBodyStatement, fields : HashMap[String, String], ms : HashMap[String, String], ls : HashMap[String, String]) : (List[SJStatement], HashMap[String, String]) = 
    statement match {
      case JBlock(modifier, xs) => 
        translateBodyInner(xs, fields, ms, ls)

      case ass@JAssignment(x, r) =>
        log.info("JAssignment has pos: " + ass.pos)
        val (a, b, c) = extractHelper(Some(SJVariableAccess(x)), r, fields, ms, ls, ass.pos)
        //log.warning("transforming JAssignment returned (a): " + a + " (b): " + b)
        if ((b.length == 0) || !(a.isInstanceOf[SJVariableAccess] && (a.asInstanceOf[SJVariableAccess].variable == x))) {
          val r = SJAssignment(SJVariableAccess(x), a)
          r.setPos(ass.pos)
          (b ++ List(r), c)
        } else
          (b, c)

      case b@JBinding(n, t, i) =>
        val (r, ls0) = i match {
          case None => (List[SJStatement](), ls)
          case Some(x) =>
            val re = JAssignment(n, x)
            re.setPos(b.pos)
            translateStatement(re, fields, ms, ls)
        }
        (r, ls0 + (n -> t))

      case fw@JFieldWrite(va, f, nv) =>
        val (nva, nvi, ls0) = extractHelper(None, va, fields, ms, ls, fw.pos)
        val (vaa, vai, ls1) = extractHelper(None, nv, fields, ms, ls0, fw.pos)
        assert(nva.isInstanceOf[SJVariableAccess])
        val r = SJFieldWrite(nva.asInstanceOf[SJVariableAccess], f, vaa)
        r.setPos(fw.pos)
        (nvi ++ vai ++ List(r), ls1)

      case co@JConditional(t, c, a) =>
        val (at, ti, ls0) = extractHelper(None, t, fields, ms, ls, co.pos)
        val (ci, ls1) = translateStatement(c, fields, ms, ls0)
        val (ai, ls2) = translateStatement(a, fields, ms, ls1)
        val r = SJConditional(at, ci, ai)
        r.setPos(co.pos)
        (ti ++ List(r), ls2)

      case x@JWhile(test, body) =>
        val (at, ti, ls0) = extractHelper(None, test, fields, ms, ls, x.pos)
        val (newbody, ls1) = translateStatement(body, fields, ms, ls0)
        val why = SJWhile(at, newbody ++ ti)
        why.setPos(x.pos)
        (ti ++ List(why), ls1)

      case r@JReturn(e : JVariableAccess) =>
        val re = SJReturn(SJVariableAccess(e.variable))
        re.setPos(r.pos)
        (List(re), ls)

      case r@JReturn(e : JLiteral) =>
        val re = SJReturn(SJLiteral(e.value))
        re.setPos(r.pos)
        (List(re), ls)

      case r@JReturn(e) =>
        val (at, ri, ls0) = extractHelper(None, e, fields, ms, ls, r.pos)
        //assert(at.isInstanceOf[SJVariableAccess])
        val re = SJReturn(at)
        re.setPos(r.pos)
        (ri ++ List(re), ls0)

      case c@JCall(receiver, name, args) =>
        val (ra, ri, ls0) = extractHelper(None, receiver, fields, ms, ls, c.pos)
        var j : Int = 0
        var newa : List[SJExpression] = List[SJExpression]()
        var lsprime = ls0
        var is : List[SJStatement] = List[SJStatement]()
        while (j < args.length) {
          val (a, i, ls1) = extractHelper(None, args(j), fields, ms, lsprime, c.pos)
          newa ::= a
          is ++= i
          lsprime = ls1
          j = j + 1
        }
        val r = SJCall(None, ra, name, newa.reverse)
        r.setPos(c.pos)
        (ri ++ is.reverse ++ List(r), lsprime)

      case p@JPostfixExpression(op, v) =>
        val (a, i, l) = extractHelper(None, p, fields, ms, ls, p.pos)
        //log.warning("postfixhelper " + a + " returning is " + i)
        if (i.takeRight(1)(0).isInstanceOf[SJFieldRead]) {
          val lfr = i.takeRight(1)(0).asInstanceOf[SJFieldRead]
          val l0 = l - (lfr.value.variable)
          (i.dropRight(1), l0)
        } else
          (i, l)

      case as@JAssert(y) =>
        assert(y.isInstanceOf[JExpression])
        val (a, i, ls1) = extractHelper(None, y.asInstanceOf[JExpression], fields, ms, ls, as.pos)
        val r = SJAssert(a)
        r.setPos(as.pos)
        (i ++ List(r), ls1)

      case (x : JSpecExpression)=>
        val res = ParseSpecification.parse(x.e)
        res.setPos(x.pos)
        (List(res), ls)

/*
      //below here there is no real sense
      case JBinaryExpression(op, l, r) =>
        val (larg, lins, ls0) = extractHelper(l, fields, ls)
        val (rarg, rins, ls1) = extractHelper(r, fields, ls0)
        (lins ++ rins ++ List(SJBinaryExpression(op, larg, rarg)), ls1)

      case JUnaryExpression(op, v) =>
        val (va, vis, ls0) = extractHelper(v, fields, ls)
        (vis ++ List(SJUnaryExpression(op, va)), ls0)

      case JNewExpression(name, args) =>
        var j : Int = 0
        var newa : List[SJExpression] = List[SJExpression]()
        var lsprime = ls
        var is : List[SJStatement] = List[SJStatement]()
        while (j < args.length) {
          val (a, i, ls1) = extractHelper(args(j), fields, lsprime)
          newa ::= a
          is ::= i
          lsprime = ls1
          j = j + 1
        }
        (is.reverse ++ List(SJNewExpression(SJVariableAccess(""), name, newa.reverse)), lsprime)
*/
  }

  import dk.itu.sdg.parsing.{LengthPosition, NoLengthPosition}
//  import scala.util.parsing.input.{Position, NoPosition}
  def extractHelper (res : Option[SJVariableAccess], x : JExpression, fields : HashMap[String, String], ms : HashMap[String, String], locals : HashMap[String, String], position : LengthPosition = NoLengthPosition) : (SJExpression, List[SJStatement], HashMap[String, String]) = x match {
      case JBinaryExpression(op, l, r) =>
        val (ri, rii, ls0) = extractHelper(None, r, fields, ms, locals, position)
        val (le, lei, ls1) = extractHelper(None, l, fields, ms, ls0, position)
        (SJBinaryExpression(op, le, ri), rii ++ lei, ls1)

      case JUnaryExpression(op, v) =>
        val (va, vis, ls0) = extractHelper(None, v, fields, ms, locals, position)
        (SJUnaryExpression(op, va), vis, ls0)

      case JPostfixExpression(op, v) =>
        val oper = op match {
          case "++" => "+"
          case "--" => "-"
        }
        v match {
          case JFieldAccess(variable, field) =>
            val a = Gensym.newsym()
            val (arg, ins, ls0) = extractHelper(Some(SJVariableAccess(a)), variable, fields, ms, locals, position)
            val rhs = SJBinaryExpression(oper, SJVariableAccess(a), SJLiteral("1"))
            val (t, ls1) = res match {
              case None =>
                val r = Gensym.newsym();
                (SJVariableAccess(r), ls0 + (r -> "int"))
              case Some(x) => (x, ls0)
            }
            assert(arg.isInstanceOf[SJVariableAccess])
            val a2 = arg.asInstanceOf[SJVariableAccess]
            val r1 = SJFieldRead(SJVariableAccess(a), a2, field)
            r1.setPos(position)
            val r2 = SJFieldWrite(a2, field, rhs)
            r2.setPos(position)
            val r3 = SJFieldRead(t, SJVariableAccess(a), field)
            r3.setPos(position)
            (t,
             ins ++ List(r1, r2, r3),
             ls1 + (a -> "int"))
          case JVariableAccess(x) =>
            val r = SJAssignment(SJVariableAccess(x), SJBinaryExpression(oper, SJVariableAccess(x), SJLiteral("1")))
            r.setPos(position)
            (SJVariableAccess(x), List(r), locals)
        }

      case JFieldAccess(v, f) =>
        val (a, i, ls0) = extractHelper(None, v, fields, ms, locals, position)
        assert(a.isInstanceOf[SJVariableAccess])
        val (r, l) = res match {
          case None =>
            val t = Gensym.newsym()
            val ty = a.asInstanceOf[SJVariableAccess].variable match {
              case y => if (ls0(y) == ls0("this"))
                          fields(f)
                        else
                          SJTable.getClass(ls0(y)).asInstanceOf[SJClassDefinition].fields(f)
            }
            (SJVariableAccess(t), ls0 + (t -> ty))
          case Some(x) => (x, ls0)
        }
        val re = SJFieldRead(r, a.asInstanceOf[SJVariableAccess], f)
        re.setPos(position)
        (r, i ++ List(re), l)

      case JCall(receiver, name, args) =>
        val (a, i, ls0) = extractHelper(None, receiver, fields, ms, locals, position)
        val (as, ins, ls1)  = exL(args, fields, ms, ls0)
        val (r, ls2) = res match {
          case None =>
            val t = Gensym.newsym()
            //log.warning("a is " + a)
            val ty = a.asInstanceOf[SJVariableAccess].variable match {
              case y => if (ls0(y) == ls0("this"))
                          ms(name)
                        else
                          SJTable.getMethodTypeOfClass(ls0(y), name)
            }
            (SJVariableAccess(t), ls1 + (t -> ty))
          case Some(x) => (x, ls1)
        }
        val re = SJCall(Some(r), a, name, as)
        re.setPos(position)
        (r, i ++ ins ++ List(re), ls2)

      case JNewExpression(name, args) =>
        val (as, ins, ls0) = exL(args, fields, ms, locals)
        val (r, ls1) = res match {
          case None =>
            val t = Gensym.newsym()
            (SJVariableAccess(t), ls0 + (t -> name))
          case Some(x) => (x, ls0)
        }
        val re = SJNewExpression(r, name, as)
        re.setPos(position)
        (r, ins ++ List(re), ls1)

      case JConditional(test, c, a) => Nil
        val (r, ls0) = res match {
          case None =>
            log.warning("JConditional: shouldn't happen!, res is None in " + test)
            val t = Gensym.newsym()
            (SJVariableAccess(t), locals + (t -> "Object"))
          case Some(x) => (x, locals)
        }
        val (ta, ti, ls1) = extractHelper(None, test, fields, ms, ls0, position)
        val tb = (a : JBodyStatement) => {
          val ls =
            if (a.isInstanceOf[JBlock])
              a.asInstanceOf[JBlock].body
            else
              List(a)
          assert(ls.takeRight(1)(0).isInstanceOf[JExpression])
          val re = JAssignment(r.variable, ls.takeRight(1)(0).asInstanceOf[JExpression])
          re.setPos(position)
          ls.dropRight(1) ++ List(re)
        }
        val (ca, ls2) = translateBodyInner(tb(c), fields, ms, ls1)
        val (aa, ls3) = translateBodyInner(tb(a), fields, ms, ls2)
        val re = SJConditional(ta, ca, aa)
        re.setPos(position)
        (r, ti ++ List(re), ls3)


    case JLiteral(x) => (SJLiteral(x), List[SJStatement](), locals)

    case JVariableAccess(x) => (SJVariableAccess(x), List[SJStatement](), locals)
  }

  def exL (args : List[JExpression], fields : HashMap[String, String], ms : HashMap[String, String], locals : HashMap[String, String]) : (List[SJExpression], List[SJStatement], HashMap[String, String]) = {
    var as : List[SJExpression] = List[SJExpression]()
    var ls : HashMap[String, String] = locals
    var is : List[SJStatement] = List[SJStatement]()
    var j : Int = 0
    while (j < args.length) {
      val (a, i, l) = extractHelper(None, args(0), fields, ms, ls)
      as ::= a
      is ++= i
      ls = l
      j += 1
    }
    (as, is, ls)
  }
}
