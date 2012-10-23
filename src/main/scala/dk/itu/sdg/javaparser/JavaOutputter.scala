/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

trait JavaOutputter {
  def mr (x : List[String]) : String = { x.mkString("\n") }

  def indent (i : Int) : String = {
    if (i > 16)
      indent(16) + indent(i - 16)
    else
      "                ".take(i)
  }

  def mapi (l : List[SJStatement], i : Int) : List[String] = { l.map(x => outputStatement(x, i)) }
  def mapb (l : List[SJBodyDefinition], i : Int, cl : Boolean) : List[String] = { l.map(x => outputBodyDef(x, i, cl)) }
  def mapm (l : Set[JModifier]) : List[String] = { l.flatMap(JModifier.unapply(_)).toList }


  import scala.collection.immutable.HashMap
  def printlocals (ls : HashMap[String, String], args : List[SJArgument], ind : Int) : String = {
    ls.flatMap(x => x match {
      case (k, t) => if (! args.map(_.id).contains(k) && k != "this") Some(indent(ind) + t + " " + k) else None
    }).mkString(";\n")
  }


  def outputDefinition (x : SJDefinition, ind : Int) : String = {
    x match {
      case SJClassDefinition(modifiers, id, s, i, b, o, f) =>
        val is = if (i.length > 0) " implements " + i.reduceLeft(_ + ", " + _) else " "
        indent(ind) + mapm(modifiers).mkString(" ") + " class " + id + is + "{\n" + mr(mapb(b, ind + 2, true)) + "\n" + indent(ind) + "}"
      case SJInterfaceDefinition(modifiers, id, is, b) =>
        val ints = if (is.length > 0) " extends " + is.mkString(", ") else ""
        indent(ind) + mapm(modifiers).mkString(" ") +  " interface " + id + ints + "{\n" + mr(mapb(b, ind + 2, false)) + "\n" + indent(ind) + "}"
    }
  }

  def concatBody (body : List[String]) : String = {
    if (body.length == 0)
      ""
    else
      body.reduceLeft((x, y) =>
        if (y.endsWith("%>"))
          if (x.endsWith("%>") || x.endsWith("}"))
            x + "\n" + y
          else
            x + " " + y.trim
        else
          x + "\n" + y)
  }

  def outputBodyDef (x : SJBodyDefinition, ind : Int, cl : Boolean) : String = {
    x match {
      case SJFieldDefinition(modifiers, id, t) => indent(ind) + mapm(modifiers).mkString(" ") + " " + t + " " + id + ";"
      case SJMethodDefinition(modifiers, id, typ, ar, bo, ls) =>
        val vars = printlocals(ls, ar, ind + 2)
        val body = concatBody(mapi(bo, ind + 2))
        val rv = if (vars.length > 0) vars + ";\n" else ""
        val rb = if (body.length > 0) body + "\n" else ""
        val entirebody = if (cl) "{\n" + rv + rb + indent(ind) + "}\n" else " "
        indent(ind) + mapm(modifiers).mkString(" ") +  " " + typ + " " + id + " (" + outputArguments(ar) + ") " + entirebody
      case SJConstructorDefinition(modifiers, typ, ar, bo, ls) =>
        val vars = printlocals(ls, ar, ind + 2)
        val body = concatBody(mapi(bo, ind + 2))
        val rv = if (vars.length > 0) vars + ";\n" else ""
        val rb = if (body.length > 0) body + "\n" else ""
        val entirebody = if (cl) "{\n" + rv + rb + indent(ind) + "}\n" else " "
        indent(ind) + mapm(modifiers).mkString(" ") +  " " + typ + " (" + outputArguments(ar) + ") " + entirebody
      case SJBodyBlock(modifier, xs) =>
        val mod = modifier match {
          case None => ""
          case Some(x) => "static"
        }
        mod + " {\n" + concatBody(mapi(xs, ind + 2)) + "\n" + indent(ind) + "} "
      case Quantification(x) => indent(ind) + "<% lvars: " + x + " %>"
      case Precondition(x) => indent(ind) + "<% requires: " + x + " %>"
      case Postcondition(x) => indent(ind) + "<% ensures: " + x + " %>"
    }
  }

  def outputArguments (x : List[SJArgument]) : String = {
    x.map(y => y.jtype + " " + y.id).mkString(", ")
  }

  def outputStatement (x : SJStatement, ind : Int) : String = {
    val r =
    x match {
      case SJAssert(ass) => "assert(" + outputExpression(ass) + ");"
      case SJWhile(test, body) => "while (" + outputExpression(test) + ") {\n" + concatBody(body.map(outputStatement(_, ind + 2))) + "\n" + indent(ind) + "}"
      case SJConditional(test, c, a) => "if (" + outputExpression(test) + ") {\n" + concatBody(c.map(outputStatement(_, ind + 2))) + "\n" + indent(ind) + "} else {\n" + concatBody(a.map(outputStatement(_, ind + 2))) + "\n" + indent(ind) + "}"
      case SJAssignment(SJVariableAccess(l), r) => l + " = " + outputExpression(r) + ";"
      case SJFieldWrite(SJVariableAccess(l), f, v) => l + "." + f + " = " + outputExpression(v) + ";"
      case SJFieldRead(SJVariableAccess(v), SJVariableAccess(l), f) => v + " = " + l + "." + f + ";"
      case SJReturn(x) => "return " + outputExpression(x) + ";"
      case SJCall(v, r, f, a) =>
        val lhs = v match {
          case None => ""
          case Some(SJVariableAccess(x)) => x + " = "
        }
        val vari = outputExpression(r)
        //that's wrong, we should look for whether it is a static method!
        //and if so, use the classname (similar to CoqOutputter)
        val varia = if (vari == "this") "" else vari + "."
        lhs + varia + f + "(" + a.map(outputExpression).mkString(", ") + ");"
      case SJNewExpression(SJVariableAccess(v), ty, ar) =>
        v + " = " + "new " + ty + "(" + ar.map(outputExpression).mkString(", ") + ");"
      case Loopinvariant(x, f) => "<% invariant: " + x + " frame: " + f + " %>"
      case RawSpecification(x) => "<% " + x + " %>"
      case x : SJExpression => outputExpression(x) + ";"
    }
    indent(ind) + r
  }

  def outputExpression (x : SJExpression) : String = {
    x match {
      case SJBinaryExpression(op, l, r) => outputExpression(l) + " " + op + " " + outputExpression(r)
      case SJUnaryExpression(op, e) => op + outputExpression(e)
      case SJVariableAccess(v) => v
      case SJLiteral(x) =>
        x match {
          case "true" => "true"
          case "false" => "false"
          case "null" => "null"
          case y => try { y.toInt.toString } catch { case e : Throwable => "\"" + y + "\"" }
        }
    }
  }
}

object JavaOutput extends JavaOutputter with JavaAST {
  import scala.util.parsing.input.CharArrayReader

  def parseandoutput (s : String) : String = {
    val intermediate = FinishAST.doitHelper(parseH(new CharArrayReader(s.toArray)))
    val ws = SimpleJavaChecker.check(intermediate)
    //do sth with the warnings, maybe!?
    intermediate.map(outputDefinition(_, 0)).reduceLeft(_ + "\n\n" + _)
  }
}
