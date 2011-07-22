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
  def mapb (l : List[SJBodyDefinition], i : Int) : List[String] = { l.map(x => outputBodyDef(x, i)) }

  def outputDefinition (x : SJDefinition, ind : Int) : String = {
    x match {
      case SJClassDefinition(modifiers, id, s, i, b, o, f) =>
        val is = if (i.length > 0) " implements " + i.reduceLeft(_ + ", " + _) else " "
        indent(ind) + modifiers.mkString(" ") + " class " + id + is + "{\n" + mr(mapb(b, ind + 2)) + "\n" + indent(ind) + "}"
      case SJInterfaceDefinition(modifiers, id, is, b) =>
        val ints = if (is.length > 0) " extends " + is.mkString(", ") else ""
        indent(ind) + modifiers.mkString(" ") +  " interface " + id + ints + "{\n" + mr(mapb(b, ind + 2)) + "\n" + indent(ind) + "}"
    }
  }

  def outputBodyDef (x : SJBodyDefinition, ind : Int) : String = {
    x match {
      case SJFieldDefinition(modifiers, id, t) => indent(ind) + modifiers.mkString(" ") + " " + t + " " + id + ";"
      case SJMethodDefinition(modifiers, id, typ, ar, bo, ls) =>
        val body = mapi(bo, ind + 2).mkString(";\n")
        val entirebody = if (body.length == 0) " " else "{\n" + body + ";\n" + indent(ind) + "}\n"
        indent(ind) + modifiers.mkString(" ") +  " " + typ + " " + id + " (" + outputArguments(ar) + ") " + entirebody
      case SJConstructorDefinition(modifiers, typ, ar, bo, ls) =>
        val body = mapi(bo, ind + 2).mkString(";\n")
        val entirebody = if (body.length == 0) " " else "{\n" + body + ";\n" + indent(ind) + "}\n"
        indent(ind) + modifiers.mkString(" ") +  " " + typ + " (" + outputArguments(ar) + ") " + entirebody
      case SJBodyBlock(modifier, xs) => {           
        if (xs.length > 1) {
          val mod = modifier match {
            case None => ""
            case Some(x) => "static"
          }
          mod + " {\n" + mapi(xs, ind + 2).mkString(";\n") + ";\n" + indent(ind) + "} "
        } else if (xs.length == 1)
          "\n" + outputStatement(xs(0), ind + 2)
        else
          ""
      }
    }
  }

  def outputArguments (x : List[SJArgument]) : String = {
    x.map(y => y.jtype + " " + y.id).mkString(", ")
  }

  def outputStatement (x : SJStatement, ind : Int) : String = {
    x match {
      case SJAssert(ass) => indent(ind) + "assert(" + outputExpression(ass) + ")"
      case SJWhile(test, body) => indent(ind) + "while (" + outputExpression(test) + ") {\n" + body.map(outputStatement(_, ind + 2)) + "\n" + indent(ind) + "}"
      case SJConditional(test, c, a) => indent(ind) + "if (" + outputExpression(test) + ") {\n" + c.map(outputStatement(_, ind + 2)) + "\n} else {\n" + a.map(outputStatement(_, ind + 2)) + "}"
      case SJAssignment(SJVariableAccess(l), r) => l + " = " + outputExpression(r)
      case SJFieldWrite(SJVariableAccess(l), f, v) => l + "." + f + " = " + outputExpression(v)
      case SJFieldRead(SJVariableAccess(v), SJVariableAccess(l), f) => v + " = " + l + "." + f
      case SJReturn(x) => "return " + outputExpression(x)
      case SJCall(v, r, f, a) =>
        val lhs = v match {
          case None => ""
          case Some(SJVariableAccess(x)) => x + " = "
        }
        lhs + outputExpression(r) + "." + f + "(" + a.map(outputExpression).mkString(", ") + ")"
      case SJNewExpression(SJVariableAccess(v), ty, ar) =>
        v + " = " + "new " + ty + "(" + ar.map(outputExpression).mkString(", ") + ")"
      case x : SJExpression => indent(ind) + outputExpression(x)
    }
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
          case y => try { y.toInt.toString } catch { case e : Exception => "\"" + y + "\"" }
        }
    }
  }
}

object JavaOutput extends JavaOutputter with JavaAST {
  import scala.util.parsing.input.CharArrayReader

  def parseandoutput (s : String) : String = {
    val intermediate = FinishAST.doitHelper(parseH(new CharArrayReader(s.toArray)))
    intermediate.map(outputDefinition(_, 0)).reduceLeft(_ + "\n\n" + _)
  }
}
