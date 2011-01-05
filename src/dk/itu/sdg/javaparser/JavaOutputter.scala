/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

object JavaOutputter {
  private var myclass : String = ""

  def coqout (loc : String, msg : String) : String = {
    "Coq.def(Coq.M." + loc + ", \"" + msg.replace("\"", "\\\"").replace("\n", "\" +\n \"") + "\");" 
  }

  def out (x : JStatement) : String = { //add pepper: specs according to CT (+ static blocks for CT.coqstuff)
    x match {
      case JClassDefinition(id, s, i, b, o) =>
        myclass = id
        val specp = ClassTable.getCoq("PRELUDE").reverse.map(x => coqout("PRELUDE", x))
        val specpr = ClassTable.getCoq(id, "PROGRAM").reverse.map(x => coqout("PROGRAM", x))
        val specb = ClassTable.getCoq(id, "BEFORESPEC").reverse.map(x => coqout("BEFORESPEC", x))
        val speca = ClassTable.getCoq(id, "AFTERSPEC").reverse.map(x => coqout("AFTERSPEC", x))
        val spect = ClassTable.getCoq("TOP").reverse.map(x => coqout("TOP", x))
        val st = "static {\n" + specp.reduceLeft(_ + "\n" + _) + spect.reduceLeft(_ + "\n" + _) + specpr.reduceLeft(_ + "\n" + _) + specb.reduceLeft(_ + "\n" + _) + speca.reduceLeft(_ + "\n" + _) + "\n}"
        val is = if (i.length > 0) " implements " + i.reduceLeft(_ + ", " + _) else " "
        "class " + id + is + "{\n" + st + b.map(out).reduceLeft(_ + "\n" + _) + "\n}"
      case JFieldDefinition(id, t) => t + " " + id + ";"
      case JMethodDefinition(id, ar, b) =>
        val sp = ClassTable.getSpecs(myclass)
        val pre = "Coq.requires(\"" + sp(id)._1.replace("\"", "\\\"") + "\");\n"
        val pos = "Coq.ensures(\"" + sp(id)._2.replace("\"", "\\\"") + "\");\n"
        //XXX: return type!
        val bo = if (b.length == 1 && b(0).isInstanceOf[JBlock])
                   b(0).asInstanceOf[JBlock].body
                 else
                   b
        "int " + id + "(" + ar.map(out).reduceLeft(_ + ", " + _) + ") {\n " + pre + pos + bo.map(out).reduceLeft(_ + ";\n" + _) + "\n}"
      case JArgument(id, t) => t + " " + id
      case JBlock(xs) => if (xs.length > 1) "{ " + xs.map(out).reduceLeft(_ + ";\n" + _) + "; }" else if (xs.length == 1) out(xs(0)) else  ""
      case JAssignment(l, r) => l + " = " + out(r)
      case JFieldWrite(va, f, v) => out(va) + "." + f + " = " + out(v)
      case JReturn(r) => "return " + out(r)
      case JBinding(n, t, in) =>
        val init = in match { case None => ""
                              case Some(x) => " = " + out(x) }
          t + " " + n + init
      case JWhile(t,b) => "while (" + out(t) + ") " + out(b)
      case JConditional(t, c, a) => "if (" + out(t) + ") " + out(c) + " else " + out(a)
      case JBinaryExpression(op, l, r) => out(l) + " " + op + " " + out(r)
      case JUnaryExpression(op, e) => op + out(e)
      case JPostfixExpression(op, e) => out(e) + op
      case JCall(v, f, as) =>
        val va = if (v != "this") v + "." else ""
        va + f + "(" + as.map(out).reduceLeft(_ + "," + _) + ")"
      case JNewExpression(t, a) => "new " + t + "(" + a.map(out).reduceLeft(_ + "," + _) + ")"
      case JLiteral(x) =>
        try {
          x.toInt.toString
        } catch {
          case e : Exception => "\"" + x + "\""
        }
      case JVariableAccess(x) => x
      case JFieldAccess(v, f) => v + "." + f
    }
  }
}
