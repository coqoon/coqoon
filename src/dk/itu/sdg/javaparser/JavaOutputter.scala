/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

trait JavaOutputter {
  private var myclass : String = ""

  def coqout (loc : String, msg : String) : String = {
    "Coq.def(Coq.M." + loc + ", \"" + msg.replace("\"", "\\\"").replace("\n", "\" +\n \"") + "\");" 
  }

  def red (x : List[String], sep : String) : String = {
    if (x.length == 0)
      ""
    else
      x.reduceLeft(_ + sep + _)
  }

  def mr (x : List[String]) : String = { red(x, "\n") }

  def out (x : JStatement) : String = {
    x match {
      case JClassDefinition(id, s, i, b, o) =>
        myclass = id
        val specp = ClassTable.getCoq("PRELUDE").reverse.map(x => coqout("PRELUDE", x))
        val specpr = ClassTable.getCoq(id, "PROGRAM").reverse.map(x => coqout("PROGRAM", x))
        val specb = ClassTable.getCoq(id, "BEFORESPEC").reverse.map(x => coqout("BEFORESPEC", x))
        val speca = ClassTable.getCoq(id, "AFTERSPEC").reverse.map(x => coqout("AFTERSPEC", x))
        val spect = ClassTable.getCoq("TOP").reverse.map(x => coqout("TOP", x))
        val st = "static {\n" + mr(specp) + mr(spect) + mr(specpr) + mr(specb) + mr(speca) + "\n}"
        val is = if (i.length > 0) " implements " + i.reduceLeft(_ + ", " + _) else " "
        "class " + id + is + "{\n" + st + mr(b.map(out)) + "\n}"
      case JFieldDefinition(id, t) => t + " " + id + ";"
      case JMethodDefinition(id, typ, ar, b) =>
        val sp = ClassTable.getSpecs(myclass)
        val (pre, pos) =
          if (sp.contains(id) && sp(id)._1 != null && sp(id)._2 != null) {
            Console.println("sp contains id (" + id + "): " + sp)
            ("Coq.requires(\"" + sp(id)._1.replace("\"", "\\\"") + "\");\n",
             "Coq.ensures(\"" + sp(id)._2.replace("\"", "\\\"") + "\");\n")
          } else
            ("", "")
        val bo = if (b.length == 1 && b(0).isInstanceOf[JBlock])
                   b(0).asInstanceOf[JBlock].body
                 else
                   b
      typ + " " + id + "(" + red(ar.map(out), ", ") + ") {\n " + pre + pos + red(bo.map(out), ";\n") + ";\n}"
      case JArgument(id, t) => t + " " + id
      case JBlock(xs) => if (xs.length > 1) "{ " + red(xs.map(out), ";\n") + "; }" else if (xs.length == 1) out(xs(0)) else  ""
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
        va + f + "(" + red(as.map(out), ",") + ")"
      case JNewExpression(t, a) => "new " + t + "(" + red(a.map(out), ",") + ")"
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

object JavaOutput extends JavaOutputter with JavaAST {
  import scala.util.parsing.input.CharArrayReader

  def parseandoutput (s : String) : String = {
    val intermediate = FinishAST.doitHelper(parseH(new CharArrayReader(s.toArray)))
    intermediate.map(out).reduceLeft(_ + "" + _)
  }
}
