/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

trait JavaOutputter {
  private var myclass : String = ""

  def coqout (loc : String, msg : String, ind : Int) : String = {
    val spaces = indent(ind) + indent(8)
    val replace = "\" +\n" + spaces + "\""
    indent(ind) + "Coq.def(Coq.M." + loc + ",\n" + spaces + "\"" + msg.replace("\"", "\\\"").replace("\n", replace) + "\");\n" 
  }

  def red (x : List[String], sep : String) : String = {
    if (x.length == 0)
      ""
    else
      x.reduceLeft(_ + sep + _)
  }

  def mr (x : List[String]) : String = { red(x, "\n") }

  def indent (i : Int) : String = {
    if (i > 16)
      indent(16) + indent(i - 16)
    else
      "                ".take(i)
  }

  def mapi (l : List[JStatement], i : Int) : List[String] = { l.map(x => out(x, i)) }

  def out (x : JStatement, ind : Int) : String = {
    x match {
      case JClassDefinition(id, s, i, b, o) =>
        myclass = id
        val specp = ClassTable.getCoq("PRELUDE").reverse.map(x => coqout("PRELUDE", x, 4))
        val specpr = ClassTable.getCoq(id, "PROGRAM").reverse.map(x => coqout("PROGRAM", x, 4))
        val specb = ClassTable.getCoq(id, "BEFORESPEC").reverse.map(x => coqout("BEFORESPEC", x, 4))
        val speca = ClassTable.getCoq(id, "AFTERSPEC").reverse.map(x => coqout("AFTERSPEC", x, 4))
        val spect = ClassTable.getCoq("TOP").reverse.map(x => coqout("TOP", x, 4))
        val stacon = mr(specp) + mr(spect) + mr(specpr) + mr(specb) + mr(speca)
        val st =
          if (stacon.length > 0)
            indent(ind + 2) + "static {\n" + stacon + "\n" + indent(ind + 2) + "}\n"
          else
            ""
        val is = if (i.length > 0) " implements " + i.reduceLeft(_ + ", " + _) else " "
        indent(ind) + "class " + id + is + "{\n" + st + mr(mapi(b, ind + 2)) + "\n" + indent(ind) + "}"
      case JFieldDefinition(id, t) => indent(ind) + t + " " + id + ";"
      case JMethodDefinition(id, typ, ar, b) =>
        val sp = ClassTable.getSpecs(myclass)
        val (pre, pos) =
          if (sp.contains(id) && sp(id)._1 != null && sp(id)._2 != null) {
            //Console.println("sp contains id (" + id + "): " + sp)
            (indent(ind + 2) + "Coq.requires(\"" + sp(id)._1.replace("\"", "\\\"") + "\");\n",
             indent(ind + 2) + "Coq.ensures(\"" + sp(id)._2.replace("\"", "\\\"") + "\");\n")
          } else
            ("", "")
        val bo = if (b.length == 1 && b(0).isInstanceOf[JBlock])
                   b(0).asInstanceOf[JBlock].body
                 else
                   b
        val body = red(mapi(bo, ind + 2), ";\n")
        val completebody = pre + pos + body
        val entirebody = if (completebody.length == 0) " " else "\n" + completebody + ";\n" + indent(ind)
        indent(ind) + typ + " " + id + " (" + red(mapi(ar, 0), ", ") + ") {" + entirebody + "}\n"
      case JArgument(id, t) => indent(ind) + t + " " + id
      case JBlock(xs) =>
        if (xs.length > 1)
          " {\n" + red(mapi(xs, ind + 2), ";\n") + ";\n" + indent(ind) + "} "
        else if (xs.length == 1)
          "\n" + out(xs(0), ind + 2)
        else  ""
      case JAssignment(l, r) => indent(ind) + l + " = " + out(r, 0)
      case JFieldWrite(va, f, v) => indent(ind) + out(va, 0) + "." + f + " = " + out(v, 0)
      case JReturn(r) => indent(ind) + "return " + out(r, 0)
      case JBinding(n, t, in) =>
        val init =
          in match {
            case None => ""
            case Some(x) => " = " + out(x, 0)
          }
        indent(ind) + t + " " + n + init
      case JWhile(t,b) => indent(ind) + "while (" + out(t, 0) + ")" + out(b, ind)
      case JConditional(t, c, a) =>
        val alt = out(a, ind)
        val altb = if (alt.length == 0) "" else ";\n" + indent(ind) + "else" + alt
        indent(ind) + "if (" + out(t, 0) + ")" + out(c, ind) + altb
      case JBinaryExpression(op, l, r) => indent(ind) + out(l, 0) + " " + op + " " + out(r, 0)
      case JUnaryExpression(op, e) => indent(ind) + op + out(e, 0)
      case JPostfixExpression(op, e) => indent(ind) + out(e, 0) + op
      case JCall(v, f, as) =>
        val va = if (v != "this") v + "." else ""
        indent(ind) + va + f + "(" + red(mapi(as, 0), ",") + ")"
      case JNewExpression(t, a) =>
        indent(ind) + "new " + t + "(" + red(mapi(a, 0), ",") + ")"
      case JLiteral(x) =>
        try {
          indent(ind) + x.toInt.toString
        } catch {
          case e : Exception =>
            //todo: handle literal char and boolean at least
            val res = if (x == "null") "null" else "\"" + x + "\"" //are there more reserved words?
            indent(ind) + res
        }
      case JVariableAccess(x) => indent(ind) + x
      case JFieldAccess(v, f) => indent(ind) + out(v, 0) + "." + f
    }
  }
}

object JavaOutput extends JavaOutputter with JavaAST {
  import scala.util.parsing.input.CharArrayReader

  def parseandoutput (s : String) : String = {
    val intermediate = FinishAST.doitHelper(parseH(new CharArrayReader(s.toArray)))
    intermediate.map(out(_, 0)).reduceLeft(_ + "\n\n" + _)
  }
}
