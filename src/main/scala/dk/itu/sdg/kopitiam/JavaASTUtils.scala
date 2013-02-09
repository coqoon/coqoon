/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait JavaASTUtils {

  import org.eclipse.jdt.core.dom.{EmptyStatement, Statement}
  import org.eclipse.jface.text.IDocument
  def printProofScript (document : IDocument, statement : Statement) : Option[String] =
    statement match {
      case x : EmptyStatement =>
        val script = document.get(x.getStartPosition, x.getLength)
        val con =
          if (script.contains("invariant:")) {
            val i1 = script.indexOf(":")
            val i2 = script.indexOf("frame:")
            val i3 = if (i2 == -1) script.length - 3 else i2
            val i = script.substring(i1 + 1, i3).trim
            val f =
              if (i2 == -1)
                "<true>"
              else
                script.substring(i3 + 6, script.length - 3).trim
            "forward (" + i + ") (" + f + ")."
          } else
            script.drop(2).dropRight(2).trim
        Some(con)
      case x : Statement =>
        val fwd = Activator.getDefault.getPreferenceStore.getBoolean("implicit")
        if (fwd)
          Some("forward.")
        else
          None
    }

  import scala.collection.immutable.Stack
  import org.eclipse.jdt.core.dom.{MethodDeclaration, Statement, WhileStatement, IfStatement, Block}
  def traverseAST [A](method : MethodDeclaration, forward : Boolean, early : Boolean, callback : Statement => Option[A]) : List[A] = {
    var todo : Stack[Statement] = Stack[Statement]()
    todo = todo.push(method.getBody)
    var res : List[A] = List[A]()
    var cont : Boolean = true
    while (! todo.isEmpty && cont) {
      val st = todo.top
      todo = todo.pop
      st match {
        case x : Block =>
          val body = scala.collection.JavaConversions.asBuffer(x.statements).map(_.asInstanceOf[Statement])
          if (forward)
            todo = todo.pushAll(body.reverse)
          else
            todo = todo.pushAll(body)
        case x : WhileStatement =>
          todo = todo.push(x.getBody)
        case x : IfStatement =>
          val el = x.getElseStatement
          if (el != null && forward)
            todo = todo.push(el)
          todo = todo.push(x.getThenStatement)
          if (el != null && ! forward)
            todo = todo.push(el)
        case x : Statement =>
          callback(x) match {
            case Some(x) =>
              res ::= x
              if (early)
                cont = false
            case None =>
          }
      }
    }
    res.reverse
  }
}

