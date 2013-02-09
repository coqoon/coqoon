/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait JavaASTUtils {

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
        case x : IfStatement =>
          val el = x.getElseStatement
          if (el != null && forward)
            todo = todo.push(el)
          todo = todo.push(x.getThenStatement)
          if (el != null && ! forward)
            todo = todo.push(el)
        case x : WhileStatement =>
          todo = todo.push(x.getBody)
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

