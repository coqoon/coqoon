/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam.javap

import dk.itu.coqoon.core.coqtop.CoqSentence
import dk.itu.coqoon.core.utilities.{TryCast, Substring}

import dk.itu.sdg.kopitiam.Activator

object JavaASTUtils {

  import org.eclipse.jdt.core.dom.{EmptyStatement, Statement}
  def printProofScript(statement : Statement) : Seq[CoqSentence.Sentence] =
    statement match {
      case x : EmptyStatement =>
        statement.getProperty("dk.itu.sdg.kopitiam.contentExpr") match {
          case script : String if script.length > 0 =>
            val con =
              if (script.contains("invariant:")) {
                val i1 = script.indexOf(":")
                val i2 = script.indexOf("frame:")
                val i3 = if (i2 == -1) script.length else i2
                val i = script.substring(i1 + 1, i3).trim
                val f =
                  if (i2 == -1)
                    "<true>"
                  else
                    script.substring(i3 + 6, script.length).trim
                "forward (" + i + ") (" + f + ")."
              } else script
            CoqSentence.getNextSentences(con, 0, con.length)
          case _ =>
            Nil
        }
      case x : Statement =>
        val fwd = Activator.getDefault.getPreferenceStore.getBoolean("implicit")
        if (fwd)
          Seq((Substring("forward."), false))
        else
          Nil
    }

  import scala.collection.JavaConversions.asScalaBuffer
  import scala.collection.immutable.Stack
  import org.eclipse.jdt.core.dom.{MethodDeclaration, Statement, WhileStatement, IfStatement, Block}
  def traverseAST[A](method : MethodDeclaration, early : Boolean,
      callback : Statement => Seq[A]) : List[A] = {
    var todo = List[Statement](method.getBody)
    val res = List.newBuilder[A]
    var cont = true
    while (!todo.isEmpty && cont) {
      val st = todo.head
      todo = todo.tail
      st match {
        case x : Block =>
          val body =
            asScalaBuffer(x.statements).flatMap(TryCast[Statement]).toList
          todo = body ++ todo
        case x : WhileStatement =>
          todo = x.getBody +: todo
        case x : IfStatement =>
          Option(x.getThenStatement).foreach(so => todo = so +: todo)
          Option(x.getElseStatement).foreach(el => todo = el +: todo)
        case x =>
          val r = if (!early) callback(x) else callback(x).take(1)
          res ++= r
          if (!r.isEmpty && early)
            cont = false
      }
    }
    res.result
  }


  import org.eclipse.jdt.core.dom.{AbstractTypeDeclaration, CompilationUnit, MethodDeclaration, TypeDeclaration}
  def traverseCU [A](c : CompilationUnit, callback : MethodDeclaration => A) : List[A] = {
    var res : List[A] = List[A]()
    var todo : Stack[AbstractTypeDeclaration] = Stack[AbstractTypeDeclaration]()
    todo = todo.pushAll(asScalaBuffer(c.types).flatMap(TryCast[AbstractTypeDeclaration]))
    while (!todo.isEmpty) {
      val t = todo.top
      todo = todo.pop
      t match {
        case x : TypeDeclaration =>
          todo = todo.pushAll(x.getTypes)
          for (m <- x.getMethods)
            res ::= callback(m)
        case _ =>
      }
    }
    res.reverse
  }

  def countMethods (c : CompilationUnit) : Int = traverseCU(c, _ => 1).size
}
