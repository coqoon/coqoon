/*
  Object that contains generic methods related to traversal and transformation of
  the Simple Java AST.

  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.analysis

import dk.itu.sdg.javaparser._

object AST {

  def foldLeft[B](statements: List[SJStatement], z: B, f: (SJStatement,B) => B): B =
    statements.foldLeft(z) { (acc,stm) => foldFunc(stm,acc,f,foldLeft[B]) }

  def foldRight[B](statements: List[SJStatement], z: B, f: (SJStatement,B) => B): B =
    statements.foldRight(z) { (stm,acc) => foldFunc(stm,acc,f,foldRight[B]) }

  private def foldFunc[B](
    stm: SJStatement,
    acc: B,
    f: (SJStatement,B) => B,
    direction: (List[SJStatement], B, (SJStatement,B) => B) => B): B = {
      stm match {
      case a@SJAssert(b)               => f(a,direction(List(b),acc,f))
      case a@SJWhile(b,c)              => f(a,direction(List(b),direction(c,acc,f),f))
      case a@SJConditional(b,c,d)      => f(a,direction(List(b),direction(c,direction(d,acc,f),f),f))
      case a@SJAssignment(b,c)         => f(a,f(b,direction(List(c),acc,f)))
      case a@SJFieldWrite(b,_,c)       => f(a,direction(List(c),acc,f))
      case a@SJFieldRead(b,c,_)        => f(a,f(b,f(c,acc)))
      case a@SJReturn(b)               => f(a,direction(List(b),acc,f))
      case a@SJCall(Some(b),c,_,d)     => f(a,f(b,direction(List(c),direction(d,acc,f),f)))
      case a@SJCall(None,b,_,c)        => f(a,direction(List(b),direction(c,acc,f),f))
      case a@SJNewExpression(b,_,c)    => f(a,f(b,direction(c,acc,f)))
      case a@SJBinaryExpression(_,b,c) => f(a,direction(List(b),direction(List(c),acc,f),f))
      case a@SJUnaryExpression(_,b)    => f(a,direction(List(b),acc,f))
      case x                           => f(x,acc)
    }
  }

  /**
   * Returns the result of applying f to each node in the tree
   * NOTE: It's very important that f only returns the same type as it's input.
   */
  def trans(statements: List[SJStatement],f: SJStatement => SJStatement): List[SJStatement] = {

    val fexpr = f.asInstanceOf[Function1[SJExpression, SJExpression]]
    val facc  = f.asInstanceOf[Function1[SJVariableAccess,SJVariableAccess]]

    def transExpr(expression: SJExpression): SJExpression = expression match {
      case SJBinaryExpression(op,l,r)  => fexpr(SJBinaryExpression(op, fexpr(l),fexpr(r)))
      case SJUnaryExpression (op,expr) => fexpr(SJUnaryExpression(op,fexpr(expr)))
      case expr: SJExpression          => fexpr(expr)
    }

    statements map { _ match {
      case SJAssert(a)                   => f(SJAssert(transExpr(a)))
      case SJWhile(test, body)           => f(SJWhile(transExpr(test),trans(body,f)))
      case SJConditional(test, c, a)     => f(SJConditional(transExpr(test), trans(c,f), trans(a,f)))
      case SJAssignment(l,r)             => f(SJAssignment(facc(l),transExpr(r)))
      case SJFieldWrite(v, field, v2)    => f(SJFieldWrite(facc(v),field,transExpr(v2)))
      case SJFieldRead(v, v2, field)     => f(SJFieldRead(facc(v),facc(v2),field))
      case SJReturn(ret)                 => f(SJReturn(transExpr(ret)))
      case SJCall(x,rec,fun,args)        => f(SJCall(x.map(facc(_)),transExpr(rec),fun,args.map(transExpr)))
      case SJNewExpression(v, typ, args) => f(SJNewExpression(facc(v),typ,args.map(transExpr)))
      case expr: SJExpression            => fexpr(expr)
    }}
  }

  // Convenience methods on ASTs

  def isUsed(variable: String, in: List[SJStatement]) = {
    in exists { stm => isWriting(variable,stm) || isReading(variable,stm) }
  }

  def isWriting(dead: String, statement: SJStatement): Boolean = foldRight(List(statement), false, { (stm: SJStatement, acc: Boolean) =>
    stm match {
      case SJNewExpression(SJVariableAccess(`dead`),_,_) => true
      case SJFieldWrite(SJVariableAccess(`dead`),_,_)    => true
      case SJAssignment(SJVariableAccess(`dead`),_)      => true
      case SJCall(_, SJVariableAccess(`dead`),_,_)       => true
      case x                                             => acc || false
    }
  })

  def isReading(variable: String, in: SJStatement): Boolean = foldRight(List(in), false, { (stm: SJStatement, acc: Boolean) =>
    stm match {
      case SJVariableAccess(`variable`)                  => true
      case SJReturn(SJVariableAccess(`variable`))        => true
      case SJFieldRead(SJVariableAccess(`variable`),_,_) => true
      case x                                             => acc || false
    }
  })

  def transform(writesOf: String, toWritesOf: String, in: List[SJStatement]) = trans(in, _ match {
    case SJVariableAccess(`writesOf`) => SJVariableAccess(toWritesOf)
    case x => x
  })

  def removeWritesOf(variable: String, in: List[SJStatement]) = {
    // It's only safe to remove writes of a variables if it might produce side-effects
    in filterNot { stm => isWriting(variable,stm) && !(stm.isInstanceOf[SJCall] || stm.isInstanceOf[SJNewExpression]) }
  }

  /**
   * Find all the SJStatements in a List of SJStatement where the given variable is
   * written.
   */
  def findStmsWithWritesOf(variable: String, in: List[SJStatement]): List[SJStatement] = {
    foldRight(in, Nil: List[SJStatement], (stm:SJStatement, acc: List[SJStatement]) => {
      if (isWriting(variable,stm)) stm :: acc else acc
    })
  }

  /**
   * Traverse the AST and find the name of all the variables that are written where the value
   * of 'variable' is read to produce the written value.
   */
  def findWriteVarsWhereVarIsRead(variable: String, in: List[SJStatement]): List[String] = {

    val isReadingVariable = isReading(variable,_: SJStatement)

    def rec(stm: SJStatement): List[String] = stm match {
      case SJCall(List(SJVariableAccess(w)), SJVariableAccess(`variable`),_,args) => List(w)
      case SJCall(List(SJVariableAccess(w)), SJVariableAccess(r),_,args)          => if (args.map(isReadingVariable).contains(true)) List(w) else Nil
      case SJNewExpression(SJVariableAccess(w),_,args)                            => if (args.map(isReadingVariable).contains(true)) List(w) else Nil
      case SJFieldWrite(SJVariableAccess(w),_,expr)                               => if (isReadingVariable(expr)) List(w) else Nil
      case SJAssignment(SJVariableAccess(w),expr)                                 => if (isReadingVariable(expr)) List(w) else Nil
      case SJBinaryExpression(_,l,r) =>
        val (x,y) = (rec(l),rec(r))
        if (x.isEmpty) { if (y.isEmpty) Nil else y } else x
      case x => Nil
    }
    foldRight(in, Nil: List[String], (stm: SJStatement,acc: List[String]) => rec(stm) ::: acc )
  }
}
