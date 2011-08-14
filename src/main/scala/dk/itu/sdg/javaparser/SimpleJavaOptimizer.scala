/* 
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet };

/** 
 * Object that contains generic methods related to traversal and transformation of 
 * the Simple Java AST.
 */
object AST {
  
  /** 
   * Fold over the AST 
   */
  def foldRight[B](statements: List[SJStatement], z: B, f: (SJStatement,B) => B): B = statements.foldRight(z){ 
    (stm,acc) => {
      stm match {
      case a@SJAssert(b)               => f(a,foldRight(List(b),acc,f))
      case a@SJWhile(b,c)              => f(a,foldRight(List(b),foldRight(c,acc,f),f))
      case a@SJConditional(b,c,d)      => f(a,foldRight(List(b),foldRight(c,foldRight(d,acc,f),f),f))
      case a@SJAssignment(b,c)         => f(a,f(b,foldRight(List(c),acc,f)))
      case a@SJFieldWrite(b,_,c)       => f(a,foldRight(List(c),acc,f))
      case a@SJFieldRead(b,c,_)        => f(a,f(b,f(c,acc)))
      case a@SJReturn(b)               => f(a,foldRight(List(b),acc,f))
      case a@SJCall(Some(b),c,_,d)     => f(a,f(b,foldRight(List(c),foldRight(d,acc,f),f)))
      case a@SJCall(None,b,_,c)        => f(a,foldRight(List(b),foldRight(c,acc,f),f))
      case a@SJNewExpression(b,_,c)    => f(a,f(b,foldRight(c,acc,f)))
      case a@SJBinaryExpression(_,b,c) => f(a,foldRight(List(b),foldRight(List(c),acc,f),f))
      case a@SJUnaryExpression(_,b)    => f(a,foldRight(List(b),acc,f))
      case x                           => f(x,acc)
    }}
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
  
  // Convenince methods on ASTs
  
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
    for { stm <- in if !isWriting(variable,stm) } yield stm 
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

object SimpleJavaOptimizer {

  import AST._

  type Variables = HashSet[String]
    
  case class ReadsAndWrites(reads: Variables = new HashSet(), writes: Variables = new HashSet()) 
  
  /** 
   * Rewrites the SJMethodDefinition to remove any dead variables. 
   */
  def liveVariableRewrite(method: SJMethodDefinition): SJMethodDefinition = {
    
    // the rewrite algorithm 
    def rewrite(deads: HashSet[String], statements: List[SJStatement]): List[SJStatement] = {
      rwOfStatements(statements).map { rw => 
        deads.foldLeft(statements){ (rewritten, dead) =>  // for each dead variable: rewrite the AST. 
          if ( rw.reads.contains(dead) ) {                // it's read & written
            // it's dead but still read in the block. If it's truely a temporary value that is simply used in _one_
            // other assignment we can replace the assignment to the tmp variable with a assignment to the variable
            // that's using the tmp variable.
            // Also, the variable shouldn't have a previous value. (TODO: Not currently checked)
            val usingDeadVar = findWriteVarsWhereVarIsRead( variable = dead, in = rewritten)
            if (usingDeadVar.size == 1) {
              transform(writesOf = dead, toWritesOf = usingDeadVar.head, in = rewritten)
            } else {
              rewritten
            }
          }
          else {                                            // it's written and not read in the block we can just remove it.
            removeWritesOf(variable = dead, in = rewritten) 
          }
        }
      } getOrElse(statements)
    }
    
    // the traversal of the AST by block.     
    def rec(remaining:    List[SJStatement], 
            currentBlock: List[SJStatement] = Nil,
            processed:    List[SJStatement] = Nil, 
            in:           HashSet[String]   = HashSet[String]()): List[SJStatement] = remaining match {
      case Nil => 
        val cBlockProcced = findDeadVariables(in, currentBlock).map(rewrite(_, currentBlock)).getOrElse(currentBlock)
        cBlockProcced ::: processed
      case x :: xs => 
        x match {
          case SJConditional(cond, consequent, alternative) =>
          {
            // Process the block after the current SJConditional (i.e. the current block)
            val rwOfBlock = rwOfStatements(currentBlock).getOrElse(ReadsAndWrites())
            val cBlockProcced = findDeadVariables(in, currentBlock).map(rewrite(_, currentBlock)).getOrElse(currentBlock)
            val newIn = (rwOfBlock.reads ++ in).filterNot( rwOfBlock.writes.contains(_))
            
            // now process the SJConditional. 
            val newConsequent  = findDeadVariables(newIn, consequent).map(rewrite(_, consequent)).getOrElse(consequent)
            val newAlternative = findDeadVariables(newIn, alternative).map(rewrite(_, alternative)).getOrElse(alternative)
            val rwConsequent   = rwOfStatements(consequent).getOrElse(ReadsAndWrites())
            val rwAlternative  = rwOfStatements(alternative).getOrElse(ReadsAndWrites())
            val rwOfCond       = rwOfStatement(cond).getOrElse(ReadsAndWrites())
            
            
            val nextIn = newIn.filterNot( x => rwConsequent.writes.contains(x) || rwAlternative.writes.contains(x) ) ++ 
              (rwOfCond.reads ++ rwConsequent.reads ++ rwAlternative.reads)
            rec(xs,Nil, SJConditional(cond, newConsequent, newAlternative) :: cBlockProcced ::: processed,nextIn)
          }
          case SJWhile(cond, body) => 
          {
            // Process the block after the current SJWhile (i.e. the current block)
            val rwOfBlock = rwOfStatements(currentBlock).getOrElse(ReadsAndWrites())
            val cBlockProcced = findDeadVariables(in, currentBlock).map(rewrite(_, currentBlock)).getOrElse(currentBlock)
            
            // The RW of the condition will always be read after the loop, hence it should be added to the 'in' variables 
            val rwOfCond = rwOfStatement(cond).getOrElse(ReadsAndWrites()) 
            val newIn = in.filterNot( rwOfBlock.writes.contains(_)) ++ (rwOfCond.reads ++ rwOfBlock.reads)
            
            val rw = rwOfStatements(body).getOrElse(ReadsAndWrites())
            val newBody = findDeadVariables(newIn, body).map(rewrite(_, body)).getOrElse(body)

            val nextIn = newIn.filterNot( rw.writes.contains(_)) ++ (rw.reads ++ rwOfCond.reads)
            rec(xs,Nil,SJWhile(cond, newBody) :: cBlockProcced ::: processed, nextIn)
          }
          case stm => rec(xs, x :: currentBlock, processed, in)
        }
    }
    
    val newBody = rec(method.body.reverse) 
    // remove any local variables from the methods 'localvariables' that are no longer used. 
    val variables = for { (k,v) <- method.localvariables if isUsed(variable = k, in = newBody) } yield (k,v)
    
    method.copy( body = newBody, localvariables = variables )
  }
  
  /** 
   * Given a list of variable that are read after the list of SJStatement (in) find
   * any dead variables. A variable is dead if it's written but not in the 'in' set. I.e.
   * the following blocks don't read it. 
   */
  def findDeadVariables(in: Set[String], statements: List[SJStatement]) = {
    val rw = rwOfStatements(statements).getOrElse(ReadsAndWrites())
    val dead = rw.writes.filterNot(in.contains(_))
    if (!dead.isEmpty) Some(dead) else None 
  }
  
  /** 
   * Given a SJStatement it will return Some(ReadsAndWrites) with the variable being 
   * read/written. None if no variable are read/written
   */
  def rwOfStatement(statement: SJStatement): Option[ReadsAndWrites] = {    
    def recursive(stm: SJStatement, before: Option[ReadsAndWrites]): Option[ReadsAndWrites] = {
      val after = (stm match {
        case SJVariableAccess(r) => Some(ReadsAndWrites( reads = HashSet(r)))
        case SJReturn(SJVariableAccess(r)) => Some(ReadsAndWrites( reads = HashSet(r)))
        case SJFieldRead(SJVariableAccess(w),SJVariableAccess(r),_) => Some(ReadsAndWrites(HashSet(r), HashSet(w)))
        case SJNewExpression(SJVariableAccess(w),_,args) => 
          val writes = Some(ReadsAndWrites( writes = HashSet(w)))
          merge(rwOfStatements(args, writes), before)
        case SJFieldWrite(SJVariableAccess(w),_,expr) => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJAssignment(SJVariableAccess(w),expr) => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJBinaryExpression(_,l,r) => merge(merge(before, recursive(l,None)),recursive(r,None))
        case SJCall(value, a:SJVariableAccess,_,args) => 
          val reads = HashSet(a.variable)
          val rw = value.map( w => ReadsAndWrites( reads = reads, writes = HashSet(w.variable)) ).getOrElse(ReadsAndWrites(reads = reads))
          merge(rwOfStatements(args, Option(rw)), before)
        case _ => None // don't care about the SJStatements that doesn't read/write variables 
      })
      merge(before,after)
    }  
    recursive(statement, None)
  }
  
  /** 
   * @see rwOfStatements. Does the same but on a List of SJStatements  
   */
  def rwOfStatements(statements: List[SJStatement], initial: Option[ReadsAndWrites] = Some(ReadsAndWrites())): Option[ReadsAndWrites] = {
    statements.map(rwOfStatement(_)).foldLeft(initial){ (acc, current) => merge(current, acc) }
  }
  
  /** 
   * Merges two option wrapped instances of ReadsAndWrites. 
   */
  private def merge(x: Option[ReadsAndWrites], y: Option[ReadsAndWrites]) = (x,y) match {
    case (None, Some(q))   => Some(q)
    case (Some(q), None)   => Some(q)
    case (None, None)      => None 
    case (Some(p),Some(q)) => Some(ReadsAndWrites(p.reads ++ q.reads, p.writes ++ q.writes))
  }
}
