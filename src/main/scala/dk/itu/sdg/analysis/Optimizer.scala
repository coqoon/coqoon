/* 
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet };
import dk.itu.sdg.javaparser._

object Optimizer {

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
            // it's dead but still read in the block. If it's truly a temporary value that is simply used in _one_
            // other assignment we can replace the assignment to the tmp variable with a assignment to the variable
            // that's using the tmp variable. Also They have to be of the same type. 
            val usingDeadVar = findWriteVarsWhereVarIsRead( variable = dead, in = rewritten)
            if (usingDeadVar.size == 1 && hasSameType(dead, usingDeadVar.head)) {
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
    
    def hasSameType(x: String, y: String) = 
      method.localvariables(x) == method.localvariables(y)
        
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
