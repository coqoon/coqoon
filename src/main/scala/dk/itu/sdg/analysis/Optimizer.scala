/*
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.analysis

import scala.collection.immutable.{ HashSet };
import dk.itu.sdg.javaparser._
import scala.annotation.{ tailrec }

object Optimizer {

  import AST._

  type Variables = HashSet[String]

  case class ReadsAndWrites(reads: Variables = 
    new HashSet(), writes: Variables = new HashSet())

  // 
  // Removes dead variables from an entire program, each method at a time.
  // 
  def removeDeadVariables(definitions: List[SJDefinition]): List[SJDefinition] = {

    def process(xs: List[SJBodyDefinition]): List[SJBodyDefinition] = xs map {
      case x: SJMethodDefinition => liveVariableRewrite(x)
      case x => x
    }

    definitions.map {
      case x: SJClassDefinition => x.copy ( body = process(x.body) )
      case x: SJInterfaceDefinition => x.copy ( body = process(x.body) )
    }
  }

  // 
  // Rewrites the SJMethodDefinition to remove any dead variables.
  // 
  def liveVariableRewrite(method: SJMethodDefinition): SJMethodDefinition = {

    @tailrec
    def rec(remaining:    List[SJStatement],
            currentBlock: List[SJStatement] = Nil,
            processed:    List[SJStatement] = Nil,
            live:         HashSet[String]   = HashSet[String]()): List[SJStatement] = remaining match {

      case Nil =>
        val deadInBlock    = findDeadVariables(live, currentBlock)
        val processedBlock = deadInBlock.map( deads => rewrite(deads, currentBlock, method) )
        processedBlock.getOrElse(currentBlock) ::: processed
      
      case x :: xs =>
        x match {
          case SJConditional(cond, consequent, alternative) =>
          {
            // Process the block after the current SJConditional (i.e. the current block)
            val rwOfBlock = rwOfStatements(currentBlock).getOrElse(ReadsAndWrites())
            val cBlockProcced = findDeadVariables(live, currentBlock).map(rewrite(_, currentBlock, method)).getOrElse(currentBlock)
            val newLive = (rwOfBlock.reads ++ live).filterNot( rwOfBlock.writes.contains(_) )

            // now process the SJConditional.
            val newConsequent  = findDeadVariables(newLive, consequent).map(rewrite(_, consequent, method)).getOrElse(consequent)
            val newAlternative = findDeadVariables(newLive, alternative).map(rewrite(_, alternative, method)).getOrElse(alternative)
            val rwConsequent   = rwOfStatements(consequent).getOrElse(ReadsAndWrites())
            val rwAlternative  = rwOfStatements(alternative).getOrElse(ReadsAndWrites())
            val rwOfCond       = rwOfStatement(cond).getOrElse(ReadsAndWrites())


            val nextLive = newLive.filterNot( x => rwConsequent.writes.contains(x) || rwAlternative.writes.contains(x) ) ++
              (rwOfCond.reads ++ rwConsequent.reads ++ rwAlternative.reads)

            rec(xs,Nil, SJConditional(cond, newConsequent, newAlternative) :: cBlockProcced ::: processed, nextLive)
          }

          case SJWhile(cond, body) =>
          {
            // Process the block after the current SJWhile (i.e. the current block)
            val rwOfBlock = rwOfStatements(currentBlock).getOrElse(ReadsAndWrites())
            val cBlockProcced = findDeadVariables(live, currentBlock).map(rewrite(_, currentBlock, method)).getOrElse(currentBlock)

            // The RW of the condition will always be read after the loop, hence it should be added to the 'live' variables
            val rwOfCond = rwOfStatement(cond).getOrElse(ReadsAndWrites())
            val newIn = live.filterNot( rwOfBlock.writes.contains(_)) ++ (rwOfCond.reads ++ rwOfBlock.reads)

            val rw = rwOfStatements(body).getOrElse(ReadsAndWrites())
            val newBody = findDeadVariables(newIn, body).map(rewrite(_, body, method)).getOrElse(body)

            val nextIn = newIn.filterNot( rw.writes.contains(_)) ++ (rw.reads ++ rwOfCond.reads)
            rec(xs,Nil,SJWhile(cond, newBody) :: cBlockProcced ::: processed, nextIn)
          }

          case stm => rec(xs, x :: currentBlock, processed, live)
        }
    }

    val newBody = rec(method.body.reverse)
    
    // remove any local variables from the methods 'localvariables' that are no longer used.
    val variables = for { (k,v) <- method.localvariables if isUsed(variable = k, in = newBody) } yield (k,v)

    method.copy( body = newBody, localvariables = variables )
  }

  // 
  // the rewrite algorithm. Given a set of dead variables, i.e. variables 
  // that are written in this block but not read in the blocks following 
  // it and a list of SJStatements (representing a block) rewrite the block 
  // to remove any unnecessary temporary variables 
  // 
  def rewrite(deads: HashSet[String], 
              statements: List[SJStatement], 
              method: SJMethodDefinition): List[SJStatement] = {

    rwOfStatements(statements).map { rw =>
      deads.foldLeft(statements){ (rewritten, dead) =>  // for each dead variable: rewrite the AST.
        if ( rw.reads.contains(dead) ) {                // it's read & written
          // it's dead but still read in the block. If it's truly a temporary value that is simply used in _one_
          // other assignment we can replace the assignment to the tmp variable with a assignment to the variable
          // that's using the tmp variable. Also They have to be of the same type.
          val usingDeadVar = findWriteVarsWhereVarIsRead( variable = dead, in = rewritten)
          if (usingDeadVar.size == 1 && hasSameType(dead, usingDeadVar.head, method)) {
            transform(writesOf = dead, toWritesOf = usingDeadVar.head, in = rewritten)
          } else {
            rewritten
          }
        }
        else { // it's written and not read in the block we can just remove it.
          removeWritesOf(variable = dead, in = rewritten)
        }
      }
    } getOrElse(statements)

  }

  // 
  // Given two variables and a method definition it checks if the two 
  // variables have the same type in that method.
  // 
  def hasSameType(x: String, y: String, method: SJMethodDefinition) =
      method.localvariables(x) == method.localvariables(y)

  //
  // Given a list of variable that are read after the list of SJStatements (live) find
  // any dead variables. A variable is dead if it's written but not in the 'live' set. I.e.
  // because the analysis is backwards when we hit this block we know that none of the
  // blocks coming after this one actually reads the variable 
  // 
  def findDeadVariables(live: Set[String], statements: List[SJStatement]): Option[HashSet[String]] = {
    val rw = rwOfStatements(statements).getOrElse(ReadsAndWrites())
    val dead = rw.writes.filterNot(live.contains(_))
    if (!dead.isEmpty) Some(dead) else None
  }

  //
  // Given a SJStatement it will return Some(ReadsAndWrites) with the variable being
  // read/written. None if no variable are read/written
  // 
  def rwOfStatement(statement: SJStatement): Option[ReadsAndWrites] = {
    def recursive(stm: SJStatement, before: Option[ReadsAndWrites]): Option[ReadsAndWrites] = {
      val after = (stm match {

        case SJVariableAccess(r) => 
          Some(ReadsAndWrites( reads = HashSet(r)))

        case SJReturn(SJVariableAccess(r)) => 
          Some(ReadsAndWrites( reads = HashSet(r)))

        case SJFieldRead(SJVariableAccess(w),SJVariableAccess(r),_) => 
          Some(ReadsAndWrites(HashSet(r), HashSet(w)))

        case SJNewExpression(SJVariableAccess(w),_,args) =>
          val writes = Some(ReadsAndWrites( writes = HashSet(w)))
          merge(rwOfStatements(args, writes), before)

        case SJFieldWrite(SJVariableAccess(w),_,expr) => 
          recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))

        case SJAssignment(SJVariableAccess(w),expr) => 
          recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))

        case SJBinaryExpression(_,l,r) => 
          merge(merge(before, recursive(l,None)),recursive(r,None))

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

  // 
  // @see rwOfStatements. Does the same but on a List of SJStatements
  // 
  def rwOfStatements(statements: List[SJStatement], initial: Option[ReadsAndWrites] = Some(ReadsAndWrites())): Option[ReadsAndWrites] = {
    statements.map(rwOfStatement(_)).foldLeft(initial){ (acc, current) => merge(current, acc) }
  }

  // 
  // Merges two option wrapped instances of ReadsAndWrites.
  // 
  private def merge(x: Option[ReadsAndWrites], y: Option[ReadsAndWrites]) = (x,y) match {
    case (None, Some(q))   => Some(q)
    case (Some(q), None)   => Some(q)
    case (None, None)      => None
    case (Some(p),Some(q)) => Some(ReadsAndWrites(p.reads ++ q.reads, p.writes ++ q.writes))
  }
}
