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

  case class IfPullOut(
    replaceUsesOf: String,
    withUsesOf: String,
    pullOut: SJStatement,
    remove: List[SJStatement]
  )

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

          case cond: SJConditional => {
            // Process the block 'after' the conditional
            val rwOfBlock = rwOfStatements(currentBlock).getOrElse(ReadsAndWrites())
            val cBlockProcced = findDeadVariables(live, currentBlock).map(rewrite(_, currentBlock, method)).getOrElse(currentBlock)
            val newLive = (rwOfBlock.reads ++ live).filterNot( rwOfBlock.writes.contains(_) )

            val (prelude, newCond1) = removeSuperfluousInConditional(cond, newLive, method)
            val (newCond2, newCBlock) = removeSuperfluousByTernary(newCond1, cBlockProcced)

            val SJConditional(test, consequent, alternative) = newCond2

            val rwConsequent   = rwOfStatements(consequent).getOrElse(ReadsAndWrites())
            val rwAlternative  = rwOfStatements(alternative).getOrElse(ReadsAndWrites())
            val rwOfTest       = rwOfStatement(test).getOrElse(ReadsAndWrites())

            val nextLive = live.filterNot( x => rwConsequent.writes.contains(x) || rwAlternative.writes.contains(x) ) ++
              (rwOfTest.reads ++ rwConsequent.reads ++ rwAlternative.reads)

            rec(xs, Nil, prelude ::: (List(newCond2)) ::: newCBlock ::: processed, nextLive)
          }

          case SJWhile(cond, body) => {
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
  // Remove any superfluous temporary variables introduced by the ternary operator
  //
  // If the ternary operator is used in a field/variable assignment then this will
  // produce a Simple Java Program with an if-statement where both branches writes
  // to a temporary variable and straight after the if-statement it will use that
  // temporary variable in a field/variable assignment
  //
  // In this case we can get rid of the temporary variable by moving the assignment
  // into both branches.
  //
  def removeSuperfluousByTernary(
    conditional: SJConditional,
    blockAfterCond: List[SJStatement]): (SJConditional, List[SJStatement]) = {

    val otherwise = (conditional, blockAfterCond)

    val result = blockAfterCond.headOption.map {
      case fw @ SJFieldWrite(SJVariableAccess(writeTo), field, SJVariableAccess(read)) if read.matches("""tmp_(\d*)""") => {

        val replace = (x: SJStatement) => x match {
          case SJAssignment(SJVariableAccess(`read`), expr) =>
            SJFieldWrite(SJVariableAccess(writeTo), field, expr)
          case stm => stm
        }

        val newConsequent  = conditional.consequent.map( replace )
        val newAlternative = conditional.alternative.map( replace )
        val newConditional = SJConditional(conditional.test, newConsequent, newAlternative)

        (newConditional, blockAfterCond.filterNot( _ == fw ) )
      }
      case stm => otherwise
    }

    result.getOrElse( otherwise )
  }

  //
  // Remove any superfluous temporary variables introduced by field reads in if-
  // statement branches.
  //
  // If the same field of an object is read in both branches it will introduce two
  // temporary variables where just one would be sufficient; In this case we simply
  // move the statement that reads the field in the 'consequent' branch out and place
  // it before the conditional. We then remove the SJField reads in both branches and
  // rename the use of the variable in the 'alternative' branch.
  //
  def removeSuperfluousInConditional(
    conditional: SJConditional,
    live: HashSet[String],
    method: SJMethodDefinition): (List[SJStatement], SJConditional) = {

    val SJConditional(cond, consequent, alternative) = conditional

    val deadInConsequent  = findDeadVariables(live, consequent)
    val deadInAlternative = findDeadVariables(live, alternative)

    val stmsWrintingToDeadInCons = deadInConsequent.map(_.flatMap( findStatementsWritingTo(_, consequent) ).toList )
    val stmsWrintingToDeadInAlt  = deadInAlternative.map (_.flatMap( findStatementsWritingTo(_, alternative) ).toList )

    val possbileTransformation = for {
      inAlt  <- stmsWrintingToDeadInAlt
      inCons <- stmsWrintingToDeadInCons
    } yield {
      findPossiblePullOuts(inAlt, inCons)
    }

    val possiblyNew = possbileTransformation.map { (xs: List[IfPullOut]) =>
      xs.foldLeft( (consequent, alternative) ) { (result, pullout) =>

        val (cons, alt) = result

        val removedCons  = cons.filterNot { pullout.remove.contains(_) }
        val replacedCons = repalceReads(pullout.replaceUsesOf, pullout.withUsesOf, removedCons)

        val removedAlt  = alt.filterNot { pullout.remove.contains(_) }
        val replacedAlt = repalceReads(pullout.replaceUsesOf, pullout.withUsesOf, removedAlt)

        (replacedCons, replacedAlt)
      }
    }

    val pulledOutConsequent  = possiblyNew.map( _._1 ).getOrElse(consequent)
    val pulledOutAlternative = possiblyNew.map( _._2 ).getOrElse(alternative)

    // Now remove normal temporary variable in each block of the conditional.
    val newConsequent  = findDeadVariables(live, pulledOutConsequent)
                          .map(rewrite(_, pulledOutConsequent, method))
                          .getOrElse(pulledOutConsequent)

    val newAlternative = findDeadVariables(live, pulledOutAlternative)
                          .map(rewrite(_, pulledOutAlternative, method))
                          .getOrElse(pulledOutAlternative)

    val prelude = possbileTransformation.map( _.map(_.pullOut) ).getOrElse(Nil)

    val newConditional = SJConditional(cond, newConsequent, newAlternative)

    (prelude, newConditional)
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

  //
  // Returns a list of all SJFieldRead that stores its value in 'to'
  //
  private def findStatementsWritingTo(to: String, block: List[SJStatement]): List[SJStatement] = {
    AST.foldRight(block, Nil, { (stm: SJStatement, acc: List[SJStatement]) =>
      stm match {
        case stm @ SJFieldRead(SJVariableAccess(`to`),_,_) => stm :: acc
        case _ => acc
      }
    })
  }

  //
  // Replace all reads of the variable 'of' with reads of the variable
  // 'use'.
  //
  def repalceReads( of: String, to: String, in: List[SJStatement] ) = AST.trans(in, (x: SJStatement) =>
    x match {
      case SJVariableAccess(`of`) => SJVariableAccess(to)
      case x => x
    })

  //
  // Given the two blocks of a conditional it will find candidates to
  // be 'pulled' out of the branches to the prelude of the conditional
  //
  // @see removeSuperfluousInConditional for why we want this
  //
  def findPossiblePullOuts(alternative: List[SJStatement], consequent: List[SJStatement]): List[IfPullOut] = {
    consequent.flatMap {
      case stm1 @ SJFieldRead(SJVariableAccess(toUse),a,b) => alternative.collect {
        case stm2 @ SJFieldRead(SJVariableAccess(toRemove),`a`,`b`) =>
          IfPullOut( replaceUsesOf = toRemove,
                     withUsesOf = toUse,
                     pullOut = stm1,
                     remove = List(stm1, stm2) )
      }
      case _ => Nil
    }
  }


}
