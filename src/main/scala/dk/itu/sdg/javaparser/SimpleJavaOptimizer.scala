/* 
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet }

object SimpleJavaOptimizer {

  type Variables = HashSet[String]
    
  case class ReadsAndWrites(reads: Variables = new HashSet(), writes: Variables = new HashSet()) 
  
  /** 
   * Rewrite the method to remove any dead variables. 
   */
  def liveVariableRewrite(method: SJMethodDefinition): SJMethodDefinition = {
    
    def rewrite(deads: HashSet[String], statements: List[SJStatement]): List[SJStatement] = {
      val rw = rwOfStatements(statements).getOrElse(ReadsAndWrites())
      deads.foldLeft(statements){ (rewritten, dead) => 
        if ( !rw.reads.contains(dead) ) // if it's just written and not read the block we can just remove it.
          rewritten
        else // if it's read/written, we have to be a bit more clever 
          rewritten
      }
    }
    
    var in = HashSet[String]()
    
    val newBody = method.body.reverse.map { _ match {
      case SJConditional(cond, consequent, alternative) => {
        val newConsequent  = findDeadVariables(in, consequent).map { deads => rewrite(deads, consequent) }.getOrElse(consequent)
        val newAlternative = findDeadVariables(in, alternative).map { deads => rewrite(deads, alternative) }.getOrElse(alternative)
        val rwConsequent   = rwOfStatements(consequent).getOrElse(ReadsAndWrites())
        val rwAlternative  = rwOfStatements(alternative).getOrElse(ReadsAndWrites())
        in = (rwConsequent.reads ++ rwAlternative.reads ++ in).filterNot( x => rwConsequent.writes.contains(x) || rwAlternative.writes.contains(x) )
        SJConditional(cond, newConsequent, newAlternative)
      }
      case SJWhile(cond, body) => {
        val rw = rwOfStatements(body).getOrElse(ReadsAndWrites())
        val newBody = findDeadVariables(in, body).map( deads => rewrite(deads, body)).getOrElse(body)
        in = (rw.reads ++ in).filterNot( rw.writes.contains(_) )
        SJWhile(cond, newBody)
      }
      case stm => {
        val rw = rwOfStatement(stm).getOrElse(ReadsAndWrites())
        in = (rw.reads ++ in).filterNot( rw.writes.contains(_) )
        stm
      }
    }}.reverse
    
    method.copy( body = newBody )
  }
  
  /** 
   * Given a list of variable that are read after the list of SJStatement (in) find
   * any dead variables. A variable is dead if it's written but not in the in set. I.e.
   * the following blocks doesn't read it. 
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
  
  /** 
   * Just for testing small example while the analysis is still under development. 
   */
  def main(args: Array[String]): Unit = {
    import scala.collection.immutable.{ HashMap }
    val prog = SJMethodDefinition(Set(Static()), "fac", "int",
     List(SJArgument("n", "int")), List(
       SJConditional(SJBinaryExpression(">=", SJVariableAccess("n"), SJLiteral("0")),
          List(SJCall(Some(SJVariableAccess("tmp_1")), SJVariableAccess("this"), "fac",
                                List(SJBinaryExpression("-", SJVariableAccess("n"), SJLiteral("1")))),
               SJAssignment(SJVariableAccess("x"), SJBinaryExpression("*", SJVariableAccess("n"), SJVariableAccess("tmp_1")))),
          List(SJAssignment(SJVariableAccess("x"), SJLiteral("1")))),
       SJReturn(SJVariableAccess("x"))),
         HashMap("x" -> "int", "tmp_1" -> "int", "n" -> "int", "this" -> "Fac"))
    println(liveVariableRewrite(prog))
  }
}
