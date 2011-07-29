/* 
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet }

object SimpleJavaOptimizer {

  type Variables = Set[String]
  
  def noVars() = HashSet[String]() 
  
  sealed trait SJStatementMetaData 
  trait RW extends SJStatementMetaData { 
    val rw: ReadsAndWrites 
    override def toString() = super.toString() + " RW " + rw.toString() 
  }

  def withRWMeta(stm: SJStatement, rAndw: ReadsAndWrites): SJStatement with RW = {
    stm match {
      case SJAssert(assertion)                          => new SJAssert(assertion) with RW { val rw = rAndw }
      case SJWhile(test, body)                          => new SJWhile(test, body) with RW { val rw = rAndw }
      case SJConditional(test, consequent, alternative) => new SJConditional(test,consequent,alternative) with RW { val rw = rAndw }
      case SJAssignment(left, right)                    => new SJAssignment(left,right) with RW { val rw = rAndw }
      case SJFieldWrite(variable, field, value)         => new SJFieldWrite(variable, field, value) with RW { val rw = rAndw }
      case SJFieldRead(value, variable, field)          => new SJFieldRead(value, value, field) with RW { val rw = rAndw }
      case SJReturn(ret)                                => new SJReturn(ret) with RW { val rw = rAndw }
      case SJCall(value, receiver, fun, arguments)      => new SJCall(value, receiver, fun, arguments) with RW { val rw = rAndw }
      case SJNewExpression(value, jtype, arguments)     => new SJNewExpression(value, jtype, arguments) with RW { val rw = rAndw }
      case SJBinaryExpression(operation, left, right)   => new SJBinaryExpression(operation, left, right) with RW { val rw = rAndw }
      case SJUnaryExpression(operation, expr)           => new SJUnaryExpression(operation, expr) with RW { val rw = rAndw }
      case SJLiteral(value)                             => new SJLiteral(value) with RW { val rw = rAndw }
      case SJVariableAccess(variable)                   => new SJVariableAccess(variable) with RW { val rw = rAndw }
    }
  }
  
  case class ReadsAndWrites(reads: Variables = new HashSet(), writes: Variables = new HashSet()) 
  
  /** 
   * TODO: DO the actual rewrite.  
   */
  def liveVariableRewrite(statements: List[SJStatement with RW]): List[SJStatement] = {

    val blocks = extractBlocks(statements).asInstanceOf[List[List[SJStatement with RW]]]
    
    val l = blocks.foldLeft((Nil: List[List[SJStatement with RW]],HashSet(): Variables)){ (result: (List[List[SJStatement with RW]], Variables), current: List[SJStatement with RW]) => 
      
      val (xs, in) = result
      
      val rw = current.foldLeft(ReadsAndWrites()){ (acc, current) => merge(Some(current.rw), Some(acc)).get }
      val dead = rw.writes.filterNot( in.contains(_) )
      if (!dead.isEmpty) println("Found dead variable: " + dead + " in " + current) //TODO: Do something proper. 
      var out = (rw.reads ++ in).filterNot( rw.writes.contains(_) )
      (current :: xs, out)
    }
    
    statements
  }
      
  /** 
  * traverse the AST and partition the statement into blocks using the following rules: 
  * 
  *   - body of if/else are considered a single block 
  *   - conditions are considered part of the block before it.
  * 
  * @param in   The list of statements still to process
  * @param out  The list of blocks. The new blocks are prepended during the processing so this will result in a reversed 
  *             order of the blocks. This is handy as we're doing a Backward Analysis. 
  */
  def extractBlocks(in: List[SJStatement], out: List[List[SJStatement]] = Nil): List[List[SJStatement]] = in match {
    case Nil => out
    case x :: xs => { x match {
      case SJConditional(cond,consequent, alternative) => extractBlocks(xs, List(cond) :: (consequent ::: alternative) :: out)
      case SJWhile(cond, body) => extractBlocks(xs, List(cond) :: body :: out)
      // not a new block. Add the statement to the block at the top of the stack. 
      case stm => extractBlocks(xs, (stm :: out.headOption.getOrElse(Nil)) :: (if (out.isEmpty) Nil else out.tail))
    }}
  }
  
  /** 
   * Traverse the AST while enriching it with additional meta-data. 
   *
   * @param stm The statement to enrich with meta-data
   * @param f A unary function that enriches the SJStatement with meta-data.  
   */
  def enrich[A <: SJStatementMetaData, Bin <: SJStatement, Bout <: SJStatement](stm: SJStatement, f:  (Bin) => Bout with A ): SJStatement with A = {
    
    val fExpr: SJExpression => SJExpression with A = f.asInstanceOf[Function1[SJExpression, SJExpression with A]]
    val fStm: SJStatement => SJStatement with A = f.asInstanceOf[Function1[SJStatement, SJStatement with A]]
    val fVar: SJVariableAccess => SJVariableAccess with A = f.asInstanceOf[Function1[SJVariableAccess, SJVariableAccess with A]]
    val enrichExpr: SJExpression => SJExpression with A = x => enrich(fExpr(x), f).asInstanceOf[SJExpression with A]
    val enrichExprList: List[SJExpression] => List[SJExpression with A] = xs => enrichList(xs, fExpr).asInstanceOf[List[SJExpression with A]]
    
    stm match {
      case SJConditional(test, consequent, alternative) => fStm(SJConditional(enrichExpr(test), enrichList(consequent, f), enrichList(alternative, f)))
      case SJNewExpression(x,y,args)                    => fStm(SJNewExpression(fVar(x),y,enrichExprList(args))) 
      case SJFieldWrite(x,y,expr)                       => fStm(SJFieldWrite(fVar(x),y,enrichExpr(expr)))
      case SJAssignment(x,expr)                         => fStm(SJAssignment(fVar(x),enrichExpr(expr)))
      case SJBinaryExpression(x,l,r)                    => fStm(SJBinaryExpression(x, enrichExpr(l), enrichExpr(r)))
      case SJCall(Some(x),y,z,args)                     => fStm(SJCall(Some(fVar(x)),enrichExpr(y),z,enrichExprList(args)))
      case SJCall(None,y,z,args)                        => fStm(SJCall(None, enrichExpr(y), z, enrichExprList(args)))
      case stm                                          => fStm(stm)
    }
  }
  
  /** 
   * @see enrich. Works the same way but on a list. 
   */
  def enrichList[A <: SJStatementMetaData, Bin <: SJStatement, Bout <: SJStatement](statements: List[SJStatement], f:  (Bin) => Bout with A ): List[SJStatement with A] = {
    statements map { enrich(_, f)}
  }
    
  def rwEnrich(m: SJMethodDefinition): SJMethodDefinition with RW = {    
    def f[B <: SJStatement](stm: B) = withRWMeta(stm, rwOfStatement(stm).getOrElse( ReadsAndWrites() ))
    new SJMethodDefinition(m.modifiers, m.id, m.jtype, m.parameters, enrichList(m.body, (x: SJStatement) => f(x)), m.localvariables) with RW { val rw = ReadsAndWrites() }
  }
  
  /** 
   * Given a SJStatement it will return Some(ReadsAndWrites) with the variable being read/written. None if
   * no variable are read/written
   */
  def rwOfStatement(statement: SJStatement): Option[ReadsAndWrites] = {
    
    def recursive(stm: SJStatement, before: Option[ReadsAndWrites]): Option[ReadsAndWrites] = {
      val after = (stm match {
        case SJVariableAccess(r)                                    => Some(ReadsAndWrites( reads = HashSet(r)))
        case SJReturn(SJVariableAccess(r))                          => Some(ReadsAndWrites( reads = HashSet(r)))
        case SJFieldRead(SJVariableAccess(w),SJVariableAccess(r),_) => Some(ReadsAndWrites(HashSet(r), HashSet(w)))
        case SJNewExpression(SJVariableAccess(w),_,args)            => 
          val writes = Some(ReadsAndWrites( writes = HashSet(w)))
          merge(rwOfStatements(args, writes), before)
        case SJFieldWrite(SJVariableAccess(w),_,expr)               => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJAssignment(SJVariableAccess(w),expr)                 => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJBinaryExpression(_,l,r)                              => merge(merge(before, recursive(l,None)),recursive(r,None))
        case SJCall(value, a:SJVariableAccess,_,args)               => 
          val reads = HashSet(a.variable)
          val rw = value.map( w => ReadsAndWrites( reads = reads, writes = HashSet(w.variable)) ).getOrElse(ReadsAndWrites(reads = reads))
          merge(rwOfStatements(args, Option(rw)), before)
        case SJConditional(test, consequent, alternative) => 
          // TODO: What to do about the 'test'? We can't really add it to the RW set as it belongs to the block before it. 
          val consequentRW = rwOfStatements(consequent)
          val alternativeRW = rwOfStatements(alternative)
          merge(consequentRW, merge(alternativeRW, before))
        case _                                                      => None // don't care about the SJStatements that doesn't read/write variables 
      })
      merge(before,after)
    }  
    recursive(statement, None)
  }
  
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
    liveVariableRewrite(rwEnrich(prog).body.asInstanceOf[List[SJStatement with RW]])
  }
}
