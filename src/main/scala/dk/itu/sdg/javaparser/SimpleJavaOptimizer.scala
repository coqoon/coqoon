/* 
  Author: Mads Hartmann Jensen
*/

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet }

object SimpleJavaOptimizer {

  type Variables = Set[String]
  type Block = List[SJStatement]

  case class ReadsAndWrites(reads: Variables = new HashSet(), writes: Variables = new HashSet()) 
  
  /** 
   * Perform a Live Variable Analysis on the method body. 
   *
   * TODO: Currently it just prints to the console whenever it finds a dead variable. 
   */
  def liveVariables(method: SJMethodDefinition): Unit = {
    
    /** 
     * Find the variables that are live at the end of the block. 
     *  
     * Any variable that is written but isn't read by the following blocks (represented by the 'in' 
     * parameter) is considered a dead variable   
     * 
     * @param block The block to analyze
     * @param in the variables that are live after the block 
     * @return the variables that are live at the end of the block. 
     */
    def analyse(block: Block, in: Variables): Variables = {
      val rw = block.foldLeft(ReadsAndWrites()){ (acc, current) => merge(rwOfStatement(current), Some(acc)).getOrElse(ReadsAndWrites()) }
      val dead = rw.writes.filterNot( in.contains(_) )
      if (!dead.isEmpty) println("Found dead variable: " + dead + " in " + block) //TODO: Do something proper. 
      (rw.reads ++ in).filterNot( rw.writes.contains(_) ) // TODO: Is it correct to also remove elements from 'in'?
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
    def extractBlocks(in: List[SJStatement], out: List[Block] = Nil): List[Block] = in match {
      case Nil => out
      case x :: xs => { x match {
        case SJConditional(cond,consequent, alternative) => extractBlocks(xs, List(cond) :: (consequent ::: alternative) :: out)
        case SJWhile(cond, body) => extractBlocks(xs, List(cond) :: body :: out)
        // not a new block. Add the statement to the block at the top of the stack. 
        case stm => extractBlocks(xs, (stm :: out.headOption.getOrElse(Nil)) :: (if (out.isEmpty) Nil else out.tail))
      }}
    }
    
    /** 
     * Run a live variable analysis on each block. The variables that are a alive at the end of each block are
     * used when analyzing the blocks before it. 
     *
     * @param block The current block to analyze 
     * @param in the variables that are live after the block 
     * @param workList The blocks still to be processed 
     */
    def process(block: Block, in: Variables, workList: List[Block]): Variables = workList match {
      case Nil     => analyse(block, in) 
      case x :: xs => process(x,analyse(block, in),xs)
    }

    val blocks = extractBlocks(method.body) 
    println(process(blocks.head, HashSet(), blocks.tail))
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
          val writes = ReadsAndWrites( writes = HashSet(w))
          val rws = args.map(rwOfStatement(_)).foldLeft(Option(writes)){ (acc, current) => merge(current, acc) }
          merge(rws, before)
        case SJFieldWrite(SJVariableAccess(w),_,expr)               => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJAssignment(SJVariableAccess(w),expr)                 => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((w)))), before))
        case SJBinaryExpression(_,l,r)                              => merge(merge(before, recursive(l,None)),recursive(r,None))
        case SJCall(value, a:SJVariableAccess,_,args)               => 
          val reads = HashSet(a.variable)
          val rw = value.map( w => ReadsAndWrites( reads = reads, writes = HashSet(w.variable)) ).getOrElse(ReadsAndWrites(reads = reads))
          val rws = args.map(rwOfStatement(_)).foldLeft(Option(rw)){ (acc, current) => merge(current, acc) }
          merge(rws, before)
        case _                                                      => None // don't care about the SJStatements that doesn't read/write variables 
      })
      merge(before,after)
    }  
    recursive(statement, None)
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
    liveVariables(prog)
  }
}
