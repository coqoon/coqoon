/* 
  Author: Mads Hartmann Jensen
  Notes: When calling a function: this.method we're registering 'this' as read. is this right?  
*/

package dk.itu.sdg.javaparser

import scala.collection.immutable.{ HashSet }

object SimpleJavaOptimizer {

  case class Read(name: String)
  case class Write(name: String)
  case class ReadsAndWrites(reads:  HashSet[Read]  = new HashSet(), 
                            writes: HashSet[Write] = new HashSet()) 
  case class State(processing:  SJStatement, 
                   inState:     Set[String] = new HashSet(), 
                   oldOutState: Set[String] = new HashSet(), 
                   newOutState: Set[String] = new HashSet(), 
                   workList:    List[SJStatement])
  
  // Given a SJStatement it will return Some(ReadsAndWrites) with the varible being read/written. None is 
  // no variable are read. 
  def rwOfStatement(statement: SJStatement): Option[ReadsAndWrites] = {
    def recursive(stm: SJStatement, before: Option[ReadsAndWrites]): Option[ReadsAndWrites] = {
      val after = (stm match {
        case SJVariableAccess(read)                                        => Some(ReadsAndWrites( reads = HashSet(Read(read))))
        case SJFieldRead(SJVariableAccess(write),SJVariableAccess(read),_) => Some(ReadsAndWrites(HashSet(Read(read)), HashSet(Write(write))))
        case SJNewExpression(SJVariableAccess(write),_,_)                  => Some(ReadsAndWrites( writes = HashSet(Write(write))))
        case SJFieldWrite(SJVariableAccess(write),_,expr)                  => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((Write(write))))), before))
        case SJAssignment(SJVariableAccess(store),expr)                    => recursive(expr, merge(Some(ReadsAndWrites( writes = HashSet((Write(store))))), before))
        case SJBinaryExpression(_,l,r)                                     => merge(merge(before, recursive(l,None)),recursive(r,None))
        case s: SJCall                                                     => s match {
          // once SJCall.receiver is a JVariableAccess this can be simplified. 
          case SJCall(None, a:SJVariableAccess,_,_)         => Some(ReadsAndWrites( reads = HashSet(Read(a.variable))))
          case SJCall(Some(access), a:SJVariableAccess,_,_) => Some(ReadsAndWrites(HashSet(Read(a.variable)), HashSet(Write(access.variable))))
          case _                                            => throw new Exception("Receiver should be a JVariableAccess")
        }
      })
      merge(before,after)
    }
    recursive(statement, None)
  }
  
  // TODO: Implement 
  def livenessAnalysis(method: SJMethodDefinition): Unit = {
    def process(state: State): State = state match {
      case s@State(stm, in, out, newOut, Nil)     => s
      case s@State(stm, in, out, newOut, x :: xs) => s
    }
      
    val statements = method.body.reverse // post-order
    process(State(processing = statements.head, workList = statements.tail))
  }
  
  private def merge(x: Option[ReadsAndWrites], y: Option[ReadsAndWrites]) = (x,y) match {
    case (None, Some(q))   => Some(q)
    case (Some(q), None)   => Some(q)
    case (None, None)      => None 
    case (Some(p),Some(q)) => Some(ReadsAndWrites(p.reads ++ q.reads, p.writes ++ q.writes))
  }
}