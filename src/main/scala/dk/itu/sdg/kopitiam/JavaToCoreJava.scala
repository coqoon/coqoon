/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait JavaToCoreJava extends VisitingAST {
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jdt.core.dom.ASTNode
  def translateAST (root : ASTNode, doc : IDocument) : Unit = {
    val co = new Translator(doc)
    root.accept(co)
    Console.println("translated...")
  }

  class Translator (doc : IDocument) extends Visitor {

    import org.eclipse.jdt.core.dom.{ArrayAccess, ArrayCreation, ArrayInitializer, Assignment, CastExpression, ConditionalExpression, Expression, FieldAccess, InfixExpression, InstanceofExpression, PostfixExpression, PrefixExpression, QualifiedName}
    def checkExpression (node : Expression) : Unit =
      node match {
        case x : QualifiedName =>
          if (x.getQualifier.isInstanceOf[QualifiedName])
            Console.println("PROBLEM: qualified name with qualified name")
        case x : FieldAccess =>
          val exp = x.getExpression
          exp match {
            case y : FieldAccess =>
              Console.println("PROBLEM: nested field access")
            case y : QualifiedName =>
              Console.println("PROBLEM: nested field with qualified name")
            case y =>
              Console.println("maybe PROBLEM: nested field with " + y.getClass.toString)
          }
        case x : ConditionalExpression =>
          Console.println("PROBLEM: conditionalexpression (but we can fix)")
        case x : PostfixExpression =>
          Console.println("PROBLEM: postfix")
        case x : PrefixExpression =>
          Console.println("PROBLEM: prefix")
        case x : Assignment =>
          //better make sure left and right are good
          val left = x.getLeftHandSide
          val right = x.getRightHandSide
          //plus operator is good!
          val op = x.getOperator
          //what is good?
        case x : InfixExpression =>
          //better make sure left and right are good
          val left = x.getLeftOperand
          val right = x.getRightOperand
          //plus operator is good!
          val op = x.getOperator
          //what is good?
//    *    TIMES
//    /  DIVIDE
//    %  REMAINDER
//    +  PLUS
//    -  MINUS
//    <<  LEFT_SHIFT
//    >>  RIGHT_SHIFT_SIGNED
//    >>>  RIGHT_SHIFT_UNSIGNED
//    <  LESS
//    >  GREATER
//    <=  LESS_EQUALS
//    >=  GREATER_EQUALS
//    ==  EQUALS
//    !=  NOT_EQUALS
//    ^  XOR
//    &  AND
//    |  OR
//    &&  CONDITIONAL_AND
//    ||  CONDITIONAL_OR
        case x : InstanceofExpression =>
          Console.println("PROBLEM: instanceof")
        case x : ArrayAccess =>
          Console.println("PROBLEM: arrayaccess")
        case x : ArrayCreation =>
          Console.println("PROBLEM: arraycreation")
        case x : ArrayInitializer =>
          Console.println("PROBLEM: arrayinitializer")
        case x : CastExpression =>
          Console.println("PROBLEM: castexpression")
        case x => //you may pass
      }

    import org.eclipse.jdt.core.dom.{ClassInstanceCreation, MethodInvocation, ParenthesizedExpression, SimpleName}
    def containsRealExpressions (node : Expression) : Boolean =
      node match {
        case x : FieldAccess => false
        case x : Assignment => false //is that right?
        case x : QualifiedName => false
        case x : ClassInstanceCreation => false
        case x : MethodInvocation => false
        case x : SimpleName => ! isField(x)
        case x : ParenthesizedExpression => containsRealExpressions(x.getExpression)
        case x : InfixExpression =>
          //both must be good
          val left = x.getLeftOperand
          val right = x.getRightOperand
          //plus operator is good!
          val op = x.getOperator
          //val opgood = ?
          containsRealExpressions(left) && containsRealExpressions(right)
        case x =>
          Console.println("got " + x.getClass.toString)
          true
      }

    import org.eclipse.jdt.core.dom.{BreakStatement, ContinueStatement, DoStatement, EnhancedForStatement, ForStatement, IfStatement, LabeledStatement, SwitchCase, SwitchStatement, SynchronizedStatement, ThrowStatement, TryStatement, WhileStatement}
    override def visitNode (node : ASTNode) : Boolean = {
      node match {
        case x : IfStatement =>
          //verify test is a _real_ expression
          val tst = x.getExpression
          if (! containsRealExpressions(tst))
            Console.println("PROBLEM in IF-TEST")
        case x : WhileStatement =>
          val tst = x.getExpression
          //should be a real expression
          if (! containsRealExpressions(tst))
            Console.println("PROBLEM in WHILE-TST")
        case x : BreakStatement =>
          Console.println("PROBLEM: break")
        case x : ContinueStatement =>
          Console.println("PROBLEM: continue")
        case x : DoStatement =>
          Console.println("PROBLEM: do")
        case x : EnhancedForStatement =>
          Console.println("PROBLEM: enhanced for")
        case x : ForStatement =>
          Console.println("PROBLEM: for")
        case x : LabeledStatement =>
          Console.println("PROBLEM: labeled statement")
        case x : SwitchCase =>
          Console.println("PROBLEM: switchcase")
        case x : SwitchStatement =>
          Console.println("PROBLEM: switch")
        case x : SynchronizedStatement =>
          Console.println("PROBLEM: synchronized")
        case x : ThrowStatement =>
          Console.println("PROBLEM: throw")
        case x : TryStatement =>
          Console.println("PROBLEM: try")
        case x : Expression =>
          checkExpression(x)
        //we can have generic types, unfortunately, somewhere...
        case x =>
      }
      true
    }

    override def endVisitNode (node : ASTNode) : Unit = { }

  }
}

