/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait CoreJavaChecker extends VisitingAST {
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jdt.core.dom.ASTNode
  def checkAST (root : ASTNode, doc : IDocument) : Unit = {
    val co = new CheckCoreJava(doc)
    root.accept(co)
    Console.println("checked...")
  }

  class CheckCoreJava (doc : IDocument) extends Visitor {

    import org.eclipse.jdt.core.dom.{ArrayAccess, ArrayCreation, ArrayInitializer, Assignment, CastExpression, ClassInstanceCreation, ConditionalExpression, Expression, FieldAccess, InfixExpression, InstanceofExpression, MethodInvocation, PostfixExpression, PrefixExpression, QualifiedName, SuperFieldAccess, SuperMethodInvocation, ThisExpression}
    def checkExpression (node : Expression) : Unit =
      node match {
        case x : QualifiedName =>
          if (x.getQualifier.isInstanceOf[QualifiedName])
            reportError("PROBLEM: qualified name with qualified name", x)
        case x : FieldAccess =>
          val exp = x.getExpression
          exp match {
            case y : FieldAccess =>
              reportError("PROBLEM: nested field access", x)
            case y : QualifiedName =>
              reportError("PROBLEM: nested field with qualified name", x)
            case y : ThisExpression =>
              //all good
            case y =>
              reportError("maybe PROBLEM: nested field with " + y.getClass.toString, x)
          }
        case x : ConditionalExpression =>
          reportError("PROBLEM: conditionalexpression (but we can fix)", x)
        case x : PostfixExpression =>
          reportError("PROBLEM: postfix", x)
        case x : PrefixExpression =>
          reportError("PROBLEM: prefix", x)
        case x : MethodInvocation =>
          if (! isMethodInvocationGood(x))
            reportError("PROBLEM: method invocation bad", x)
        case x : ClassInstanceCreation =>
          if (! isClassInstanceCreationGood(x))
            reportError("PROBLEM: class instance creation bad", x)
        case x : Assignment =>
          //better make sure left and right are good
          val left = x.getLeftHandSide
          val right = x.getRightHandSide
          if (! containsRealExpressions(left))
            if (! containsRealExpressions(right))
              reportError("PROBLEM: left of assignment not an expression: " + left, x)
            else
              left match {
                case x : FieldAccess => //fine!
                case x : QualifiedName => //fine!
                case x =>
                  reportError("maybe problem: left is not an expression, but right is one. left is a [" + x.getClass.toString + "]", x)
              }
          else if (! containsRealExpressions(right))
            //left is already known to be a real expression!
            right match {
              case x : MethodInvocation => //we're good!
              case x : ClassInstanceCreation => //we're good!
              case x =>
                reportError("PROBLEM: right of assignment not an expression [" + x.getClass.toString + "]: " + right, x)
            }
          //plus operator is good!
          val op = x.getOperator
          if (! isAssignmentOperatorGood(op))
            reportError("PROBLEM: assignment operator no good", x)
          //what is good?
        case x : InfixExpression =>
          //better make sure left and right are good
          val left = x.getLeftOperand
          if (! containsRealExpressions(left))
            reportError("PROBLEM: left of infix is not an expression", x)
          val right = x.getRightOperand
          if (! containsRealExpressions(right))
            reportError("PROBLEM: right of infix is not an expression", x)
          //plus operator is good!
          val op = x.getOperator
          if (! isInfixOperatorGood(op))
            reportError("PROBLEM: infix with bad operator", x)
        case x : InstanceofExpression =>
          reportError("PROBLEM: instanceof", x)
        case x : ArrayAccess =>
          reportError("PROBLEM: arrayaccess", x)
        case x : ArrayCreation =>
          reportError("PROBLEM: arraycreation", x)
        case x : ArrayInitializer =>
          reportError("PROBLEM: arrayinitializer", x)
        case x : CastExpression =>
          reportError("PROBLEM: castexpression", x)
        case x : SuperMethodInvocation =>
          reportError("PROBLEM: super method invocation", x)
        case x : SuperFieldAccess =>
          reportError("PROBLEM: super field access", x)
        case x => //you may pass
      }

    def isInfixOperatorGood (x : InfixExpression.Operator) : Boolean =
      x match {
        case InfixExpression.Operator.TIMES => true
        case InfixExpression.Operator.PLUS => true
        case InfixExpression.Operator.MINUS => true
        case InfixExpression.Operator.LESS => true
        case InfixExpression.Operator.LESS_EQUALS => true
        case InfixExpression.Operator.GREATER => true
        case InfixExpression.Operator.GREATER_EQUALS => true
        case InfixExpression.Operator.EQUALS => true
        case InfixExpression.Operator.NOT_EQUALS => true
        // /  DIVIDE
        // %  REMAINDER
        // <<  LEFT_SHIFT
        // >>  RIGHT_SHIFT_SIGNED
        // >>>  RIGHT_SHIFT_UNSIGNED
        // ^  XOR
        // &  AND
        // |  OR
        // &&  CONDITIONAL_AND
        // ||  CONDITIONAL_OR
        case x => false
      }

    def isAssignmentOperatorGood (x : Assignment.Operator) : Boolean =
      x match {
        case Assignment.Operator.ASSIGN => true
        // += PLUS_ASSIGN - we can rewrite
        // -= MINUS_ASSIGN - we can rewrite
        // *= TIMES_ASSIGN - we can rewrite
        // /= DIVIDE_ASSIGN
        // &= BIT_AND_ASSIGN
        // |= BIT_OR_ASSIGN
        // ^= BIT_XOR_ASSIGN
        // %= REMAINDER_ASSIGN
        // <<= LEFT_SHIFT_ASSIGN
        // >>= RIGHT_SHIFT_SIGNED_ASSIGN
        // >>>= RIGHT_SHIFT_UNSIGNED_ASSIGN
        case y => false
      }

    //currently we only support empty constructors
    def isClassInstanceCreationGood (node : ClassInstanceCreation) : Boolean =
      scala.collection.JavaConversions.asBuffer(node.arguments).map(_.asInstanceOf[Expression]).toList.length == 0
    def isMethodInvocationGood (node : MethodInvocation) : Boolean =
      areArgumentsGood(scala.collection.JavaConversions.asBuffer(node.arguments).map(_.asInstanceOf[Expression]).toList)

    def areArgumentsGood (args : List[Expression]) : Boolean =
      args.filterNot(containsRealExpressions).length == 0

    import org.eclipse.jdt.core.dom.{BooleanLiteral, CharacterLiteral, NullLiteral, NumberLiteral, StringLiteral, ThisExpression}
    import org.eclipse.jdt.core.dom.{ParenthesizedExpression, SimpleName}
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
          isInfixOperatorGood(op) &&
            containsRealExpressions(left) &&
            containsRealExpressions(right)
        case x : BooleanLiteral => true
        case x : CharacterLiteral => true
        case x : NullLiteral => true
        case x : NumberLiteral => true
        case x : StringLiteral => true
        case x : ThisExpression => (x.getQualifier == null)
        case x =>
          Console.println("got " + x.getClass.toString)
          true
      }

    import org.eclipse.jdt.core.dom.{ArrayType, BreakStatement, ContinueStatement, DoStatement, EnhancedForStatement, FieldDeclaration, ForStatement, IfStatement, LabeledStatement, ParameterizedType, SwitchCase, SwitchStatement, SynchronizedStatement, ThrowStatement, TryStatement, VariableDeclarationFragment, WhileStatement, WildcardType}
    override def visitNode (node : ASTNode) : Boolean = {
      node match {
        case x : IfStatement =>
          //verify test is a _real_ expression
          val tst = x.getExpression
          if (! containsRealExpressions(tst))
            reportError("PROBLEM in IF-TEST", tst)
        case x : WhileStatement =>
          val tst = x.getExpression
          //should be a real expression
          if (! containsRealExpressions(tst))
            reportError("PROBLEM in WHILE-TST", tst)
        case x : FieldDeclaration =>
          //no initialzers!
          val frag = scala.collection.JavaConversions.asBuffer(x.fragments).map(_.asInstanceOf[VariableDeclarationFragment]).toList
          if (frag.filter(_.getInitializer != null).length > 0)
            reportError("PROBLEM: field declaration with initializer", x)
        case x : BreakStatement =>
          reportError("PROBLEM: break", x)
        case x : ContinueStatement =>
          reportError("PROBLEM: continue", x)
        case x : DoStatement =>
          reportError("PROBLEM: do", x)
        case x : EnhancedForStatement =>
          reportError("PROBLEM: enhanced for", x)
        case x : ForStatement =>
          reportError("PROBLEM: for", x)
        case x : LabeledStatement =>
          reportError("PROBLEM: labeled statement", x)
        case x : SwitchCase =>
          reportError("PROBLEM: switchcase", x)
        case x : SwitchStatement =>
          reportError("PROBLEM: switch", x)
        case x : SynchronizedStatement =>
          reportError("PROBLEM: synchronized", x)
        case x : ThrowStatement =>
          reportError("PROBLEM: throw", x)
        case x : TryStatement =>
          reportError("PROBLEM: try", x)
        case x : ArrayType =>
          reportError("PROBLEM: ArrayType", x)
        case x : ParameterizedType =>
          reportError("PROBLEM: ParameterizedType", x)
        case x : WildcardType =>
          reportError("PROBLEM: WildcardType", x)
        case x : Expression =>
          checkExpression(x)
        case x =>
      }
      true
    }
  }
}

