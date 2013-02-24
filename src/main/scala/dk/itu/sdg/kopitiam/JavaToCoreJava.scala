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

    import org.eclipse.jdt.core.dom.{ArrayAccess, ArrayCreation, ArrayInitializer, Assignment, CastExpression, ClassInstanceCreation, ConditionalExpression, Expression, FieldAccess, InfixExpression, InstanceofExpression, MethodInvocation, PostfixExpression, PrefixExpression, QualifiedName, SuperFieldAccess, SuperMethodInvocation, ThisExpression}
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
            case y : ThisExpression =>
              //all good
            case y =>
              Console.println("maybe PROBLEM: nested field with " + y.getClass.toString)
          }
        case x : ConditionalExpression =>
          Console.println("PROBLEM: conditionalexpression (but we can fix)")
        case x : PostfixExpression =>
          Console.println("PROBLEM: postfix")
        case x : PrefixExpression =>
          Console.println("PROBLEM: prefix")
        case x : MethodInvocation =>
          if (! isMethodInvocationGood(x))
            Console.println("PROBLEM: method invocation bad")
        case x : ClassInstanceCreation =>
          if (! isClassInstanceCreationGood(x))
            Console.println("PROBLEM: class instance creation bad")
        case x : Assignment =>
          //better make sure left and right are good
          val left = x.getLeftHandSide
          val right = x.getRightHandSide
          if (! containsRealExpressions(left))
            if (! containsRealExpressions(right))
              Console.println("PROBLEM: left of assignment not an expression: " + left)
            else
              left match {
                case x : FieldAccess => //fine!
                case x : QualifiedName => //fine!
                case x => Console.println("maybe problem: left is not an expression, but right is one. left is a [" + x.getClass.toString + "]")
              }
          else if (! containsRealExpressions(right))
            //left is already known to be a real expression!
            right match {
              case x : MethodInvocation => //we're good!
              case x : ClassInstanceCreation => //we're good!
              case x =>
                Console.println("PROBLEM: right of assignment not an expression [" + x.getClass.toString + "]: " + right)
            }
          //plus operator is good!
          val op = x.getOperator
          if (! isAssignmentOperatorGood(op))
            Console.println("PROBLEM: assignment operator no good")
          //what is good?
        case x : InfixExpression =>
          //better make sure left and right are good
          val left = x.getLeftOperand
          if (! containsRealExpressions(left))
            Console.println("PROBLEM: left of infix is not an expression")
          val right = x.getRightOperand
          if (! containsRealExpressions(right))
            Console.println("PROBLEM: right of infix is not an expression")
          //plus operator is good!
          val op = x.getOperator
          if (! isInfixOperatorGood(op))
            Console.println("PROBLEM: infix with bad operator")
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
        case x : SuperMethodInvocation =>
          Console.println("PROBLEM: super method invocation")
        case x : SuperFieldAccess =>
          Console.println("PROBLEM: super field access")
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
      args.filter(containsRealExpressions).length == args.length

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
            Console.println("PROBLEM in IF-TEST")
        case x : WhileStatement =>
          val tst = x.getExpression
          //should be a real expression
          if (! containsRealExpressions(tst))
            Console.println("PROBLEM in WHILE-TST")
        case x : FieldDeclaration =>
          //no initialzers!
          val frag = scala.collection.JavaConversions.asBuffer(x.fragments).map(_.asInstanceOf[VariableDeclarationFragment]).toList
          if (frag.filter(_.getInitializer != null).length > 0)
            Console.println("PROBLEM: field declaration with initializer")
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
        case x : ArrayType =>
          Console.println("PROBLEM: ArrayType")
        case x : ParameterizedType =>
          Console.println("PROBLEM: ParameterizedType")
        case x : WildcardType =>
          Console.println("PROBLEM: WildcardType")
        case x : Expression =>
          checkExpression(x)
        case x =>
      }
      true
    }

    override def endVisitNode (node : ASTNode) : Unit = { }

  }
}

