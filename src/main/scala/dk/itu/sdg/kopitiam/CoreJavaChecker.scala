/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait CoreJavaChecker extends VisitingAST {
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jdt.core.dom.ASTNode
  def checkAST (jes : JavaEditorState, root : ASTNode, doc : IDocument) : Boolean = {
    val co = new CheckCoreJava(jes, doc)
    root.accept(co)
    Console.println("checked...")
    co.getSuccess
  }

  class CheckCoreJava (jes : JavaEditorState, doc : IDocument) extends ReportingVisitor(jes) {
    import org.eclipse.jdt.core.dom.{ArrayAccess, ArrayCreation, ArrayInitializer, Assignment, CastExpression, ClassInstanceCreation, ConditionalExpression, Expression, FieldAccess, InfixExpression, InstanceofExpression, MethodInvocation, PostfixExpression, PrefixExpression, QualifiedName, SimpleName, SuperFieldAccess, SuperMethodInvocation, ThisExpression}
    def checkExpression (node : Expression) : Unit =
      node match {
        case x : QualifiedName =>
          if (x.getQualifier.isInstanceOf[QualifiedName])
            reportError("Nested field access (qualified name twice) is not yet supported by Kopitiam", x)
        case x : FieldAccess =>
          val exp = x.getExpression
          exp match {
            case y : FieldAccess =>
              reportError("Nested field access is not yet supported by Kopitiam", x)
            case y : QualifiedName =>
              reportError("Nested field access (with qualified name) is not supported by Kopitiam", x)
            case y : ThisExpression =>
              //all good
            case y =>
              reportError("Nested field access is not supported by Kopitiam", x)
          }
        case x : ConditionalExpression =>
          reportError("The ternary operator is not yet supported by Kopitiam", x)
        case x : PostfixExpression =>
          reportError("The postfix expression is not yet supported by Kopitiam", x)
        case x : PrefixExpression =>
          reportError("The prefix expression is not yet supported by Kopitiam", x)
        case x : MethodInvocation =>
          if (! isMethodInvocationGood(x))
            reportError("The method call should use only simple expression, this is too complex for Kopitiam", x)
        case x : ClassInstanceCreation =>
          if (! isClassInstanceCreationGood(x))
            reportError("The class instance creation has to pass no arguments currently in Kopitiam", x)
        case x : Assignment =>
          //better make sure left and right are good
          val left = x.getLeftHandSide
          val right = x.getRightHandSide
          if (! containsRealExpressions(left))
            if (! containsRealExpressions(right))
              reportError("Both sides are not simple expression, but one side should be. This is too complex for Kopitiam", x)
            else
              left match {
                case x : FieldAccess => //fine!
                case x : QualifiedName => //fine!
                case x : SimpleName => //fine!
                case x =>
                  Console.println("x is " + x.getClass.toString)
                  reportError("This is not a simple expression, it is too complex for Kopitiam", x)
              }
          else if (! containsRealExpressions(right))
            //left is already known to be a real expression!
            right match {
              case x : MethodInvocation => //we're good!
              case x : ClassInstanceCreation => //we're good!
              case x =>
                reportError("This is not a simple expression, it is too complex for Kopitiam", x)
            }
          //plus operator is good!
          val op = x.getOperator
          if (! isAssignmentOperatorGood(op))
            reportError("This assignment operator is not yet supported by Kopitiam", x)
          //what is good?
        case x : InfixExpression =>
          //better make sure left and right are good
          val left = x.getLeftOperand
          if (! containsRealExpressions(left))
            reportError("This is not a simple expression, it is too complex for Kopitiam", left)
          val right = x.getRightOperand
          if (! containsRealExpressions(right))
            reportError("This is not a simple expression, it is too complex for Kopitiam", right)
          //plus operator is good!
          val op = x.getOperator
          if (! isInfixOperatorGood(op))
            reportError("The infix operator is not yet supported by Kopitiam", x)
        case x : InstanceofExpression =>
          reportError("The instanceof statement is not yet supported by Kopitiam", x)
        case x : ArrayAccess =>
          reportError("Arrays are not yet supported by Kopitiam", x)
        case x : ArrayCreation =>
          reportError("Arrays are not yet supported by Kopitiam", x)
        case x : ArrayInitializer =>
          reportError("Arrays are not yet supported by Kopitiam", x)
        case x : CastExpression =>
          reportError("A dynamic cast is not yet supported by Kopitiam", x)
        case x : SuperMethodInvocation =>
          reportError("The super method is not supported by Kopitiam, due to its lack of class-to-class inheritance", x)
        case x : SuperFieldAccess =>
          reportError("A super field is not supported by Kopitiam, due to its lack of class-to-class inheritance", x)
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
      scala.collection.JavaConversions.asScalaBuffer(node.arguments).map(_.asInstanceOf[Expression]).toList.length == 0
    def isMethodInvocationGood (node : MethodInvocation) : Boolean =
      areArgumentsGood(scala.collection.JavaConversions.asScalaBuffer(node.arguments).map(_.asInstanceOf[Expression]).toList)

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
            reportError("The test of a conditional statement may only contain a simple expression which accesses variables on the stack in Kopitiam. Also, only a limited set of operators is supported.", tst)
        case x : WhileStatement =>
          val tst = x.getExpression
          //should be a real expression
          if (! containsRealExpressions(tst))
            reportError("The test of a while loop may only contain a simple expression which accesses variables on the stack in Kopitiam. Also, only a limited set of operators is supported.", tst)
        case x : FieldDeclaration =>
          //no initialzers!
          val frag = scala.collection.JavaConversions.asScalaBuffer(x.fragments).map(_.asInstanceOf[VariableDeclarationFragment]).toList
          if (frag.filter(_.getInitializer != null).length > 0)
            reportError("A Field declaration with an initialization expression is not yet supported by Kopitiam", x)
        case x : BreakStatement =>
          reportError("The break statement is not yet supported by Kopitiam", x)
        case x : ContinueStatement =>
          reportError("The continue statement is not yet supported by Kopitiam", x)
        case x : DoStatement =>
          reportError("The do loop is not yet supported by Kopitiam, please use while", x)
        case x : EnhancedForStatement =>
          reportError("The enhanced for loop is not yet supported by Kopitiam, please use while", x)
        case x : ForStatement =>
          reportError("The for loop is not yet supported by Kopitiam, please use while", x)
        case x : LabeledStatement =>
          reportError("Statements with a label are not yet supported by Kopitiam", x)
        case x : SwitchCase =>
          reportError("The switch case is not yet supported by Kopitiam, please use if", x)
        case x : SwitchStatement =>
          reportError("The switch statement is not yet supported by Kopitiam, please use if", x)
        case x : SynchronizedStatement =>
          reportError("The synchronized statement is not yet supported by Kopitiam", x)
        case x : ThrowStatement =>
          reportError("The throw statement is not yet supported by Kopitiam", x)
        case x : TryStatement =>
          reportError("The try statement is not supported by Kopitiam", x)
        case x : ArrayType =>
          reportError("Array types are not yet supported by Kopitiam", x)
        case x : ParameterizedType =>
          reportError("Generics / Parameterized types are not yet supported by Kopitiam", x)
        case x : WildcardType =>
          reportError("Wildcard types are not yet supported by Kopitiam", x)
        case x : Expression =>
          checkExpression(x)
        case x =>
      }
      true
    }
  }
}

