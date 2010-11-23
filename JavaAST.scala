package dk.itu.sdg.javaparser

trait JStatement { }
case class JClassDefinition (id : String, superclass : String, interfaces : List[String], body : List[JStatement], outerclass : Option[String]) extends JStatement
case class JInterfaceDefinition (id : String, interfaces : List[String], body : List[JStatement]) extends JStatement

trait InnerStatement extends JStatement

case class JFieldDefinition (id : String, jtype : String) extends InnerStatement
case class JMethodDefinition (id : String, parameters : List[JArgument], body : List[JBodyStatement]) extends InnerStatement

case class JArgument (id : String, jtype : String) extends InnerStatement

trait JBodyStatement extends InnerStatement
case class JBlock (body : List[JBodyStatement]) extends JBodyStatement
case class JAssignment (left : String, right : JExpression) extends JBodyStatement
case class JFieldWrite (variable : JExpression, field : String, value : JExpression) extends JBodyStatement
case class JReturn (ret : JExpression) extends JBodyStatement
case class JBinding (name : String, jtype : String, init : Option[JExpression]) extends JBodyStatement
case class JWhile (test : JExpression, body : JBlock) extends JBodyStatement

trait JExpression extends JBodyStatement
case class JConditional (test : JExpression, consequent : JBodyStatement, alternative : JBodyStatement) extends JExpression
case class JBinaryExpression (operation : String, left : JExpression, right : JExpression) extends JExpression
case class JUnaryExpression (operation : String, expr : JExpression) extends JExpression
case class JPostfixExpression (operation : String, expr : JExpression) extends JExpression
case class JCall (variable : String, fun : String, arguments : List[JExpression]) extends JExpression
case class JNewExpression (jtype : String, arguments : List[JExpression]) extends JExpression
case class JLiteral (value : String) extends JExpression
case class JVariableAccess (variable : String) extends JExpression
case class JFieldAccess (variable : JExpression, field : String) extends JExpression


trait JavaAST extends JavaParser { // with CoqOutputter {
  import scala.util.parsing.input._

  def parse(r: Reader[Char]) : String = {
    ClassTable.empty
    //Console.println("scanning " + r)
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    //Console.println("scanned " + p)
    p match {
      case Success(x @ ~(_,_), _) =>
        val conv = FinishAST.doit(x)
        //coqoutput(conv).reduceLeft(_ + "\n" + _)
        conv
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString); ""
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString); ""
    }
  }
}

