/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

sealed abstract class JStatement { }

sealed abstract class JModifier()

case class Private()      extends JModifier
case class Protected()    extends JModifier
case class Public()       extends JModifier
case class Final()        extends JModifier
case class Abstract()     extends JModifier
case class Static()       extends JModifier
case class Native()       extends JModifier
case class Transient()    extends JModifier
case class Volatile()     extends JModifier
case class Synchronized() extends JModifier
case class Strict()       extends JModifier

object JModifier extends JavaTerms {
      
  def apply(str: String): Option[JModifier] = str match {
    case "private"      => Some(Private())
    case "protected"    => Some(Protected())
    case "public"       => Some(Public())
    case "final"        => Some(Final())
    case "abstract"     => Some(Abstract())
    case "static"       => Some(Static())
    case "native"       => Some(Native())
    case "transient"    => Some(Transient())
    case "volatile"     => Some(Volatile())
    case "synchronized" => Some(Synchronized())
    case "strict"       => Some(Strict())
    case _              => None
  }
  
  def unapply(modifier: JModifier): Option[String] = modifier match {
    case Private()      => Some("private")
    case Protected()    => Some("protected")
    case Public()       => Some("public")
    case Final()        => Some("final")
    case Abstract()     => Some("abstract")
    case Static()       => Some("static")
    case Native()       => Some("native")
    case Transient()    => Some("transient")
    case Volatile()     => Some("volatile")
    case Synchronized() => Some("synchronized")
    case Strict()       => Some("strict")
    case _              => None
  }
}

case class JClassDefinition (modifiers : Set[JModifier], id : String, superclass : String, interfaces : List[String], body : List[JStatement], outerclass : Option[String]) extends JStatement
case class JInterfaceDefinition (modifiers : Set[JModifier], id : String, interfaces : List[String], body : List[JStatement]) extends JStatement

trait InnerStatement extends JStatement

case class JFieldDefinition (modifiers : Set[JModifier], id : String, jtype : String, initializer: Option[JExpression]) extends InnerStatement
case class JMethodDefinition (modifiers : Set[JModifier], id : String, jtype : String, parameters : List[JArgument], body : List[JBodyStatement]) extends InnerStatement
case class JConstructorDefinition (modifiers : Set[JModifier], jtype : String, parameters : List[JArgument], body : List[JBodyStatement]) extends InnerStatement

case class JArgument (id : String, jtype : String) extends InnerStatement

trait JBodyStatement extends InnerStatement
case class JBlock (modifier: Option[Static] = None, body : List[JBodyStatement]) extends JBodyStatement 
case class JAssert(assertion : JBodyStatement) extends JBodyStatement
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
case class JCall (receiver : JExpression, fun : String, arguments : List[JExpression]) extends JExpression
case class JNewExpression (jtype : String, arguments : List[JExpression]) extends JExpression
case class JLiteral (value : String) extends JExpression
case class JVariableAccess (variable : String) extends JExpression
case class JFieldAccess (variable : JExpression, field : String) extends JExpression


trait JavaAST extends JavaParser { // with CoqOutputter {
  import scala.util.parsing.input._

  def parse(r: Reader[Char], name : String) : String = {
    FinishAST.doit(parseH(r), name)
  }

  def parseNoSpec(r: Reader[Char], name : String) : (String, String) = {
    FinishAST.doitNoSpec(parseH(r), name)
  }

  def parseH(r: Reader[Char]) : Any = {
    ClassTable.empty
    //Console.println("scanning " + r)
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    //Console.println("scanned " + p)
    p match {
      case Success(x @ ~(_,_), _) =>
        x
        //val conv = FinishAST.doit(x)
        //coqoutput(conv).reduceLeft(_ + "\n" + _)
        //conv
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString); null
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString); null
    }
  }
}

