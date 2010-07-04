import org.scalacheck._
import Prop._

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

trait UnStdTokens extends StdTokens {
	case class CharLit(chars: String) extends Token {
    	override def toString = "'" + chars + "'"
	}
}

class TestParser extends JavaAST
{
}

object JavaParserSpec extends Properties("JavaParser")
{
	val parser = new TestParser
	val scanner = new parser.lexical.Scanner("{ 1; }")	
	val result = parser.MethodBody(scanner)
	
}