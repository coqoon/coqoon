import scala.util.parsing.combinator._ 
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.ImplicitConversions

import scala.util.parsing.input._
import scala.collection.mutable.{Map=>MutableMap}
import scala.actors._
import scala.actors.Actor._

// class Arith extends JavaTokenParsers with RichParser
class Arith extends JavaTokenParsers with RichParser
{ 	
	override type Elem = Char
	type Token = String
	
	def token = ident|decimalNumber
	
	def E: Parser[Any] =
		( E ~~ "+" ~ E
		||! E ~ "-" ~ E
		||! "-" ~ E
		||! E ~ "*" ~ E
		||! E ~ "/" ~ E
		||! E ~ "^" ~ E
		||! "(" ~ E ~ ")"
		||! decimalNumber
		)

	def ding: Parser[Any] = expr
	
	/*
	def |||| [U](q: => Parser[U]): Parser[U] = new Parser[U] {
	  def apply(in: Input) = {
        val res1 = Parser.this(in)
        val res2 = q(in)
        
        (res1, res2) match {
          case (s1 @ Success(_, next1), s2 @ Success(_, next2)) => if (next2.pos < next1.pos) s1 else s2
          case (s1 @ Success(_, _), _) => s1
          case (_, s2 @ Success(_, _)) => s2
          case (e1 @ Error(_, _), _) => e1
          case (f1 @ Failure(_, next1), f2 @ Failure(_, next2)) => if (next2.pos < next1.pos) f1 else f2
          case (f1 @ Failure(_, next1), e2 @ Error(_, next2)) => if (next2.pos < next1.pos) f1 else e2
        }
      }
      override def toString = "|$"
    }
	*/
	
	def expr: Parser[Any] = term ^^^^ rep("+"~term | "-"~term) 
	def term: Parser[Any] = factor ^^^^ rep("*"~factor | "/"~factor) 
	// def factor: Parser[Any] = floatingPointNumber | "("~expr~")" 
	def factor: Parser[Any] = decimalNumber | "("~expr~")" 
} 

object ArithTest extends Arith
{ 
	def main(args: Array[String]) { 
		println("input : "+args(0)) 
		// val cs = new CharSequenceReader(args(0))
		println(parseAll(E, args(0))) 
		// E(new lexical.Scanner(Console.readLine()))
	} 
} 
