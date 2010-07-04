import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.actors._
import scala.actors.Actor._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.Tokens

// http://scala.sygneca.com/patterns/pimp-my-library

trait RichParser extends Scanners with Tokens
{
	override type Elem = Char
	
	def whitespace = rep(whitespaceChar)
	def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)
	
	// when we call a custom combinator and the left operand is a parser object,
	// we want it implicitly converted into our custom object for this method
	implicit def RichParser[T](p: Parser[T]) = new RichParser(p)
	
	class ChildParser[T](parent: Actor, p: Parser[T], in: Input) extends Actor {		
		def act() {			
			val res = p(in)
			if (res.successful) {
				println("success!")
				parent ! res
			}
			
			exit()
		}
	}
	
	class RichParser[T](p: Parser[T]) {
		// def ^^^^ [U](f: T => U): Parser[U] = p.map(f).named(toString+"^^^^")
		def ^^^^ [U](q: => Parser[U]): Parser[~[T, U]] = {
			println("hi!")
			(for(a <- p; b <- q) yield new ~(a,b)).named("~")
		}
		
		def ~~ [U](q: => Parser[U]): Parser[~[T, U]] = {
			println("hi")
			(for(a <- p; b <- q) yield new ~(a,b)).named("~")
		}
		
		// our act method waits for a response from the children
		def response: ParseResult[Any] = {
			var kids = 2
			
			while (kids > 0) { 
				println(kids + " kids left, entering receive")
				receive { 
					case Exit(actor, reason: String) => {
						println("failure: " + reason)
						kids = kids - 1
						if (kids == 0)
							return Failure(reason, null)
					}
					case s @ Success(_, _) => {
						println("success!")
						return s
					}
					case _ => {
						println("unknown message: ") // + msg)
					}
				}
			}
			
			return Failure("fail", null)
		}
		
		// we have parsers self and q and want to kick them off in parallel
		// so we create a new parser and see who comes back first
		def ||! [U >: T](q: => Parser[U]): Parser[Any] = new Parser[Any] {
			println("hello")
			def apply(in: Input) = {
				Actor.self.trapExit = true
				println(in.getClass)
				
				for (val kid <- List(p, q)) {
					val clone = new CharSequenceReader(in.source)
					link(new ChildParser(Actor.self, kid, clone).start)
				}

				response
			}
		}
	}
	
	override def phrase[T](p: Parser[T]) = {
		println(p.toString)
		super.phrase(p)
	}
		
    
  
	
	/*
	class SingleStepScanner(in: Reader[Char]) extends Scanner(in) { 
		private val (tok, rest1, rest2) = whitespace(in) match {
	      case Success(_, in1) => 
	        token(in1) match {
	          case Success(tok, in2) => (tok, in1, in2)
	          case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
	        }
	      case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
	    }
	    private def skip(in: Reader[Char]) = if (in.atEnd) in else in.rest
	    override def source: java.lang.CharSequence = in.source
	    override def offset: Int = in.offset
	    def first = tok
	    def rest = new Scanner(rest2)
	    def pos = rest1.pos
	    def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
	} */
}