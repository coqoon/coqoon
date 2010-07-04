import scala.util.parsing.combinator._

trait JavaImplicits
{
	self: Parsers => 
	
	// If Options could look like 0 or 1 element lists, that'd be lovely.
	implicit def optionToList[T](x: Option[T]): List[T] = x match {
		case Some(s) => List(s)
		case None => List()
	}
	
	// Or, often an Option is really acting as a boolean
	implicit def optionToBoolean[T](x: Option[T]): Boolean = x match {
		case Some(_) => true
		case None => false
	}

	implicit def enrichParser[T](p: Parser[T]) = new RichParser(p)
	implicit def enrichParseResult[T](p: ParseResult[T]) = new RichParseResult(p)
	
	class RichParser[T](p: Parser[T]) {
		// sequential parser that transforms Some(Result) into Result 
		// and discards None
	    def ?~> [U](q: => Parser[U]): Parser[Any] = new Parser[Any] {
			def apply(in: Input) = {
	        	val res1 = p(in)
				val in1 = res1.next
				res1 match {
					case Success(Some(x), _) => Success(x, in1).append(q(in1))
					case _ => q(in1)
					// case _ => Error("?-> should only be used on Options", in)
				}
			}
		}
	}
	
	class RichParseResult[T](x: ParseResult[T]) { }
	
	// wrap opts in this if you want a bool
	def bool[T](p: => Parser[Option[T]]) = p ^^
	{
		case Some(_) => true
		case _ => false
	} 
	
	// or in this if you want a 0-or-1 element list
	def list[T](p: => Parser[Option[T]]) = p ^^
	{
		case Some(x) => List(x)
		case None => List()
	}
	
	/*
	class RichOption[T](value: Option[T]) {
		
	}
	*/
}