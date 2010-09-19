import scala.util.parsing.input._

trait JavaAST extends JavaParser
{
	var x : Int = 0
	def parse(r: Reader[Char]) : ParseResult[Any] = { 
		val p = phrase(compilationUnit)(new lexical.Scanner(r))
		
		p match {
			case Success(x @ ~(_,_), _) => coqoutput(x) //pp(x, 0)			// Console.println(x)
      		case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString) 
      		case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString) 
		}
                Console.print("\n");
		p
	}
	
	// override def qualifiedId = rep1sep(id, ".") ^^ { x => x.reduceLeft("")((a,b) => a + "." + b) } ^^ ida
		
	def pp(xs: Any, indent: Int): Unit = {
		def iprint(s: String) = {
			for (i <- 1 to indent) Console.print(" ")
			Console.print(s)
		}
		def iprintln(s: String) = iprint(s); Console.print("\n");
			
		xs match {
			case x1~x2 =>
				pp(x1, indent + 2)
				print(" ~ ")
				pp(x2, indent + 2)
			case xs @ List(_) =>
				for (x <- xs) yield pp(x, indent + 2)
                        case xs : JClass =>
                                iprintln("Class " + xs.id + " (" + xs.body.length + ")"); for (x <- xs.body) yield pp(x, indent + 2)
		        case x @ _ =>
				iprintln(x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
		}
	}
	

  def coqoutput (xs : Any) : Unit = {
    xs match {
      case Nil =>
      case x1~x2 => coqoutput(x1); coqoutput(x2)
      case JClass(id, typ, supers, inters, body) =>
        Console.println("class " + id + " implements " + inters + " supers " + supers); body.foreach(coqoutput(_))
      case xs @ List(_) => xs.foreach(coqoutput(_))
      case x @ _ => Console.println(x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
    }
  }
	
}
