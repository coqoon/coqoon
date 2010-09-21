import scala.util.parsing.input._

trait JavaAST extends JavaParser
{
  def parse(r: Reader[Char]) : ParseResult[Any] = {
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    p match {
      case Success(x @ ~(_,_), _) => coqoutput(x) //pp(x, 0) // Console.println(x)
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
    }
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


  def coqoutput (xs : Any) : String = {
    xs match {
      case Nil =>
      case x1~x2 => coqoutput(x1); coqoutput(x2)
      case JInterface(id, typ, inters, body) =>
        Console.println("Definition " + id + " := Build_Inter (SS.empty) ") ; body.foreach(coqoutput(_))
      case JClass(id, typ, supers, inters, body) =>
        Console.println("Definition " + id + " := Build_Class "); body.foreach(coqoutput(_))
      case MethodDeclaration(name, typ, parameters, throws, body) =>
        Console.println("SM.add \"" + name + "\" (["); parameters.foreach(coqoutput(_))
        body match {
          case None => 
          case Some(b) => coqoutput(b)
          case Block(xs) => xs.foreach(coqoutput(_))
        }
      case x :: tl => coqoutput(x); tl.foreach(coqoutput(_))
      case Some(x) => coqoutput(x)
      case None =>
      case x => Console.println("leaf " + x.asInstanceOf[AnyRef].getClass().toString() + ": " + x.toString)
    }
    ""
  }
}
