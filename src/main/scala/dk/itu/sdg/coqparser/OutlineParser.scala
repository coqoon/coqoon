package dk.itu.sdg.coqparser

import dk.itu.sdg.parsing._

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers, Parsers}
import scala.util.parsing.combinator.token.Tokens

trait OutlineVernacular {
  trait OutlineSentence extends VernacularRegion {
    override val outline = true
  }

  trait OutlineStructure extends VernacularRegion {
    override val outline = true
    val contents : List[VernacularRegion] = Nil
  }
}

trait OutlinerTokens extends Tokens {
  case class Command (chars : String) extends Token
}

class OutlinerLexer extends Lexical with RegexParsers with OutlinerTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh
  
  override type Elem = Char

  def whitespace = rep('('~'*'~commentContents | '\t' | '\r' | '\n' | ' ')

  def comment : Parser[Any] =
    ('('~'*')~>commentContents
  private def commentContents : Parser[List[Char]] =
    ( '('~'*'~commentContents~commentContents ^^ { case '('~'*'~nested~rest => '(' :: '*' :: (nested ++ rest) }
    | '*'~')' ^^^ List('*', ')')
    | chrExcept(EofCh)~commentContents ^^ { case char~contents => char :: contents }
    | failure("Comment not finished")
    )

  def string : Parser[String] = '"'~>inString ^^ {
    chars => "\"" + chars.mkString + "\""
  }
  private def inString : Parser[List[Char]] =
    ( '"'~'"' ~ inString ^^ { case '"'~'"'~rest => '"' :: rest }
    | chrExcept(EofCh, '"') ~ inString ^^ { case ch~rest => ch :: rest }
    | '"' ^^^ Nil
    | failure("String not properly terminated")
    )

  private def commandStart = """[a-zA-Z]""".r

  private def commandContents =
    ( comment ^^^ " "
    | string
    | not(commandEnd)~>elem("char", (e)=>e != EofCh) ^^ { char => char.toString }
    )

  private def commandEnd = """\.([\r\n\t ]|$)""".r

  def command : Parser[Command] = commandStart~rep(commandContents)~commandEnd ^^ {
    case start~contents~end => Command(start + contents.mkString)
  }
//"""[^\r\n\t ].*\.([\r\n\t ]|$)""".r ^^ Command

  def token = command
}

object TestOutlinerLexer extends OutlinerLexer with Application {
    def test () : Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new Scanner(input)
      var result = collection.mutable.ListBuffer[Token]()
      while (!scan.atEnd) {
        result += scan.first
        scan = scan.rest
      }
      println(result.toList)
      test()
    }
  }
  test()
}

class VernacularOutliner extends LengthPositionParsers with TokenParsers with OutlineVernacular {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer
}
