package dk.itu.sdg.coqparser

import dk.itu.sdg.parsing._

import scala.annotation.tailrec

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers, Parsers}
import scala.util.parsing.combinator.token.Tokens

object OutlineVernacular {
  trait OutlineSentence extends VernacularRegion {
    override val outline = true
  }

  case class UnknownSentence (chars : String) extends OutlineSentence {
    override def outlineName = chars.split(":=")(0).replace("""\s+""", " ")
  }

  case class Definition (chars : String) extends OutlineSentence {
    override def outlineName = chars.split(":=")(0).replace("""\s+""", " ")
  }

  case class Assertion (chars : String, name : String) extends OutlineSentence {
    override def outlineName = chars.replace("""\s+""", " ")
  }

  case class Goal (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  case class Import (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  case class ModuleStart (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }
  
  case class SectionStart (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }
  
  case class End (name : String) extends OutlineSentence {
    override def outlineName = "End " + name + "."
  }
  
  trait OutlineStructure extends VernacularRegion {
    override val outline = true
    val contents : List[VernacularRegion] = Nil
  }
  
  case class Module (name : String, override val contents : List[VernacularRegion]) extends OutlineStructure {
    override def toString = "Module " + name + contents.mkString("(", ",", ")")
    override def outlineName = "Module " + name
  }

  case class Section (name : String, override val contents : List[VernacularRegion]) extends OutlineStructure {
    override def toString = "Section " + name + contents.mkString("(", ",", ")")
    override def outlineName = "Section " + name
  }

  case class Document (override val contents : List[VernacularRegion]) extends OutlineStructure
}

object SentenceFinder {
  private val whitespace = Set(' ', '\r', '\n', '\t')
  
  def findCommands(script : String, offset : Int = 0) : Stream[(Int, Int)] = {
    if (offset >= script.length) Stream.Empty
    else if (whitespace contains script(offset)) findCommands(script, offset + 1)
    else {
      (for (end <- getCommand(script, offset))
       yield (offset, end - offset + 1) #:: findCommands(script, end + 1)) getOrElse Stream.Empty 
    }
  }
  
  @tailrec
  private def skipString(script : String, offset : Int) : Option[Int] = {
    if (offset >= script.length) None
    else {
      script(offset) match {
        case '\\' => skipString(script, offset + 2)
        case '"' => Some(offset + 1)
        case _ => skipString(script, offset + 1)
      }
    }
  }
  
  @tailrec
  private def skipComment(script : String, offset : Int, level : Int = 0) : Option[Int] =
    if (offset + 1 >= script.length) None
    else if (script(offset) == '*' && script(offset + 1) == ')') {
      if (level < 1) Some(offset + 1)
      else skipComment(script, offset, level - 1)
    } else if (script(offset) == '(' && script(offset + 1) == '*') {
      skipComment(script, offset + 1, level + 1)
    } else skipComment(script, offset + 1, level + 1)
   
  private def isCommandEnd(script : String, pos : Int) : Boolean =
    pos + 1 < script.length &&
    script(pos) == '.' &&
    (whitespace contains script(pos + 1))
  
  private def getCommand(script : String, ptr : Int) : Option[Int] = {
    if (ptr >= script.length) None // Overshot the end somehow
    else if (ptr == script.length - 1) { // Reached end of proof script
      if (script(ptr) == '.') Some(ptr)
      else None
    }
    else script(ptr) match {
      case '"' =>
        for {
          stringEnd <- skipString(script, ptr + 1)
          cmd <- getCommand(script, stringEnd)
        } yield cmd
      case '(' if script(ptr + 1) == '*' => 
        for {
          commentEnd <- skipComment(script, ptr)
          cmd <- getCommand(script, commentEnd)
        } yield cmd
      case '.' if whitespace contains script(ptr + 1) => Some(ptr)
      case _ => getCommand(script, ptr + 1)
    }
  }
}

trait OutlineTokens extends Tokens {
  case class Tok (chars : String) extends Token
}

class OutlineLexer extends Lexical with VernacularReserved with OutlineTokens with RegexParsers { //with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh

  type Tokens <: OutlineTokens
  override type Elem = Char

  def whitespace = rep('('~'*'~commentContents | '\t' | '\r' | '\n' | ' ')

  def ident : Parser[Token] = """[\p{L}_][\p{L}_0-9']*""".r ^^ Tok //TODO: unicode-id-part from Coq reference

  def accessIdent : Parser[Token] = """\.[\p{L}_][\p{L}_0-9']*""".r ^^ Tok

  def num : Parser[Token] = """-?\d+""".r ^^ Tok

  def string : Parser[Token] = '"'~>inString ^^ { chars => Tok("\"" + chars.mkString + "\"") }
  private def inString : Parser[List[Char]] =
    ( '"'~'"' ~ inString ^^ { case '"'~'"'~rest => '"' :: rest }
    | chrExcept(EofCh, '"') ~ inString ^^ { case ch~rest => ch :: rest }
    | '"' ^^^ Nil
    | failure("String not properly terminated")
    )

  def comment : Parser[Token] =
    ('('~'*')~>commentContents ^^ { chars => Tok("(*" + chars.mkString) }
  private def commentContents : Parser[List[Char]] =
    ( '('~'*'~commentContents~commentContents ^^ { case '('~'*'~nested~rest => '(' :: '*' :: (nested ++ rest) }
    | '*'~')' ^^^ List('*', ')')
    | chrExcept(EofCh)~commentContents ^^ { case char~contents => char :: contents }
    | failure("Comment not finished")
    )

  // Based on technique from scala/util/parsing/combinator/lexical/StdLexical.scala
  private lazy val _delim : Parser[Token] = {
    def parseDelim (s : String) : Parser[Token] = accept(s.toList) ^^ { x => Tok(x.mkString) }
    operator.sortWith(_ < _).map(parseDelim).foldRight(failure("no matching special token") : Parser[Token]) {
      (x, y) => y | x
    }
  }
  def delim : Parser[Token] = _delim

  def token = ident | accessIdent | num | string | delim
}

trait SentenceParser extends Parsers with TokenParsers {
  import OutlineVernacular._
  
  val lexical = new OutlineLexer
  type Tokens = OutlineLexer

  import lexical.Tok 
    
  implicit def acceptLiteral(str : String) : Parser[String] =
    Tok(str) ^^^ str
  
  def dot : Parser[Any] = "."
    
  def withDot[P](p : Parser[P]) : Parser[P] = p<~dot
    
  def tok : Parser[String] = accept("name", {case Tok(name) => name})
    
  def sentence : Parser[OutlineSentence] =
    withDot( moduleStartSentence
           | endSentence
           ) | unknownSentence
  
  def moduleStartSentence : Parser[ModuleStart] =
    "Module"~>(tok<~rep(tok)) ^^ ModuleStart
  
  def endSentence : Parser[End] = "End"~>tok ^^ End
  
  def unknownSentence : Parser[UnknownSentence] =
    rep(tok) ^^ { tokens => UnknownSentence(tokens.mkString(" ")) }

  def parseString (input : String) : ParseResult[VernacularRegion] = {
    import scala.util.parsing.input.CharSequenceReader
    phrase(sentence)(new lexical.Scanner(input))
  }
}


object TestSentences extends Application with SentenceParser {
  def read(filename : String) = scala.io.Source.fromFile(filename).mkString
  
  def test() : Unit = {
    Console.print("filename> ")
    val input = Console.readLine
    if (input != "q") {
      val text = read(input)
      println(text)
      for ((pos, len) <- SentenceFinder.findCommands(text)) {
        val sentence = text.substring(pos, pos+len)
        println("Found [" + parseString(sentence) + "]")
      }
      test()
    } else println("done")
  }
  test()
}
