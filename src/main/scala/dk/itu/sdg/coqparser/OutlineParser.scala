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

  case class Assertion (assertionType : String, name : String, args : String, prop : String) extends OutlineSentence {
    override def outlineName = assertionType + " " + name + " : " + prop
  }

  case class Goal (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  case class Import (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  case class ProofStart () extends OutlineSentence {
    override def outlineName = "Proof"
  }
  
  case class ProofEnd (chars : String) extends OutlineSentence {
    override def outlineName = chars
  }
  
  case class ModuleStart (name : String) extends OutlineSentence {
    override def outlineName = "Module " + name
  }
  
  case class SectionStart (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }
  
  case class End (name : String) extends OutlineSentence {
    override def outlineName = "End " + name
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
  
  case class Proof (assertion : Assertion, override val contents : List[VernacularRegion], end : String) extends OutlineStructure {
    override def outlineName = assertion.outlineName + " ... " + end
  }

  case class Document (override val contents : List[VernacularRegion]) extends OutlineStructure
}

object SentenceFinder {
  private val whitespace = Set(' ', '\r', '\n', '\t')
  
  def findCommands(script : String, offset : Int = 0) : List[(Int, Int)] = {
    if (offset >= script.length) Nil
    else if (whitespace contains script(offset)) findCommands(script, offset + 1)
    else {
      (for (end <- getCommand(script, offset))
       yield (offset, end - offset + 1) :: findCommands(script, end + 1)) getOrElse Nil 
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
  private lazy val _delim : Parser[String] = {
    def parseDelim (s : String) : Parser[String] = accept(s.toList) ^^ { x => x.mkString }
    operator.sortWith(_ < _).map(parseDelim).foldRight(failure("no matching special token") : Parser[String]) {
      (x, y) => y | x
    }
  }
  def delim : Parser[Token] = (_delim | """[^a-zA-Z \r\n\t0-9]+""".r) ^^ Tok //_delim

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
    
  def notDot : Parser[String] = accept("not dot", {case Tok(name) if name != "." => name})
  
  def notDotOrColonEqual : Parser[String] = accept("not dot or :=", {
    case Tok(name) if name != "." && name != ":=" => name
  })
  
  def notTok(strs : String*) : Parser[String] = accept("not " + strs.mkString(", "), {
    case Tok(name) if !(strs contains name) => name
  })
    
  def withDot[P](p : Parser[P]) : Parser[P] = p<~dot
    
  def tok : Parser[String] = accept("name", {case Tok(name) => name})
    
  def sentence : Parser[OutlineSentence] =
    ( moduleStartSentence
    | sectionStartSentence
    | endSentence
    | assertion
    | proofStart
    | proofEnd
    | unknownSentence
    )
  
  def moduleStartSentence : Parser[ModuleStart] =
    withDot(Tok("Module")~>(notTok("Import", ".")<~rep(notDot))) ^^ {
      case name if name != "Import" => ModuleStart(name)
    }
  
  def sectionStartSentence : Parser[SectionStart] =
    withDot(Tok("Section")~>(notDot<~rep(notDot))) ^^ {
      case name if name != "Import" => SectionStart(name)
    }
  
  def endSentence : Parser[End] = withDot("End"~>tok) ^^ End
  
  def unknownSentence : Parser[UnknownSentence] =
    rep(tok) ^^ { tokens => UnknownSentence(tokens.mkString(" ")) }

  def assertion : Parser[Assertion] =
    assertionKeyword~tok~rep(notTok(":"))~":"~rep(notDotOrColonEqual)~dot ^^ {
      case kwd~name~args~_~prop~_ => Assertion(kwd, name, args.mkString(" "), prop.mkString(" "))
    }
  
  def assertionKeyword : Parser[String] =
    ( "Theorem"
    | "Lemma"
    | "Remark"
    | "Fact"
    | "Corollary"
    | "Proposition"
    | "Definition"
    | "Example"
    )

  def proofStart : Parser[ProofStart] = "Proof"~dot ^^^ ProofStart()
  
  def proofEnd : Parser[ProofEnd] =
    ("End" | "Qed" | "Admitted" | "Defined" | "Save" | "Proof"~"term")<~dot ^^ {
      case str : String => ProofEnd(str)
      case proof~term => ProofEnd(proof + " " + term)
    }
  
  def parseString (input : String) : ParseResult[OutlineSentence] = {
    import scala.util.parsing.input.CharSequenceReader
    phrase(sentence)(new lexical.Scanner(input))
  }
}

object OutlineBuilder {
  import OutlineVernacular._
  
  val parser = new SentenceParser {}
  def parse(coqSource : String) : Document = {
    val sentences = SentenceFinder.findCommands(coqSource) map {
      case (pos, len) => (pos, len, parser.parseString(coqSource.substring(pos, pos+len)))
    } collect {
      case (pos, len, parser.Success(v, _)) => v.setPos(pos, len); v 
    }
    getDocument(sentences)
  }
  
  def getDocument(sentences : List[OutlineSentence]) : Document =
    Document(buildOutline(sentences))
  
  private def buildOutline(sentences : List[VernacularRegion]) : List[VernacularRegion] = {
    sentences match {
      case Nil => Nil
      case ModuleStart(name) :: rest => buildOutline(findModule({(id, contents) => Module(id, contents)}, rest, name))
      case SectionStart(name) :: rest => println("  ---Section " + name);buildOutline(findModule({(id, contents) => Section(id, contents)}, rest, name))
      case Assertion(kwd, name, args, prop) :: rest => buildOutline(findProof(rest, Assertion(kwd, name, args, prop)))
      case s :: ss => s :: buildOutline(ss)
    }
  }
  
  @tailrec
  private def findProof(sentences : List[VernacularRegion], assertion : Assertion, soFar : List[VernacularRegion] = Nil) : List[VernacularRegion] = {
    sentences match {
      case Nil => Nil
      case ProofStart() :: rest => findProof(rest, assertion, soFar)
      case ProofEnd(end) :: rest => Proof(assertion, buildOutline(soFar.reverse), end) :: rest
      case s :: rest => findProof(rest, assertion, s :: soFar)
    }
  }
  
  @tailrec
  private def findModule(constructor : (String, List[VernacularRegion]) => OutlineStructure, sentences : List[VernacularRegion], name : String, soFar : List[VernacularRegion] = Nil) : List[VernacularRegion] = {
    sentences match {
      case Nil => Nil
      case End(what) :: rest if name == what => constructor(name, buildOutline(soFar.reverse)) :: rest
      case s :: ss => findModule(constructor, ss, name, s :: soFar)
    }
  }
  
}

object TestSentences extends Application with SentenceParser {
  def read(filename : String) = scala.io.Source.fromFile(filename).mkString
  
  def test() : Unit = {
    Console.print("filename> ")
    val input = Console.readLine
    if (input != "q") {
      val text = input //read(input)
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
