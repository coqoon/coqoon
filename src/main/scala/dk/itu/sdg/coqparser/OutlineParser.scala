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

  case class Assertion (assertionType : String, name : String, args : String, prop : String) extends OutlineSentence {
    override def outlineName = assertionType + " " + name + " : " + prop
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
  
  case class InductiveCase (name : String, `type` : String) extends VernacularRegion {
    override def outlineName = name + " " + `type`
    override val outline = true
  }
  
  case class Inductive (name : String, `type` : String, cases : List[InductiveCase]) extends OutlineSentence {
    override def outlineName = "<I>Inductive " + name + " : " + `type` + cases
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
  
  case class Proof (start : Option[ProofStart], override val contents : List[VernacularRegion], end : String) extends OutlineStructure {
    override def outlineName = "Proof ... " + end
  }

  case class Document (override val contents : List[VernacularRegion]) extends OutlineStructure
}


// Pass 1: split the source document into sentences / tactic applications
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
// End pass 1


//Pass 2: Attempt to parse each sentence
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
    | chrExcept(EofCh, '"')~inString ^^ { case ch~rest => ch :: rest }
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
    | inductive
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
  
  def inductive : Parser[Inductive] =
    "Inductive"~notTok(".", ":=", ":")~rep(notTok(":="))~":="~
    opt("|")~opt(repsep(inductiveCase, "|"))~dot ^^ {
      case _~name~typ~_~_~cases~_ => Inductive(name, typ.mkString(" "), cases getOrElse Nil)
    }
  
  def inductiveCase : Parser[InductiveCase] = tok~rep(notTok("|", ".")) ^^ {
    case name~typ => InductiveCase(name, typ.mkString(" "))
  }
  
  def unknownSentence : Parser[UnknownSentence] =
    rep(tok) ^^ { tokens => UnknownSentence(tokens.mkString(" ")) }

  def assertion : Parser[Assertion] =
    assertionKeyword~tok~rep(notTok(":"))~":"~rep(notDotOrColonEqual)~dot ^^ {
      case kwd~name~args~_~prop~_ => Assertion(kwd, name, args.mkString(" "), prop.mkString(" "))
    }
  
  def assertionKeyword : Parser[String] =
    ( "Theorem"
    | "Lemma"
    | "Instance"
    | "Add"
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
// End pass 2


// Pass 3: Construct the hierarchical structure (group modules, sections, and proofs)
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
      case (a@Assertion(kwd, name, args, prop)) :: rest => a :: buildOutline(findProof(rest, None))
      case s :: ss => s :: buildOutline(ss)
    }
  }
  
  @tailrec
  private def findProof(sentences : List[VernacularRegion], start : Option[ProofStart], soFar : List[VernacularRegion] = Nil) : List[VernacularRegion] = {
    sentences match {
      case Nil => Nil
      case (s@ProofStart()) :: rest => findProof(rest, Some(s), soFar)
      case (pe@ProofEnd(end)) :: rest => {
        val outline = soFar match {
          case Nil => Nil
          case (y:List[VernacularRegion]) => buildOutline(soFar.reverse.tail)
        }
        val proof = Proof(start, outline, end)
        start match {
          case None => ()
          case Some(x) => (x.pos, pe.pos) match {
            case (RegionPosition(offset, _), RegionPosition(endOffset, endLength)) => {
              val length = (endOffset + endLength) - offset
              proof.setPos(offset, length)
            }
          }
          case _ => ()
        }
        proof :: rest
      }
      case s :: rest =>
        //Console.println("findproof for s " + s + "\n\tsofar " + soFar + "\n\tand rest " + rest)
        findProof(rest, start, s :: soFar)
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

// End pass 3

/**
 * Simple test app for sentence splitting and parsing
 */
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
