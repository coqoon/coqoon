package dk.itu.sdg.coqparser

import dk.itu.sdg.parsing._

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

  private def commandStart = """\S""".r

  private def commandContents =
    ( comment ^^^ " "
    | string
    | not(commandEnd)~!elem("char", (e) => e != EofCh) ^^ { case _~char => char.toString }
    )

  private def commandEnd = '.' ~ (accept('\n') | '\r' | '\t' | ' ' | EofCh)

  def command : Parser[Command] = commandStart~rep(commandContents)~commandEnd ^^ {
    case start~contents~end => Command(start + contents.mkString)
  }

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

class VernacularOutliner extends LengthPositionParsers with TokenParsers with VernacularReserved {
  import OutlineVernacular._

  val lexical = new OutlinerLexer
  type Tokens = OutlinerLexer

  def commandIf (name : String, pred : String => Boolean) : Parser[String] = acceptMatch(name, {
    case lexical.Command(chars) if pred(chars) => chars
  })

  def outline = rep(outlineItem)

  def outlineItem = lengthPositioned(`import` | module | section | sentence) ^^ {x => println(x); x}

  def sentence = definition | assertion | unknown

  private val ImportPattern = """Module\s+Import|Require\s+Import|Import""".r
  def `import` = commandIf("import directive", { str : String =>
    !(ImportPattern findPrefixOf str isEmpty)
  }) ^^ Import

  def definition : Parser[OutlineSentence] = elem("definition", {
    case lexical.Command(chars) =>
      (chars.startsWith("Definition") || chars.startsWith("Let")) && chars.contains(":=")
    case _ => false
  }) ^^ { case lexical.Command(chars) => Definition(chars) }

  private val AssertionPattern =
    """(Theorem|Lemma|Remark|Fact|Corollary|Proposition|Definition|Let|Example)\s+(\S+)""".r
  def assertion : Parser[OutlineSentence] = elem("assertion", {
    case lexical.Command(chars) if !AssertionPattern.findPrefixOf(chars).isEmpty && !chars.contains(":=") => true
    case lexical.Command(chars) if chars.startsWith("Goal") => true
    case _ => false
  })<~proof ^^ {
    case lexical.Command(chars) =>
    (for (AssertionPattern(name) <- AssertionPattern findPrefixOf chars)
     yield Assertion(chars, name)) getOrElse Goal(chars)
  }

  def proof : Parser[Any] = rep1(proofStep)~proofEnd //rep(proofStep)~proofEnd
  def proofStep = not(proofEnd)~!elem("proof step", (s) => true)
  def proofEnd = elem("end of proof", {
    case lexical.Command(chars) if proofEnders contains chars => true
    case lexical.Command(chars) if chars startsWith "Abort" => true
    case lexical.Command(chars) if chars startsWith "Suspend" => true
    case _ => false
  })

  def module : Parser[Module] = for {
    name <- moduleStart
    body <- rep(not(moduleEnd(name))~!outlineItem ^^ { case _~item => item })
    _ <- moduleEnd(name)
  } yield Module(name, body)

  private val ModulePattern = """Module\s+([a-zA-Z0-9\.]+)""".r
  def moduleStart : Parser[String] = elem("Module", {
    case lexical.Command(chars) if chars.startsWith("Module") => true
    case _ => false
  }) ^^ {
    case lexical.Command(chars) => {
      (for (ModulePattern(name) <- ModulePattern findPrefixOf chars)
       yield name) getOrElse ""
    }
  }

  def section : Parser[Section] = for {
    name <- sectionStart
    body <- rep(not(moduleEnd(name))~!outlineItem ^^ { case _~item => item })
    _ <- moduleEnd(name)
  } yield Section(name, body)

  private val SectionPattern = """Section\s+([a-zA-Z0-9]+)""".r
  def sectionStart : Parser[String] = elem("Section", {
    case lexical.Command(chars) if chars.startsWith("Section") => true
    case _ => false
  }) ^^ {
    case lexical.Command(chars) => {
      (for (SectionPattern(name) <- SectionPattern findPrefixOf chars)
       yield name) getOrElse ""
    }
  }

  private val ModuleEndPattern = """End\s(\S+)""".r
  def moduleEnd(name : String) : Parser[Any] =
    elem("End of module/section " + name,
         (cmd : Elem) =>  cmd match {
           case lexical.Command(ModuleEndPattern(chars)) if chars == name => true
           case _ => false
         })

  def unknown : Parser[UnknownSentence] = acceptMatch ("Sentence", {
    case tok : lexical.Command => UnknownSentence(tok.chars)
  })

  def parseString (input : String) : ParseResult[Document] = {
    import scala.util.parsing.input.CharSequenceReader
    phrase(outline)(new lexical.Scanner(input)) map Document
  }
}

object TestOutliner extends VernacularOutliner with Application {

  import scala.util.parsing.input.Reader
  def parse (in : Reader[Char]) : Unit = {
    val p = phrase(outline)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => Console.println("Parse Success: " + x)
      case _ => Console.println("Parse Fail " + p)
    }
  }

  def test () : Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new lexical.Scanner(input)
      var lexResult = collection.mutable.ListBuffer[lexical.Token]()
      while (!scan.atEnd) {
        lexResult += scan.first
        scan = scan.rest
      }
      print("Lexer: ")
      println(lexResult.toList)

      import scala.util.parsing.input.CharSequenceReader
      parse(new CharSequenceReader(input))
      test()
    }
  }
  test()
}
