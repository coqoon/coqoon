/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.coqparser

trait VernacularRegion 
case class VernacularDeclaration () extends VernacularRegion { }
case class VernacularDefinition () extends VernacularRegion { }
case class VernacularSyntax () extends VernacularRegion { }
case class VernacularModule () extends VernacularRegion { }
case class VernacularSentence (data : List[String]) extends VernacularRegion { }
case class VernacularNamespace (head : String, tail : String) extends VernacularRegion { }

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers, Parsers}
import scala.util.parsing.combinator.token.Tokens
//import scala.util.parsing.combinator.RegexParsers


trait VernacularReserved {
  // Not technically reserved words, but they work as such. Used to start top-level forms.
  val keyword = """Axiom Conjecture Parameter Parameters Variable Variables Hypothesis
                   Hypotheses Definition Example Inductive CoInductive Fixpoint CoFixpoint
                   Program Goal Let Remark Fact Corollary Proposition Lemma Theorem Tactic
                   Ltac Notation Infix Add Record Section Module Require Import Export Open
                   Proof End Qed Admitted Save Defined Print Eval Check Hint""".split("""\s+""").toList

  val operator = List("!", "%", "&", "&&", "(", "()", ")",
                      "*", "+", "++", ",", "-", "->", ".",
                      ".(", "..", "/", "/\\", ":", "::", ":<", "//\\\\",
                      ":=", ":>", ";", "<", "<-", "<->", "<:",
                      "<=", "<>", "=", "=>", "=_D", ">", ">->",
                      ">=", "?", "?=", "@", "[", "\\/", "]",
                      "^", "{", "|", "|-", "||", "}", "~", "\\", "·=·", "'")

  // The reserved words as listed in the reference manual
  val keywords = List("_", "as", "at", "cofix", "else", "end",
                      "exists", "exists2", "fix", "for", "forall", "fun",
                      "if", "IF", "in", "let", "match", "mod",
                      "Prop", "return", "Set", "then", "Type", "using",
                      "where", "with")
}

trait CoqTokens extends Tokens {
  case class Ident (chars : String) extends Token
  case class AccessIdent (chars : String) extends Token
  case class Delim(chars : String) extends Token //the "special tokens" in the Coq reference
  case class Num (chars : String) extends Token
  case class StringLit (chars : String) extends Token
  case class Comment (chars: String) extends Token
  case class Keyword (chars : String) extends Token
}

class VernacularLexer extends Lexical with VernacularReserved with CoqTokens with RegexParsers { //with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh

  type Tokens <: CoqTokens
  override type Elem = Char

  def whitespace = """[\t\n\ ]*""".r
  
  def ident : Parser[Token] = """[\p{L}_][\p{L}_0-9']*""".r ^^ processIdent// TODO: unicode-id-part from Coq reference
  private def processIdent(name : String) : Token =
	if (keywords contains name)
	  Keyword(name)
	else
      Ident(name)
  
  def accessIdent : Parser[Token] = """\.[\p{L}_][\p{L}_0-9']*""".r ^^ AccessIdent
  
  def num : Parser[Token] = """-?\d+""".r ^^ Num
  
  def string : Parser[Token] = '"'~>inString ^^ {chars => StringLit(chars.mkString)}
  private def inString : Parser[List[Char]] =
    ( '"' ~ '"' ~ inString ^^ {case '"'~'"'~rest => '"' :: rest}
    | chrExcept(EofCh, '"') ~ inString ^^ {case ch~rest => ch :: rest}
    | '"' ^^^ Nil
    | failure("String not properly terminated")
    )
    
  def comment : Parser[Token] =
	('('~'*')~>commentContents ^^ {chars => Comment("(*" + chars.mkString)}
  private def commentContents : Parser[List[Char]] =
	( '('~'*'~commentContents~commentContents ^^ {case '('~'*'~nested~rest => '(' :: '*' :: (nested ++ rest)}
	| '*'~')' ^^^ List('*', ')')
	| chrExcept(EofCh)~commentContents ^^ {case char~contents => char :: contents}
	| failure("Comment not finished")
    )
  
  // Based on technique from scala/util/parsing/combinator/lexical/StdLexical.scala
  private lazy val _delim : Parser[Token] = {
	def parseDelim(s : String): Parser[Token] = accept(s.toList) ^^ {x => Delim(x mkString)}
	operator.sortWith(_<_).map(parseDelim).foldRight(failure("no matching special token"): Parser[Token]) {
		(x, y) => y | x
	}
  }
  def delim : Parser[Token] = _delim
  
  def token = ident | accessIdent | num | string | comment | delim
}

object TestLexer extends VernacularLexer with Application {
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


trait VernacularParser extends TokenParsers with ImplicitConversions with VernacularReserved {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer

  import lexical.{Ident, StringLit, Num, Keyword, Delim}

  def ident = elem("identifier", _.isInstanceOf[Ident])
    
  def ident(str : String) = elem("identifier " + str, {
    case Ident(name) if name == str => true
    case _ => false
  })

  def keyword(str : String) = elem("keyword " + str, {
    case Keyword(name) if name == str => true
    case _ => false
  })
  
  def delim(str : String) = elem("delimiter " + str, {
    case Delim(name) if name == str => true
    case _ => false
  })
  
  def string = elem("string", _.isInstanceOf[StringLit])

  def top = rep1(sentence)

  def sentence = toplevelcommand | "\n"

  //def comment = "(*" ~> rep1(nocomment) <~ "*)" ^^ VernacularComment
  //da (.|...) only valid in proof-mode?
  def toplevelcommand = commandfragment ~ (("." <~ ws) | ("." ~ "." ~ ("." <~ ws)))
  //what do we need here?
  // -> Module XXX <: PROGRAM -> CT
  // -> Definition...
  // -> Build_Class/_Program/_Interface/_Method
  // -> Build_spec (translate to pre/post)

  def commandfragment = assumption | definition | assertion | syntax | module | (end <~ ws) ~ ident | proof | sideeffect

  def term : Parser[Any] = ident ~ delim(".") ~ ident | ident | (delim("(") <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ (opt(ws) ~> ")") | string | numericLit ^^ { case x => x.toInt } | "::" | "_" | "," | "->" | "++" | "match" ~ rep(ws) ~ term ~ rep(ws) ~ "with" ~ rep(ws) ~ rep1sep(term, rep(ws)) ~ rep(ws) ~ "end" | "|" | "=>" | "*" | ">" | "=" | "-" | ";" | "fun" | (":" <~ rep(ws)) ~ term | "?=" | "%" ~ term | ">=" | "<-" | ("[" <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ "]" | "/" | "\\" | "!" | "·=·" | "/\\" | "in" | "forall" | "{" ~ rep1sep(term, rep(ws)) ~ "}" | "as" | "@" | ":=" | "'" | "|-" | "()" | "//\\\\" | sort

  def sort = ident("Prop") | ident("Set") | ident("Type")

  def assumption = assumptionStart ~ assRest

  def assRest = rep1(ident) ~ delim(":") ~ ident //term

  def assumptionStart = (
    "Axiom"
    | "Conjecture"
    | "Parameter"
    | "Parameters"
    | "Variable"
    | "Variables"
    | "Hypothesis"
    | "Hypotheses"
    )

  def name = ( ident | keyword("_") )

  def binders = rep1(binder)

  def binder = (
    name
    | "(" ~ name ~ ":" ~ term ~ ")"
    | "(" ~ name ~ opt(rep(ws) ~ ":" ~ term) ~ rep(ws) ~ ":=" ~ rep(ws) ~ term
  )

  def definition = (definitionStart <~ ws) ~ ident ~ rep(ws) ~ opt(binders) ~ opt(opt(ws) ~ ":" ~ rep(ws) ~ term) ~ ws ~ ":=" ~ rep(ws) ~ rep1sep(term, rep(ws))

  def definitionStart = (
    "Definition"
    | "Example"
    | "Inductive"
    | "CoInductive"
    | "Fixpoint"
    | "CoFixpoint"
    | "Program"
    | "Goal"
    | "Let"
  )

  def assertion = (assertionStart <~ ws) ~ (ident <~ rep(ws)) ~ (":" <~ rep(ws)) ~ rep1sep(term, rep(ws)) ~ ("." <~ rep1(ws)) ~ opt(proofStart) ~ proofBody

  def assertionStart = (
    "Remark"
    | "Fact"
    | "Corollary"
    | "Proposition"
    | "Lemma"
    | "Theorem"
  )

  def syntax = (syntaxStart <~ ws) ~ rep1sep(term, rep(ws))

  def syntaxStart = (
    "Tactic"
    | "Ltac"
    | "Notation"
    | "Infix"
    | "Add"
    | "Record"
    | "Hint"
  )

  def module = moduleStart

  def moduleStart = (
    ("Section" <~ ws) ~ ident
    | ("Module" <~ ws) ~ ("Import" <~ ws) ~ (ident <~ ws) ~ (":=" <~ ws) ~ rep1sep(ident, rep(ws))
    | ("Module" <~ ws) ~ (ident <~ ws) ~ ("<:" <~ ws) ~ rep1sep(ident, rep(ws))
    | ("Require" <~ ws) ~ (("Import" | "Export") <~ ws) ~ ident
    | ("Import" <~ ws) ~ ident
    | ("Open" <~ ws) ~ (ident <~ ws) ~ ident
    )

  def proofStart = "Proof" ~ ("." <~ rep1(ws))
  def proofBody = rep(tactics) ~ end
  def proof = proofStart ~ proofBody

  def tactics = rep1sep(term, rep(ws)) ~ (("." | ";") <~ rep1(ws))

  def end = "End" | "Qed" | "Admitted" | "Save" | "Defined"

  def sideeffect = sideeffectstart ~ rep(ws) ~ rep1sep(term, rep(ws))
  def sideeffectstart = "Print" | "Eval" | "Check"
}

object VernacularDefinitions {
  import scala.collection.mutable.HashMap
  val defs = new HashMap[String, Object]()
}

object ParseV extends VernacularParser {
  import scala.util.parsing.input.Reader
  def parse (in : Reader[Char]) : Unit = {
    val p = phrase(top)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => //Console.println("success: " + x)
      case _ => Console.println("Fail " + p)
    }
  }
}

/*
object Main extends Application {
  import java.io.{FileInputStream,InputStreamReader,File}
  import scala.util.parsing.input.StreamReader

  override def main (args : Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    ParseV.parse(StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")))
  }
}
*/

