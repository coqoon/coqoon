
trait VernacularRegion { }
case class VernacularDeclaration () extends VernacularRegion { }
case class VernacularDefinition () extends VernacularRegion { }
case class VernacularSyntax () extends VernacularRegion { }
case class VernacularModule () extends VernacularRegion { }
case class VernacularComment (data : List[String]) extends VernacularRegion { }
case class VernacularSentence (data : List[String]) extends VernacularRegion { }
case class VernacularNamespace (head : String, tail : String) extends VernacularRegion { }
case class VernacularDots (left : List[String], right : List[String]) extends VernacularRegion { }

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers

class VernacularLexer extends StdLexical { //with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh

  override def whitespace : Parser[Any] = rep('(' ~ '*' ~ comment)
  override def comment : Parser[Any] = (
    '*' ~ ')' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment
  )
}

trait VernacularParser extends StdTokenParsers with ImplicitConversions {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer

  lexical.delimiters ++= ".; ;\n;\t;\r".split(";").toList
  import lexical.{Identifier,StringLit,NumericLit}

  val keyword = """Axiom Conjecture Parameter Parameters Variable Variables Hypothesis
                   Hypotheses Definition Example Inductive CoInductive Fixpoint CoFixpoint
                   Program Goal Let Remark Fact Corollary Proposition Lemma Theorem Tactic
                   Ltac Notation Infix Add Record Section Module Require Import Export Open
                   Proof End Qed Admitted Save Defined Print Eval Check Hint""".split("""\s+""").toList
  lexical.reserved ++= keyword

  val operator = List("!", "%", "&", "&&", "(", "()", ")",
                      "*", "+", "++", ",", "-", "->", ".",
                      ".(", "..", "/", "/\\", ":", "::", ":<",
                      ":=", ":>", ";", "<", "<-", "<->", "<:",
                      "<=", "<>", "=", "=>", "=_D", ">", ">->",
                      ">=", "?", "?=", "@", "[", "\\/", "]",
                      "^", "{", "|", "|-", "||", "}", "~", "\\", "·=·", "'")

  lexical.delimiters ++= operator

  val keywords = List("_", "as", "at", "cofix", "else", "end",
                      "exists", "exists2", "fix", "for", "forall", "fun",
                      "if", "IF", "in", "let", "match", "mod",
                      "Prop", "return", "Set", "then", "Type", "using",
                      "where", "with")
  
  lexical.reserved ++= keywords

  def string = stringLit

  def ws = " " | "\n" | "\t" | "\r"

  def top = rep1(sentence)

  def sentence = toplevelcommand | "\n"

  //def comment = "(*" ~> rep1(nocomment) <~ "*)" ^^ VernacularComment
  //da (.|...) only valid in proof-mode?
  def toplevelcommand = commandfragment ~ (("." ~ ws) | ("." ~ "." ~ "." ~ ws))
  //what do we need here?
  // -> Module XXX <: PROGRAM
  // -> Definition...
  // -> Build_Class/_Program/_Interface/_Method
  // -> Build_spec (translate to pre/post)

  def commandfragment = assumption | definition | assertion | syntax | module | end ~ ws ~ ident | proof

  def term : Parser[Any] = ident ~ "." ~ ident | ident | "(" ~ opt(ws) ~ rep1sep(term, rep(ws)) ~ opt(ws) ~ ")" | string | numericLit | "::" | "_" | "," | "->" | "++" | "match" | "with" | "|" | "=>" | "*" | ">" | "=" | "end" | "-" | ";" | "fun" | ":" ~ rep(ws)~ term | "?=" | "%" ~ term | ">=" | "<-" | "[" ~ opt(ws) ~ rep1sep(term, rep(ws)) ~ "]" | "/" | "\\" | "!" | "·=·" | "/\\" | "in" | "forall" | "{" ~ rep1sep(term, rep(ws)) ~ "}" | "as" | "@" | ":=" | "'" | "|-" | "()"

  def assumption = assumptionStart ~ assRest

  def assRest = rep1(ident) ~ ":" ~ ident //term

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

  def definition = definitionStart ~ ws ~ rep1sep(ident, rep1(ws)) ~ ws ~ ":=" ~ rep(ws) ~ rep1sep(term, rep(ws))

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

  def assertion = assertionStart ~ ws ~ ident ~ rep(ws) ~ ":" ~ rep(ws) ~ rep1sep(term, rep(ws)) ~ "." ~ rep1(ws) ~ opt(proofStart) ~ proofBody

  def assertionStart = (
    "Remark"
    | "Fact"
    | "Corollary"
    | "Proposition"
    | "Lemma"
    | "Theorem"
  )

  def syntax = syntaxStart ~ ws ~ rep1sep(term, rep(ws))

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
    "Section" ~ ws ~ ident
    | "Module" ~ ws ~ "Import" ~ ws ~ ident ~ ws ~ ":=" ~ ws ~ rep1sep(ident, rep(ws))
    | "Module" ~ ws ~ ident ~ ws ~ "<:" ~ ws ~ rep1sep(ident, rep(ws))
    | "Require" ~ ws ~ ("Import" | "Export") ~ ws ~ ident
    | "Import" ~ ws ~ ident
    | "Open" ~ ws ~ ident ~ ws ~ ident
    )

  def proofStart = "Proof" ~ "." ~ rep1(ws)
  def proofBody = rep(tactics) ~ end
  def proof = proofStart ~ proofBody

  def tactics = rep1sep(term, rep(ws)) ~ ("." | ";") ~ rep1(ws)

  def end = "End" | "Qed" | "Admitted" | "Save" | "Defined"

  def sideeffects = "Print" | "Eval" | "Check"
}

object ParseV extends VernacularParser {
  import scala.util.parsing.input.Reader
  def parse (in : Reader[Char]) : Unit = {
    val p = phrase(top)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => Console.println("success: " + x)
      case _ => Console.println("Fail " + p)
    }
  }
}

object Main extends Application {
  import java.io.{FileInputStream,InputStreamReader,File}
  import scala.util.parsing.input.StreamReader

  override def main (args : Array[String]) = {
    ParseV.parse(StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")))
  }
}
