
trait VernacularRegion { }
case class VernacularDeclaration () extends VernacularRegion { }
case class VernacularDefinition () extends VernacularRegion { }
case class VernacularSyntax () extends VernacularRegion { }
case class VernacularModule () extends VernacularRegion { }
case class VernacularComment (data : List[String]) extends VernacularRegion { }
case class VernacularSentence (data : List[String]) extends VernacularRegion { }
case class VernacularNamespace (head : String, tail : String) extends VernacularRegion { }
case class VernacularDots (left : List[String], right : List[String]) extends VernacularRegion { }

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object VernacularParser extends RegexParsers {
  override def skipWhitespace = false

  //nothing is holy! apart from .
  val anything = """[!\"#$%&'()\*\+,-/:;<=>?\[\\\]\^_`\{\|\}~a-zA-Z0-9]+\s*""".r 

  val nocomment = "[^\\*)]+".r

  def top = rep1(sentence)

  def sentence = comment | toplevelcommand 
    //declaration | definition | syntax | module | comment | proof

  def comment = "(*" ~> rep1(nocomment) <~ "*)" ^^ VernacularComment
  def toplevelcommand = rep1(commandfragment) <~ dotws ^^ VernacularSentence
  
  def commandfragment = anything | dot | dotdot | dotdotdot

  val dotdotdot = "..."
  val dotdot = ".."
  val dot = "."
  val dotws = """\.\s+""".r

  def parseItem(s:String) = {
    parse(top, s)
  }
}
/*  def declaration = (
    "Axiom"
    | "Conjecture"
    | "Parameter"
    | "Parameters"
    | "Variable"
    | "Variables"
    | "Hypothesis"
    | "Hypotheses"
    )

  def definition = (
    "Definition"
    | "Example"
    | "Inductive"
    | "CoInductive"
    | "Fixpoint"
    | "CoFixpoint"
    | "Lemma"
    | "Remark"
    | "Fact"
    | "Corollary"
    | "Proposition"
    | "Theorem"
    | "Program"
    | "Goal"
    | "Let"
  )

  def syntax = (
    "Tactic"
    | "Ltac"
    | "Notation"
    | "Infix"
    | "Add"
    | "Record"
  )

  def module = (
    "Section"
    | "Module"
    | "Require"
    | "Import"
    | "Export"
    | "Open"
    )

  def proof = "Proof"

  def end = "End" | "Qed" | "Admitted" | "Save" | "Defined"

  def sideeffects = "Print" | "Eval" | "Check"
*/

