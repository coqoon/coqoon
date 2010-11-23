
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

  lexical.delimiters ++= ".; ".split(";").toList
  import lexical.Identifier

  def top = rep1(sentence)

  def sentence = toplevelcommand // | toplevelcommand 
    //declaration | definition | syntax | module | comment | proof

  //def comment = "(*" ~> rep1(nocomment) <~ "*)" ^^ VernacularComment
  def toplevelcommand = rep1(commandfragment) ~ (("." ~ " ") | ("." ~ "." ~ "." ~ " "))
  
  def commandfragment = ident | "." ~ "." ~ ident
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

object ParseV extends VernacularParser {
  import scala.util.parsing.input.CharArrayReader
  def parse (s : String) : Unit = {
    val in = new CharArrayReader(s.toArray)
    val p = phrase(top)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => Console.println("success: " + x)
      case _ => Console.println("Fail " + p)
    }
  }
}

object Main extends Application {
  override def main (args : Array[String]) = {
    ParseV.parse(args(0))
  }
}
