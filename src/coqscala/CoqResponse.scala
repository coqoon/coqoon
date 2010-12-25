package coqscala

trait CoqResponse { }

case class CoqGoal (n : Int, goals : List[String]) extends CoqResponse { }
case class CoqVariablesAssumed (vars : String) extends CoqResponse { }
case class CoqError (message : List[String]) extends CoqResponse { }
case class CoqProofCompleted () extends CoqResponse { }
case class CoqTheoremDefined (theorem : String) extends CoqResponse { }
case class CoqUnknown (stuff : String) extends CoqResponse { }

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.JavaTokenParsers

trait CoqResponseParser extends JavaTokenParsers {
  def top = rep1(vars | comp | thmd | erro | goal | rest)

  val anything = """[^\n]+""".r
  val number = """[0-9]+""".r

  def goal = number ~ """subgoal[s]?""".r ~ rep1(anything) ^^ { case (x~_)~y => CoqGoal(x.toInt, y) }
  def vars = ident <~ "is assumed" ^^ CoqVariablesAssumed
  def comp = "Proof completed." ^^ { _ => CoqProofCompleted() }
  def thmd = ident <~ "is defined" ^^ CoqTheoremDefined
  def erro = (
    "Error:" ~> rep1(anything) 
    | "Toplevel input" ~> rep1(anything)
  ) ^^ CoqError
  def rest = (anything | "\n") ^^ CoqUnknown
}

object ParseCoqResponse extends CoqResponseParser {
  def parse (s : String) : List[CoqResponse] = parseAll(top, s).get
}

