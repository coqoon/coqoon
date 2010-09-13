package coqscala

trait CoqResponse { }

case class CoqGoal (n : Int, goals : List[String]) extends CoqResponse { }
case class CoqVariablesAssumed (vars : List[String]) extends CoqResponse { }
case class CoqError (message : List[String]) extends CoqResponse { }
case class CoqProofCompleted () extends CoqResponse { }
case class CoqTheoremDefined (theorem : String) extends CoqResponse { }
case class CoqUnknown (stuff : List[String]) extends CoqResponse { }

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.JavaTokenParsers

trait CoqResponseParser extends JavaTokenParsers {
  def top = vars | comp | thmd | erro | goal | rest

  val anything = """[^\n]+""".r
  val number = """[0-9]+""".r

  def goal = number ~ """subgoal[s]?""".r ~ rep1(anything) ^^ { case (x~_)~y => CoqGoal(x.toInt, y) }
  def vars = rep1(ident <~ "is assumed") ^^ CoqVariablesAssumed
  def comp = "Proof completed." ^^ { _ => CoqProofCompleted() }
  def thmd = ident <~ "is defined" ^^ CoqTheoremDefined
  def erro = (
    "Error:" ~> rep1(anything) 
    | "Toplevel input" ~> rep1(anything)
  ) ^^ CoqError
  def rest = rep(anything) ^^ CoqUnknown
}

object ParseCoqResponse extends CoqResponseParser {
  def parse (s : String) : CoqResponse = parseAll(top, s).get
}

