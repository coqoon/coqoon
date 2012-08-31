/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

sealed abstract class CoqResponse { }

case class CoqGoal (n : Int, goals : List[String]) extends CoqResponse { }
case class CoqVariablesAssumed (vars : String) extends CoqResponse { }
case class CoqError (message : List[String]) extends CoqResponse { }
case class CoqProofCompleted () extends CoqResponse { }
case class CoqTheoremDefined (theorem : String) extends CoqResponse { }
case class CoqUnknown (stuff : String) extends CoqResponse { }
case class CoqUserInterrupt () extends CoqResponse { }
case class CoqWarning (message : String) extends CoqResponse { }
case class CoqShellReady (mono : Boolean, tokens : CoqShellTokens) extends CoqResponse { }
case class CoqSearchResult (name : String) extends CoqResponse { }

object ParseCoqResponse {
  private val ident = """([\p{L}_][\p{L}_0-9']*)"""

  private val Goal = """([0-9]+) subgoal(.*)""".r
  private val Vars = (ident + """ is assumed""").r
  private val Comp = """Proof completed.""".r
  private val Thmd = (ident + """ is defined""").r
  private val Inte = """User interrupt.""".r
  private val Erro = """(Error:|Toplevel input,)(.*)""".r
  private val Sear = (ident + """:.*""").r

  //FIX: Platform-independent line-separator instead of hardcoded "\n"
  private val LineSeparator = System.getProperty("line.separator");
  
  def parse (s : String) : CoqResponse = {
    s.split(LineSeparator).take(1)(0) match {
      case Goal(x, _) => CoqGoal(x.toInt, s.split(LineSeparator).toList.drop(1))
      case Vars(v) => CoqVariablesAssumed(v)
      case Comp() => CoqProofCompleted()
      case Thmd(t) => CoqTheoremDefined(t)
      case Erro(_, _) => CoqError(s.split(LineSeparator).toList)
      case Inte() => CoqUserInterrupt()
      case Sear(n) => CoqSearchResult(n)
      case x => CoqUnknown(s)
    }
  }
}

