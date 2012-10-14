/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

sealed abstract class CoqResponse { }

case class CoqGoal (n : Int, goals : List[String]) extends CoqResponse { }
case class CoqVariablesAssumed (vars : String) extends CoqResponse { }
case class CoqError (message : String, msgwithoutnl : String, start : Int, len : Int) extends CoqResponse { }
case class CoqProofCompleted () extends CoqResponse { }
case class CoqTheoremDefined (theorem : String) extends CoqResponse { }
case class CoqUnknown (stuff : String) extends CoqResponse { }
case class CoqUserInterrupt () extends CoqResponse { }
case class CoqWarning (message : String) extends CoqResponse { }
case class CoqShellReady (mono : Boolean, tokens : CoqShellTokens) extends CoqResponse { }
case class CoqSearchResult (name : String) extends CoqResponse { }

object ParseCoqResponse {
  private val ident = """([\p{L}_][\p{L}_0-9']*)"""

  private val NoGoal = """No more subgoals.*""".r
  private val Goal = """([0-9]+) subgoal(.*)""".r
  private val Vars = (ident + """ is assumed""").r
  private val Comp = """Proof completed.""".r
  private val Thmd = (ident + """ is defined""").r
  private val Inte = """User interrupt.""".r
  private val Erro = """(Error:|Toplevel input,)(.*)""".r
  private val Sear = (ident + """:.*""").r

  private val LineSeparator = System.getProperty("line.separator");
  
  def parse (s : String) : CoqResponse = {
    s.split(LineSeparator).take(1)(0) match {
      case Goal(x, _) => CoqGoal(x.toInt, s.split(LineSeparator).toList.drop(1))
      case NoGoal() => CoqProofCompleted()
      case Vars(v) => CoqVariablesAssumed(v)
      case Comp() => CoqProofCompleted()
      case Thmd(t) => CoqTheoremDefined(t)
      case Erro(_, _) => {
        //TODO: what if Error not found, should come up with a sensible message anyways!
        val msg = s.split(LineSeparator).toList
        val mess = msg.indexWhere(_.startsWith("Error"))
        val p =
          if (mess == -1) {
            //"Syntax error" for example!
            val err = msg.indexWhere(_.contains("error"))
            if (err == -1)
              msg.drop(msg.indexWhere(_.contains("^^")))
            else
              msg.drop(err)
          } else
            msg.drop(mess)
        val ps = p.reduceLeft(_ + "\n" + _)
        if (msg.length > 0 && msg(0).contains("characters")) {
          //msg(0) is: Toplevel input, characters xx-yy
          val ff = msg(0).drop(msg(0).indexWhere(_ == 's') + 2).trim
          val split = ff.indexWhere(_ == '-')
          val pos0 = ff.substring(0, split).toInt
          val pos1 = ff.substring(split + 1, ff.length - 1).toInt
          CoqError(ps, p.reduceLeft(_ + " " + _), pos0, pos1 - pos0)
        } else
          CoqError(ps, p.reduceLeft(_ + " " + _), 0, 0)
      }
      case Inte() => CoqUserInterrupt()
      case Sear(n) => CoqSearchResult(n)
      case x => CoqUnknown(s)
    }
  }
}

