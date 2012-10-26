/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.util.parsing.input.Positional

sealed abstract class Specification () extends SJExpression with SJBodyDefinition with Positional {
  var data : String
}

case class Precondition (override var data : String) extends Specification { }
case class Postcondition (override var data : String) extends Specification { }
case class Quantification (override var data : String) extends Specification { }
case class Loopinvariant (override var data : String, var frame : String) extends Specification { }

//either proof script or predicate - depending on the context
case class RawSpecification (override var data : String) extends Specification { }


object ParseSpecification {
  private val Lvar = """lvars: (.*)""".r
  private val Prec = """requires: (.*)""".r
  private val Precon = """precondition: (.*)""".r
  private val Postc = """ensures: (.*)""".r
  private val Postcon = """postcondition: (.*)""".r
  private val Invariant = """invariant: (.*) frame: (.*)""".r

  def parse (s : String) : Specification = {
    s.trim match {
      case Precon(x) => Precondition(x)
      case Prec(x) => Precondition(x)
      case Postcon(x) => Postcondition(x)
      case Postc(x) => Postcondition(x)
      case Lvar(x) => Quantification(x)
      case Invariant(x, y) => Loopinvariant(x, y)
      case x => RawSpecification(x)
    }
  }
}



