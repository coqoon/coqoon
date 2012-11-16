/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.javaparser

sealed abstract class Specification () extends SJExpression with SJBodyDefinition {
  var data : String
}

sealed abstract class TopLevelSpecification () extends Specification {
  var method : Option[SJInvokable]
}

case class Precondition (override var data : String, override var method : Option[SJInvokable]) extends TopLevelSpecification { }
case class Postcondition (override var data : String, override var method : Option[SJInvokable]) extends TopLevelSpecification { }
case class Quantification (override var data : String, override var method : Option[SJInvokable]) extends TopLevelSpecification { }
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
      case Precon(x) => Precondition(x, None)
      case Prec(x) => Precondition(x, None)
      case Postcon(x) => Postcondition(x, None)
      case Postc(x) => Postcondition(x, None)
      case Lvar(x) => Quantification(x, None)
      case Invariant(x, y) => Loopinvariant(x, y)
      case x => RawSpecification(x)
    }
  }
}



