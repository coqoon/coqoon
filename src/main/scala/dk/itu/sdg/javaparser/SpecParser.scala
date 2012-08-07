/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.javaparser

sealed abstract class Specification () extends SJExpression with SJBodyDefinition {
  val data : String
}

case class Precondition (override val data : String) extends Specification { }
case class Postcondition (override val data : String) extends Specification { }
case class Loopinvariant (override val data : String) extends Specification { }

case class RepresentationPredicate (override val data : String) extends Specification { }
//either proof script or predicate - depending on the context
case class RawSpecification (override val data : String) extends Specification { }


object ParseSpecification {
  private val Precon = """precondition: (.*)""".r
  private val Postcon = """postcondition: (.*)""".r
  private val Invariant = """invariant: (.*)""".r
  private val Representation = """representation: (.*)""".r

  def parse (s : String) : Specification = {
    s.trim match {
      case Precon(x) => Precondition(x)
      case Postcon(x) => Postcondition(x)
      case Invariant(x) => Loopinvariant(x)
      case Representation(x) => RepresentationPredicate(x)
      case x => RawSpecification(x)
    }
  }
}



