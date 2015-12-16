package dk.itu.coqoon.ui.text

class PushdownAutomaton[C] {
  def this(a : PushdownAutomaton[C]*) = {
    this()
    a.foreach(a => transitions ++= a.transitions)
  }

  import PushdownAutomaton.{State, Element}
  private type Transition = PushdownAutomaton.Transition[C]

  protected object Actions {
    object Transition {
      def apply(from : State, pop : Option[Element],
          input : Option[C], push : Option[Element], to : State) =
        addTransition(
            PushdownAutomaton.Transition(from, pop, input, push, to))
    }
    object BasicTransition {
      def apply(from : State, input : C, to : State) =
        Transition(from, None, Some(input), None, to)
    }
    object DefaultTransition {
      def apply(from : State, to : State) =
        Transition(from, None, None, None, to)
    }
  }

  private var transitions :
      Map[State, Map[(Option[Element], Option[C]), Transition]] = Map()

  private def getTransitions(s : State) = transitions.getOrElse(s, Map())
  private def addTransition(t : Transition) = {
    transitions +=
      (t.from -> (getTransitions(t.from) + ((t.pop, t.input) -> t)))
    t
  }

  case class Execution(position : State, stack : Seq[Element]) {
    def accept(input : C) : Option[(Transition, Execution)] = {
      val transitions = getTransitions(position)
      val transition =
        transitions.get((stack.headOption, Some(input))).orElse(
            transitions.get((None, Some(input)))).orElse(
            transitions.get((None, None)))
      transition match {
        case Some(t) =>
          Some((t, Execution(t.to, t.push.toSeq ++ stack.drop(t.pop.size))))
        case _ =>
          None
      }
    }
  }
}
object PushdownAutomaton {
  case class Element(label : String)
  case class State(label : String)

  case class Transition[C](from : State, pop : Option[Element],
      input : Option[C], push : Option[Element], to : State) {
    override def toString =
      s"(${from} ---[${pop.getOrElse("")}/${input.getOrElse("")}/${push.getOrElse("")}]--> ${to})"
  }
}