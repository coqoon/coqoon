/* PushdownAutomaton.scala
 * An implementation of a pushdown automaton (of sorts)
 * Copyright © 2015 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

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

    override def toString =
      s"Execution($position, ${stack.mkString("[", ", ", "]")})"
  }
}
object PushdownAutomaton {
  case class Element(label : String) {
    override def toString = s"($label)"
  }
  case class State(label : String) {
    override def toString = s"<$label>"
  }

  case class Transition[C](from : State, pop : Option[Element],
      input : Option[C], push : Option[Element], to : State) {
    override def toString = {
      val pops = pop.map(p => s"popping '$p', ").getOrElse("")
      val pushs = push.map(p => s", pushing '$p'").getOrElse("")
      val inputs = input match {
        case Some(p) =>
          s"matching '$p'"
        case None =>
          s"default transition"
      }
      s"(${from} ---[$pops$inputs$pushs}]--> ${to})"
    }
  }
}