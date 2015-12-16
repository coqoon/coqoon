/* RuleRecogniser.scala
 * Simple IRules for matching arbitrary sequences and complete partitions
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

import org.eclipse.jface.text.rules.{IRule, Token, IToken}
import PushdownAutomaton.State

abstract class RuleRecogniser extends PushdownAutomaton[Char] with IRule {
  private var finalStates = Map[State, IToken]()
  protected def addFinalState(s : State, t : IToken) = finalStates += (s -> t)

  import org.eclipse.jface.text.rules.ICharacterScanner
  override def evaluate(scanner : ICharacterScanner) = {
    var count = 0
    var finished = false
    var last = Execution(RuleRecogniser.start, Seq())
    while (!finished) {
      val input = Option(scanner.read).filter(
          _ != ICharacterScanner.EOF).map(_.asInstanceOf[Char])
      input.foreach(_ => count += 1)
      input.flatMap(last.accept) match {
        case Some((_, next)) =>
          last = next
        case None =>
          /* Since reading the last character didn't help, unread it (if there
           * was one!) */
          input.foreach(_ => try scanner.unread finally count -= 1)
          finished = true
      }
    }
    finalStates.get(last.position) match {
      case Some(token) if count > 0 =>
        token
      case _ =>
        while (count > 0) try scanner.unread finally count -= 1
        Token.UNDEFINED
    }
  }
}
protected object RuleRecogniser {
  final lazy val start = State("")
}

class ExtensibleRecogniser extends RuleRecogniser {
  def recognise(s : String, t : IToken) = {
    var i = 0
    while (i < s.length) {
      Actions.BasicTransition(
          State(s.substring(0, i)), s(i), State(s.substring(0, i + 1)))
      i += 1
    }
    addFinalState(State(s), t)
  }
}

class DummyRecogniser(t : IToken) extends RuleRecogniser {
  Actions.DefaultTransition(RuleRecogniser.start, RuleRecogniser.start)
  addFinalState(RuleRecogniser.start, t)
}