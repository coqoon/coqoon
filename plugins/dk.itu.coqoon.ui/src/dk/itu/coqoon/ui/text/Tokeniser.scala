/* Tokeniser.scala
 * A string tokeniser that works by inspecting pushdown automaton transitions
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

class Tokeniser(val automaton : PushdownAutomaton[Char]) {
  import Tokeniser._
  import PushdownAutomaton.{State => Token}

  type TransitionInspector = Transition => Option[(Token, Int)]
  protected object TransitionInspector {
    def apply(transition : TransitionInspector) = append(transition)
    def append(transition : TransitionInspector) =
      appendTransitionInspector(transition)
    def prepend(transition : TransitionInspector) =
      prependTransitionInspector(transition)
  }
  protected object InterestingTransition {
    def apply(transition : Transition, begins : Token, leadin : Int) =
      appendTransitionInspector {
        case t if t == transition => Some((begins, leadin))
        case _ => None
      }
  }

  class TokenIterator(initialToken : Token, start : RType#Execution,
      input : CharSequence) extends Iterator[(Token, String)] {
    private var lastTokenStart = 0
    private var lastTokenType = initialToken
    private var position = 0
    private var state = start

    override def hasNext = {
      prime()
      nextToken != None
    }
    override def next : (Token, String) = {
      prime()
      nextToken match {
        case Some(t) =>
          nextToken = None
          return t
        case None =>
          nextToken.get
      }
    }

    private var nextToken : Option[(Token, String)] = None
    private def prime() : Unit = {
      import dk.itu.coqoon.core.utilities.Substring
      if (nextToken == None) {
        while (nextToken == None && position < input.length) {
          val c = input.charAt(position)
          state.accept(c) match {
            case Some((t, e)) =>
              position += 1
              transitionInspectors.flatMap(ti => ti(t)).headOption foreach {
                case (begins, leadin)
                    if lastTokenType != begins =>
                  val tokenContent = Substring(
                      input, lastTokenStart, position - leadin)
                  nextToken = Some((lastTokenType, tokenContent.toString))
                  lastTokenStart = Math.max(0, position - leadin)
                  lastTokenType = begins
                case _ =>
              }
              state = e
            case _ =>
              /* Oh no, bail out */
              ???
          }
        }

        if (nextToken == None) {
          /* Produce a final token if possible */
          val tokenContent =
            Substring(input, lastTokenStart, position)
          if (tokenContent.length > 0) {
            nextToken = Some((lastTokenType, tokenContent.toString))
            lastTokenStart = position
          }
        }
      }
    }
  }

  private var transitionInspectors : Seq[TransitionInspector] = Seq()
  private def appendTransitionInspector(t : TransitionInspector) =
    transitionInspectors :+= t
  private def prependTransitionInspector(t : TransitionInspector) =
    transitionInspectors +:= t

  def tokens(start : PushdownAutomaton.State,
      input : CharSequence) : Iterator[(Token, String)] =
    new TokenIterator(start, automaton.Execution(start, Seq()), input)
}
object Tokeniser {
  protected type RType = PushdownAutomaton[Char]
  protected type Transition = PushdownAutomaton.Transition[Char]
}