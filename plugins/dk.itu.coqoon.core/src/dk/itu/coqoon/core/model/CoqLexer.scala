/* CoqLexer.scala
 * Runtime-modifiable lexing infrastructure for Coq code
 * Copyright © 2014 Alexander Faithfull
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

package dk.itu.coqoon.core.model

trait Scanner[A] {
  def read() : Option[A]
  def unread()
}

trait SeekableScanner[A] extends Scanner[A] {
  def getOffset() : Int
  def setOffset(o : Int)
}

class SequenceScanner[A](seq : Seq[A]) extends SeekableScanner[A] {
  private var position = 0

  override def getOffset = position
  override def setOffset(o : Int) = (position = o)

  override def read() =
    if (position < seq.length) {
      try Some(seq(position)) finally position += 1
    } else None
  override def unread() = if (position > 0) position -= 1
}

import StateRule._

class StateRule[I, T, N <: TokenState[I, T, N]](
    label : String = "<anonymous>", default : T, stateInit : => N) {
  private val start : N = stateInit

  def recognise(input : Seq[I], token : T) = {
    var s = start
    for (i <- input)
      s = s.require(Some(i), stateInit)
    s.setToken(Option(token))
  }

  def getStartState() = start

  def evaluate(scanner : Scanner[I]) : T = {
    var s = Option(start)
    var stack : List[N] = List()
    do {
      val c = scanner.read
      s = if (c != None) {
        val k = s.flatMap(_.get(c))
        if (k != None) {
          stack = k.get +: stack
        } else scanner.unread
        k
      } else None
    } while (s != None)
    while (stack.headOption != None) {
      stack.head.getToken match {
        case Some(token) => return token
        case None =>
          scanner.unread
          stack = stack.tail
      }
    }
    default
  }

  override def toString = "StateRule(" + label + ")"
}
object StateRule {
  trait State[Input, NextState <: State[Input, NextState]] {
    private var next : Map[Option[Input], NextState] = Map()

    def get(in : Option[Input]) : Option[NextState] = next.get(in)
    def require(in : Option[Input], nsb : => NextState) =
        next.get(in) match {
      case Some(next) => next
      case None =>
        val s = nsb
        next += (in -> s)
        s
    }

    def add(in : Option[Input], ns : NextState) =
      if (!next.contains(in))
        next += (in -> ns)
  }

  trait TokenState[Input, Token, NextState <: State[Input, NextState]]
      extends State[Input, NextState] {
    private var token : Option[Token] = None

    def getToken() = token
    def setToken(token : Option[Token]) = (this.token = token)
  }

  trait FallbackState[Input, NextState <: State[Input, NextState]]
      extends State[Input, NextState] {
    private var fallback : Option[NextState] = None

    override def get(in : Option[Input]) = super.get(in).orElse(getFallback)

    def getFallback() = fallback
    def setFallback(fallback : Option[NextState]) =
      (this.fallback = fallback)
  }
}
