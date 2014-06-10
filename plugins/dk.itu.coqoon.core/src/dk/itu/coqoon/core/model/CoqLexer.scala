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

class SequenceScanner[A](seq : Seq[A]) extends Scanner[A] {
  private var position = 0

  override def read() =
    if (position < seq.length) {
      try Some(seq(position)) finally position += 1
    } else None
  override def unread() = if (position > 0) position -= 1
}

class StateRule[A, T](label : String = "<anonymous>", default : T) {
  import StateRule._

  private val start : State[A, T] = new State

  def recognise(input : Seq[A], token : T) = {
    var s = start
    for (i <- input)
      s = s.require(Some(i), new State[A, T])
    s.setToken(Option(token))
  }

  def getStartState() = start

  def evaluate(scanner : Scanner[A]) : T = {
    var s = Option(start)
    var stack : List[State[A, T]] = List()
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

  override def toString = "BasicRule(" + label + ")"
}
object StateRule {
  class State[A, T] {
    private var next : Map[Option[A], State[A, T]] = Map()
    private var token : Option[T] = None

    def get(c : Option[A]) = next.get(c).orElse(getFallback)
    def require(
        c : Option[A], f : => State[A, T]) : State[A, T] = next.get(c) match {
      case Some(s) => s
      case None =>
        val s = f
        next += (c -> s)
        s
    }

    private var fallback : Option[State[A, T]] = None
    def getFallback() = fallback
    def setFallback(f : Option[State[A, T]]) = (fallback = f)

    def add(c : Option[A], s : State[A, T]) : Unit =
      if (!next.contains(c))
        next += (c -> s)

    def getToken() = token
    def setToken(t : Option[T]) = (token = t)
  }
}
