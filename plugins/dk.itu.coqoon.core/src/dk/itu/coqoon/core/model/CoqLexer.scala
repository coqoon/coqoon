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

trait CharacterScanner {
  def read() : Int
  def unread()
}
object CharacterScanner {
  final val EOF = -1
}

class StateRule[T](label : String = "<anonymous>", default : T) {
  import StateRule._

  private val start : State[T] = new State

  def recognise(input : String, token : T) = {
    var s = start
    for (i <- input)
      s = s.require(i)
    s.setToken(token)
  }

  def getStartState() = start

  def evaluate(scanner : CharacterScanner) : T = {
    var s = Option(start)
    var stack : List[State[T]] = List()
    do {
      val c = scanner.read
      s = if (c != CharacterScanner.EOF) {
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
  class State[T] {
    private var next : Map[Int, State[T]] = Map()
    private var token : Option[T] = None

    def get(c : Int) = next.get(c).orElse(getFallback)
    def require(c : Int) : State[T] = next.get(c) match {
      case Some(s) => s
      case None =>
        val s = new State[T]
        next += (c -> s)
        s
    }

    private var fallback : Option[State[T]] = None
    def getFallback() = fallback
    def setFallback(f : State[T]) = (fallback = Option(f))

    def add(c : Int, s : State[T]) : Unit =
      if (!next.contains(c))
        next += (c -> s)

    def getToken() = token
    def setToken(t : T) = (token = Option(t))
  }
}
