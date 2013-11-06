/* Miscellany.scala
 * Miscellaneous utility classes
 * Copyright © 2013 Alexander Faithfull
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

package dk.itu.coqoon.core.utilities

class Substring(val base : CharSequence, val start : Int, val end : Int)
    extends CharSequence with Seq[Char] {
  private class SubstringIterator extends Iterator[Char] {
    private var position = 0
    override def hasNext = (position < Substring.this.length)
    override def next = try charAt(position) finally position = position + 1
  }

  override def apply(offset : Int) = charAt(offset)
  override def charAt(offset : Int) = base.charAt(start + offset)
  override def length = end - start
  override def subSequence(start : Int, end : Int) =
    new Substring(this, start, end)

  override def iterator : Iterator[Char] = new SubstringIterator

  override def toString = mkString
}
object Substring {
  def apply(base : CharSequence) : Substring =
    apply(base, 0, base.length)
  def apply(base : CharSequence, start : Int) : Substring =
    apply(base, start, base.length)
  def apply(base : CharSequence, start : Int, end : Int) : Substring =
    new Substring(base, start, end)
}

class FunctionIterator[A](f : () => Option[A]) extends Iterator[A] {
  private var cache : Option[A] = None
  private def prepare() : Unit = cache = cache.orElse { f() }
  override def next() = { prepare(); try cache.get finally cache = None }
  override def hasNext() = { prepare(); (cache != None) }
}
object FunctionIterator {
  def apply[A](f : () => A) = new FunctionIterator(() => Option(f()))
  import java.io.{Reader, InputStream, BufferedReader, InputStreamReader}
  def lines(i : InputStream) : FunctionIterator[String] =
    lines(new InputStreamReader(i))
  def lines(i : Reader) : FunctionIterator[String] =
    apply(new BufferedReader(i).readLine)
}

class CacheSlot[A](constructor : () => A) {
  private val lock = new Object

  private var slot : Option[A] = None
  def test() = lock synchronized (slot != None)
  def get() = lock synchronized slot match {
    case Some(x) => x
    case None =>
      slot = Option(constructor()); slot.get
  }
  def set(value : Option[A]) = lock synchronized (slot = value)
  def clear() = set(None)
}
object CacheSlot {
  def apply[A](constructor : => A) = new CacheSlot(() => constructor)
}