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

case class Substring(val base : CharSequence, val start : Int, val end : Int)
    extends CharSequence {
  def contentEquals(cs : CharSequence) : Boolean =
    if (length == cs.length) {
      var i = 0
      while (i < length)
        try {
          if (charAt(i) != cs.charAt(i))
            return false
        } finally i += 1
      true
    } else false

  override def equals(a : Any) = a match {
    case a : Substring =>
      contentEquals(a)
    case _ =>
      super.equals(a)
  }

  override final lazy val length = end - start

  override def charAt(offset : Int) = base.charAt(start + offset)
  override def subSequence(start : Int, end : Int) =
    new Substring(base, this.start + start, this.start + end)

  private class Iterator extends scala.Iterator[Char] {
    private var position = 0
    override def hasNext() = (position < Substring.this.length)
    override def next() = try charAt(position) finally position += 1
  }
  def iterator() : scala.Iterator[Char] = new Iterator

  override def toString = base.subSequence(start, end).toString
}
object Substring {
  def apply(base : CharSequence) : Substring =
    apply(base, 0, base.length)
  def apply(base : CharSequence, start : Int) : Substring =
    apply(base, start, base.length)
}

object TotalReader {
  import java.io.{Reader, InputStream, BufferedReader, InputStreamReader}
  def read(is : InputStream) : String = read(new InputStreamReader(is))
  def read(r : Reader) : String = _read(new BufferedReader(r))
  private def _read(reader : BufferedReader) = {
    val builder = new StringBuilder
    val buf = new Array[Char](8192)
    var count = 0
    do {
      builder ++= buf.toSeq.take(count)
      count = reader.read(buf)
    } while (count != -1)
    builder.result
  }
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

  def asOption() = lock synchronized slot
}
object CacheSlot {
  def apply[A](constructor : => A) = new CacheSlot(() => constructor)
}