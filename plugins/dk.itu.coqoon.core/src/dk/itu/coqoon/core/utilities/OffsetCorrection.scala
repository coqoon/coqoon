/* OffsetCorrection.scala
 * Correction from Coq UTF-8 byte offsets to Java char offsets
 * Copyright © 2016 Alexander Faithfull
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

object OffsetCorrection {
  private def utf8len(codepoint : Int) =
    if (codepoint >= 0x0000 && codepoint <= 0x007F) {
      1
    } else if (codepoint >= 0x0080 && codepoint <= 0x07FF) {
      2
    } else if (codepoint >= 0x0800 && codepoint <= 0xFFFF) {
      3
    } else if (codepoint >= 0x10000 && codepoint <= 0x10FFFF) {
      4
    } else {
      /* invalid codepoint */
      0
    }
  private def utf16len(codepoint : Int) =
    Character.charCount(codepoint)

  /* All of the offsets we get out of Coq are byte offsets into UTF-8 strings;
   * they must be converted to UTF-16 char offsets before we can use them to
   * make any decisions at all in the Java world */
  def utf8OffsetToCharOffset(
      offset : Int, sequence : CharSequence) : Option[Int] = {
    var utf8p = 0
    var utf16p = 0
    var high : Option[Char] = None
    var i = 0
    val len = sequence.length
    while (i <= len) {
      if (utf8p == offset) {
        return Some(utf16p)
      } else if (utf8p > offset) {
        return None
      } else if (i >= len) {
        return None
      }
      val c = sequence.charAt(i)
      val point =
        high match {
          case None =>
            c
          case Some(h) =>
            if (Character.isSurrogatePair(h, c)) {
              high = None
              Character.toCodePoint(h, c)
            } else return None
        }
      if (!Character.isHighSurrogate(c)) {
        utf8p += utf8len(point)
        utf16p += utf16len(point)
      } else {
        high = Some(c)
      }
      i += 1
    }
    None
  }

  def fixPair(seq : CharSequence, start : Int, end : Int) =
    utf8OffsetToCharOffset(start, seq).flatMap(
        start => utf8OffsetToCharOffset(end, seq).map(
            end => (start, end)))

  import java.nio.charset.Charset
  private val testVector = "abc🍪de fgæp école"
  val offsets = Seq(
      0 -> Some(0),
      1 -> Some(1),
      2 -> Some(2),
      3 -> Some(3),
      4 -> None,
      5 -> None,
      6 -> None,
      7 -> Some(5),
      8 -> Some(6),
      9 -> Some(7),
      10 -> Some(8),
      11 -> Some(9),
      12 -> Some(10),
      13 -> None,
      14 -> Some(11),
      15 -> Some(12),
      16 -> Some(13),
      17 -> None,
      18 -> Some(14),
      19 -> Some(15),
      20 -> Some(16),
      21 -> Some(17),
      22 -> Some(18),
      23 -> None)
  def main(args : Array[String]) = {
    for ((u8o, co) <- offsets)
      assert(utf8OffsetToCharOffset(u8o, testVector) == co)
    println("OK")
  }
}