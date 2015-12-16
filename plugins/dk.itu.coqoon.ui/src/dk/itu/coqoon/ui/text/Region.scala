/* Region.scala
 * Support for tracking and manipulating text regions
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

import org.eclipse.jface.text.{IRegion, Position, TypedRegion}

case class Region(start : Int, length : Int) extends IRegion {
  lazy val end = (start + length)
  assert(start >= 0 && length >= 0)

  override def getLength() = length
  override def getOffset() = start

  def move(o : Int) = Region(o, length = length)
  def resize(l : Int) = Region(start, length = l)

  def extend(l : Int) = Region(start, length = length + l)
  def translate(o : Int) = Region(start + o, length = length)
  /* Returns the smallest Region containing both this region and @r. */
  def union(r : Region) = {
    val newStart = Math.min(start, r.start)
    val newLength = Math.max(end, r.end) - newStart
    Region(newStart, length = newLength)
  }
  /* Returns a Region corresponding to the overlap between this region and
   * @r, if such an overlap exists. */
  def intersection(r : Region) : Option[Region] =
    if (r.end < start || r.start >= end) {
      None
    } else {
      val newStart = Math.max(start, r.start)
      val newLength = Math.min(end, r.end) - newStart
      Some(Region(newStart, length = newLength))
    }
  def contains(p : Int) = (p >= start && p < end)
  def contains(r : Region) = (union(r) == this)

  def asTypedRegion(t : String) = new TypedRegion(start, length, t)
  def makePosition() = new Position(start, length)
}