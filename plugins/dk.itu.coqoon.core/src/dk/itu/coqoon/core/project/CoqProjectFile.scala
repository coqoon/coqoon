/* CoqProjectFile.scala
 * Read, manipulate and write _CoqProject files
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

package dk.itu.coqoon.core.project

import dk.itu.coqoon.core.utilities.Substring

object CoqProjectFile {
  final val Backslash = "^\\\\".r.unanchored
  final val Whitespace = "^\\s".r.unanchored
  final val QuotationMark = "^\"".r.unanchored

  type CoqProjectFile = Seq[CoqProjectEntry]
  def toString(f : CoqProjectFile) =
    f.map(_.toTokens).map(_.mkString(" ")).mkString("\n")
  def fromString(s : String) : CoqProjectFile =
    CoqProjectEntry.fromTokens(shellTokenise(s))

  def shellTokenise(s : CharSequence) : Seq[String] = {
    var i = 0
    var inString = false
    var content = false
    var token = List[Char]()
    var results = List[String]()
    while (i < s.length) Substring(s, i) match {
      case q @ QuotationMark() =>
        inString = !inString
        content = true; i += 1
      case q @ Backslash() =>
        content = true; token ++= q.subSequence(1, 2); i += 2
      case Whitespace() if !inString && !content =>
        i += 1
      case Whitespace() if !inString =>
        results :+= token.mkString
        content = false; token = List(); i += 1
      case q =>
        content = true; token :+= q.charAt(0); i += 1
    }
    if (content)
      results :+= token.mkString
    results
  }
}

sealed abstract class CoqProjectEntry {
  def toTokens : Seq[String]
}
object CoqProjectEntry {
  private final val ESCAPE = "(\\s|\"|\\\\)".r.unanchored

  def fromTokens(t : Seq[String]) : Seq[CoqProjectEntry] = t match {
    case "-custom" :: command :: deps :: target :: tail =>
      CustomEntry(command, deps, target) +: fromTokens(tail)
    case "-I" :: dir :: tail =>
      IncludeEntry(dir) +: fromTokens(tail)
    case "-R" :: physical :: logical :: tail =>
      RecursiveEntry(physical, logical) +: fromTokens(tail)
    case name :: "=" :: value :: tail =>
      VariableEntry(name, value) +: fromTokens(tail)
    case "-byte" :: tail =>
      ByteEntry +: fromTokens(tail)
    case "-opt" :: tail =>
      NativeEntry +: fromTokens(tail)
    case "-arg" :: option :: tail =>
      ArgEntry(option) +: fromTokens(tail)
    case "-install" :: option :: tail =>
      InstallEntry(option) +: fromTokens(tail)
    case "-f" :: file :: tail =>
      FileEntry(file) +: fromTokens(tail)
    case "-o" :: file :: tail =>
      OutputEntry(file) +: fromTokens(tail)
    case "-h" :: tail =>
      HelpEntry("-h") +: fromTokens(tail)
    case "--help" :: tail =>
      HelpEntry("--help") +: fromTokens(tail)
    case t :: tail =>
      TargetEntry(t) +: fromTokens(tail)
    case Nil => Seq.empty
  }
  def escape(str : String) = str match {
    case ESCAPE(_) =>
      "\"" + str.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    case "" => "\"\""
    case _ => str
  }
}

import CoqProjectEntry._

case class CustomEntry(
    command : String, deps : String, target : String) extends CoqProjectEntry {
  override def toTokens = Seq(
      "-custom", escape(command), escape(deps), escape(target))
}
case class IncludeEntry(dir : String) extends CoqProjectEntry {
  override def toTokens = Seq("-I", escape(dir))
}
case class RecursiveEntry(
    physical : String, logical : String) extends CoqProjectEntry {
  override def toTokens = Seq("-R", escape(physical), escape(logical))
}
case class VariableEntry(
    name : String, value : String) extends CoqProjectEntry {
  override def toTokens = Seq(escape(name), "=", escape(value))
}
case object ByteEntry extends CoqProjectEntry {
  override def toTokens = Seq("-byte")
}
case object NativeEntry extends CoqProjectEntry {
  override def toTokens = Seq("-opt")
}
case class ArgEntry(option : String) extends CoqProjectEntry {
  override def toTokens = Seq("-arg", escape(option))
}
case class InstallEntry(option : String) extends CoqProjectEntry {
  override def toTokens = Seq("-install", escape(option))
}
case class FileEntry(file : String) extends CoqProjectEntry {
  override def toTokens = Seq("-f", escape(file))
}
case class OutputEntry(file : String) extends CoqProjectEntry {
  override def toTokens = Seq("-o", escape(file))
}
case class HelpEntry(repr : String) extends CoqProjectEntry {
  override def toTokens = Seq(repr)
}

case class TargetEntry(t : String) extends CoqProjectEntry {
  override def toTokens = Seq(t)
}