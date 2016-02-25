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
  type CoqProjectFile = Seq[CoqProjectEntry]
  def toString(f : CoqProjectFile) =
    f.map(_.toTokens).map(_.mkString(" ")).mkString("\n")
  def fromString(s : String) : CoqProjectFile =
    CoqProjectEntry.fromTokens(shellTokenise(s))

  def shellTokenise(s : CharSequence) = shellTokeniseWithLines(s).flatten
  def shellTokeniseWithLines(s : CharSequence) : Seq[Seq[String]] = {
    var i = 0
    var inString : Option[Char] = None
    var content = false
    var lines = List[List[String]]()
    var words = List[String]()
    var chars = List[Char]()
    def finishWord() =
      if (content) {
        words :+= chars.mkString
        chars = Nil; content = false
      }
    def finishLine() = {
      finishWord
      if (!words.isEmpty) {
        lines :+= words
        words = Nil
      }
    }
    while (i < s.length) s.charAt(i) match {
      case '#' if inString.isEmpty =>
        finishLine
        while (i < s.length && s.charAt(i) != '\n')
          i += 1
        i += 1 /* Skip over the (possible) newline */
      case '\n' if inString.isEmpty =>
        finishLine; i += 1

      /* Beginning and ending strings */
      case '"' if inString.isEmpty =>
        inString = Some('"')
        content = true; i += 1
      case '\'' if inString.isEmpty =>
        inString = Some('\'')
        content = true; i += 1
      case c if inString.contains(c) =>
        inString = None
        i += 1

      case '\\' if inString.isEmpty =>
        s.charAt(i + 1) match {
          case '\n' =>
            /* Escaped newlines should be ignored */
          case q =>
            content = true
            chars :+= q
        }
        i += 2
      case '\\' if inString.contains('\"') =>
        /* Inside double quotes, backslashes can only escape dollar signs,
         * backticks, backslashes, double quotes and newlines, and are
         * otherwise treated as actual backslashes */
        s.charAt(i + 1) match {
          case q @ ('$' | '`' | '\"' | '\\') =>
            chars :+= q
          case '\n' =>
            /* Escaped newlines should be ignored */
          case e =>
            chars :+= '\\'
            chars :+= e
        }
        i += 2
      /* Inside single quotes, backslashes do nothing, so there's no case
       * for them */

      case c if c.isWhitespace && inString.isEmpty && !content =>
        i += 1
      case c if c.isWhitespace && inString.isEmpty =>
        finishWord; i += 1
      case c =>
        content = true; chars :+= c; i += 1
    }
    finishLine
    lines
  }

  def main(args : Array[String]) = {
    var file = List[String]()
    var line : Option[String] = None
    do {
      line.foreach(line => {
        file :+= line
      })
      line = Option(readLine).filter(!_.isEmpty).map(_.trim)
    } while (line != None)
    println(shellTokeniseWithLines(file.mkString("\n")).map(
        _.mkString("[", "; ", "]")).mkString("[", "; ", "]"))
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
      MLPathEntry(dir) +: fromTokens(tail)
    case "-Q" :: physical :: logical :: tail =>
      QualifiedRecursiveEntry(physical, logical) +: fromTokens(tail)
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
case class MLPathEntry(physical : String) extends CoqProjectEntry {
  override def toTokens = Seq("-I", escape(physical))
}
case class QualifiedRecursiveEntry(
    physical : String, logical : String) extends CoqProjectEntry {
  override def toTokens = Seq("-Q", escape(physical), escape(logical))
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