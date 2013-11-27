/* CoqSentence.scala
 * Coq sentence extraction
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

package dk.itu.coqoon.core.coqtop

import dk.itu.coqoon.core.utilities.Substring

object CoqSentence {
  final val CommentStart = """^\(\*""".r.unanchored
  final val CommentEnd = """^\*\)""".r.unanchored
  final val QuotationMark = "^\"".r.unanchored
  final val Bullet = """^(\+|-|\*)""".r.unanchored
  final val CurlyBracket = """^(\{|\})(\s|$)""".r.unanchored
  final val FullStop = """^\.(\s|$)""".r.unanchored
  final val Ellipsis = """^\.\.\.(\s|$)""".r.unanchored

  final val DotRun = """^(\.+)(\s|$)""".r.unanchored
  final val WhitespaceRun = """^(\s+)""".r.unanchored

  type Sentence = (Substring, Boolean)

  def getNextSentence(
      doc : CharSequence, offset : Int = 0) : Option[Sentence] = {
    var i = offset
    var commentDepth = 0
    var inString = false
    var content = false
    while (i < doc.length) Substring(doc, i) match {
      case CommentStart() if !inString =>
        commentDepth += 1
        i += 2
      case CommentEnd() if !content && !inString && commentDepth == 1 =>
        return Some((Substring(doc, offset, i + 2), true))
      case CommentEnd() if !inString && commentDepth > 0 =>
        commentDepth -= 1
        i += 2
      case QuotationMark() =>
        inString = !inString
        i += 1
      case FullStop(_) if !inString && commentDepth == 0 =>
        return Some((Substring(doc, offset, i + 1), false))
      case Ellipsis(_) if !inString && commentDepth == 0 =>
        return Some((Substring(doc, offset, i + 3), false))
      case CurlyBracket(t, _) if !content && !inString && commentDepth == 0 =>
        return Some((Substring(doc, offset, i + 1), false))
      case Bullet(_) if !content && !inString && commentDepth == 0 =>
        return Some((Substring(doc, offset, i + 1), false))
      case DotRun(dots, end) if !inString && commentDepth == 0 =>
        content = true
        i += dots.length + end.length
      case WhitespaceRun(ws) =>
        i += ws.length
      case _ =>
        if (commentDepth == 0)
          content = true
        i += 1
    }
    None
  }

  def getNextSentences(
      doc : CharSequence, from : Int, to : Int) : Seq[Sentence] = {
    val steps = Seq.newBuilder[Sentence]
    var offset = from
    while (offset <= to) getNextSentence(doc, offset) match {
      case Some(s @ (text, synthetic)) =>
        offset = text.end
        if (offset <= to)
          steps += s
      case _ => offset = Int.MaxValue
    }
    steps.result
  }

  object Classifier {
    object SectionStartSentence {
      val expr =
        ("^\\s*Section\\s+([a-zA-Z0-9_]+)\\s*\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(identifier) =>
          Some(identifier)
        case _ => None
      }
    }

    object SectionEndSentence {
      val expr =
        ("^\\s*End\\s+([a-zA-Z0-9_]+)\\s*\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(identifier) =>
          Some(identifier)
        case _ => None
      }
    }

    object AssertionSentence {
      val expr =
        ("^\\s*(Theorem|Lemma|Remark|Fact|Corollary|" +
         "Proposition|Definition|Example)\\s+([a-zA-Z0-9_]+)(?s)(.*)\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
    }

    object FixpointSentence {
      val expr =
        ("^\\s*(Fixpoint|CoFixpoint)\\s+([a-zA-Z0-9_]+)(?s)(.*)\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
    }

    object InductiveSentence {
      val expr =
        ("^\\s*(Inductive|CoInductive)\\s+([a-zA-Z0-9_]+)(?s)(.*)\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
    }

    object DefinitionSentence {
      val expr =
        ("^\\s*(Definition|Let)\\s+([a-zA-Z0-9_]+)(?s)(.*):=(.*)\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(keyword, identifier, binders, body) =>
          Some((keyword, identifier, binders, body))
        case _ => None
      }
    }

    object ProofEndSentence {
      val expr = ("^\\s*(Qed|Defined|Admitted)\\s*\\.$").r
      def unapply(input : CharSequence) = input match {
        case expr(keyword) => Some((keyword))
        case _ => None
      }
    }
  }
}

class ParserStack[A, B] {
  import dk.itu.coqoon.core.utilities.TryCast

  var stack = List[Either[A, B]]()

  def push(value : A) = stack +:= Left(value)
  def pushContext(value : B) = stack +:= Right(value)

  def popContext(value : B) : (B, Seq[A]) = popContext(a => value == a)
  def popContext(f : B => Boolean) : (B, Seq[A]) = {
    var (prefix, remainder) = stack.span {
      case Right(v) if f(v) => false
      case Right(v) =>
        throw new Exception(
            "Innermost context " + v + " not accepted by comparator")
      case _ => true
    }
    if (remainder.headOption.flatMap(_.right.toOption).map(f) != Some(true))
      throw new Exception(
          "" + remainder.headOption + " not acceptable to comparator")
    val tag = remainder.head.right.get
    stack = remainder.tail
    /* prefix can only contain Lefts at this point */
    (tag, prefix.map(_.left.get))
  }

  def getStack() : Seq[A] = stack.flatMap(_.left.toSeq)
  def isComplete() = stack.forall(p => p.isLeft)
}