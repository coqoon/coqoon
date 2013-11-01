/* CoqSentence.scala
 * Coq sentence extraction
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

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
}