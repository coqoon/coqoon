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

import dk.itu.coqoon.core.utilities.{TryCast, Substring}

object CoqSentence {
  final val CommentStart = """^\(\*""".r.unanchored
  final val CommentEnd = """^\*\)""".r.unanchored
  final val QuotationMark = "^\"".r.unanchored
  final val Bullet = """^(\++|-+|\*+)""".r.unanchored
  final val CurlyBracket = """^(\{|\})(\s|$)""".r.unanchored
  final val FullStop = """^\.(\s|$)""".r.unanchored
  final val Ellipsis = """^\.\.\.(\s|$)""".r.unanchored

  final val DotRun = """^(\.+)(\s|$)""".r.unanchored
  final val WhitespaceRun = """^(\s+)""".r.unanchored

  type Sentence = (Substring, Boolean)

  /* Keep this in sync with the Python build script */
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
      case Bullet(b) if !content && !inString && commentDepth == 0 =>
        return Some((Substring(doc, offset, i + b.length), false))
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

  import dk.itu.coqoon.core.model.ICoqScriptSentence
  object Classifier {
    object LtacSentence {
      val expr = ("(?s)^\\s*Ltac\\s+([a-zA-Z0-9_']+)(.*)\\.$").r
      def unapply(input : String) = input match {
        case expr(identifier, body) => Some(identifier, body)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[(String, String)] =
        unapply(s.getText)
    }

    object ModuleStartSentence {
      val expr = ("(?s)^\\s*Module\\s+([a-zA-Z0-9_']+)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(identifier) => Some(identifier)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object SectionStartSentence {
      val expr = ("(?s)^\\s*Section\\s+([a-zA-Z0-9_']+)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(identifier) => Some(identifier)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object IdentifiedEndSentence {
      val expr = ("(?s)^\\s*End\\s+([a-zA-Z0-9_']+)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(identifier) => Some(identifier)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object AssertionSentence {
      val expr =
        ("(?s)^\\s*(Theorem|Lemma|Remark|Fact|Corollary|" +
         "Proposition|Definition|Example)\\s+([a-zA-Z0-9_']+)(.*)\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[(String, String, String)] =
        unapply(s.getText)
    }

    object FixpointSentence {
      val expr =
        ("(?s)^\\s*(Fixpoint|CoFixpoint)\\s+([a-zA-Z0-9_']+)(.*)\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[(String, String, String)] =
        unapply(s.getText)
    }

    object InductiveSentence {
      val expr =
        ("(?s)^\\s*(Inductive|CoInductive)\\s+([a-zA-Z0-9_']+)(.*)\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword, identifier, body) =>
          Some((keyword, identifier, body))
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[(String, String, String)] =
        unapply(s.getText)
    }

    object DefinitionSentence {
      val expr =
        ("(?s)^\\s*(Definition|Let)\\s+([a-zA-Z0-9_']+)(.*):=(.*)\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword, identifier, binders, body) =>
          Some((keyword, identifier, binders, body))
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) :
          Option[(String, String, String, String)] =
        unapply(s.getText)
    }

    object ProofStartSentence {
      val expr = ("(?s)^\\s*(Proof|Next Obligation)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword) => Some(keyword)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object ProofEndSentence {
      val expr = ("(?s)^\\s*(Qed|Defined|Admitted|Abort)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(keyword) => Some(keyword)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object LoadSentence {
      val expr = ("(?s)^\\s*Load\\s+(.*)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(ident) => Some(ident)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object RequireSentence {
      val expr = ("(?s)^\\s*Require\\s+(Import\\s+|Export\\s+|)(.*)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(kind, ident) =>
          val idents : Seq[String] =
            if (ident(0) == '"') {
              Seq(ident.substring(1).split("\"", 2)(0))
            } else ident.split("\\s+")
          Some(kind, idents)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[(String, Seq[String])] =
        unapply(s.getText)
    }

    object FromRequireSentence {
      val expr = ("(?s)^\\s*From\\s+(.*)\\s+Require\\s+(Import\\s+|Export\\s+|)(.*)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(prefix, kind, idents) =>
          Some((prefix, kind, idents.split("\\s+").toSeq))
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) :
          Option[(String, String, Seq[String])] =
        unapply(s.getText)
    }

    object DeclareMLSentence {
      val expr = ("(?s)^\\s*Declare\\s+ML\\s+Module\\s+\"(.*)\"\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(ident) => Some(ident)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object BulletSentence {
      val expr = ("(?s)^\\s*(-+|\\*+|\\++)\\s*$").r
      def unapply(input : String) = input match {
        case expr(bullet) => Some(bullet)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }

    object SubproofSentence {
      val expr = ("(?s)^\\s*\\{\\s*$").r
      def unapply(input : String) = input match {
        case expr() => true
        case _ => false
      }
      def unapply(s : ICoqScriptSentence) : Boolean = unapply(s.getText)
    }

    object EndSubproofSentence {
      val expr = ("(?s)^\\s*\\}\\s*$").r
      def unapply(input : String) = input match {
        case expr() => true
        case _ => false
      }
      def unapply(s : ICoqScriptSentence) : Boolean = unapply(s.getText)
    }

    object SetSentence {
      val expr = ("(?s)^\\s*Set\\s+(.*)\\s*\\.$").r
      def unapply(input : String) = input match {
        case expr(what) => Some(what)
        case _ => None
      }
      def unapply(s : ICoqScriptSentence) : Option[String] = unapply(s.getText)
    }
  }
  object CollectComments {
    def unapplySeq[B <: ICoqScriptSentence](
        input : Seq[B]) : (Seq[B], Seq[B]) =
      input.splitAt(input.indexWhere(_.isSynthetic == false))
  }
}

class ParserStack[A, B] {
  var stack = List[Either[A, B]]()

  def push(value : A) = stack +:= Left(value)
  def pushContext(value : B) = stack +:= Right(value)

  def popContext() : (B, Seq[A]) = {
    var (prefix, remainder) = stack.span {
      case Right(_) => false
      case _ => true
    }
    if (remainder == Nil)
      throw new Exception("No active context")
    val tag = remainder.head.right.get
    stack = remainder.tail
    /* prefix can only contain Lefts at this point */
    (tag, prefix.map(_.left.get))
  }

  /* Returns the innermost context that satisfies the given predicate. */
  def getContext(predicate : B => Boolean) : Option[B] = {
    for (Right(r) <- stack if predicate(r))
      return Some(r)
    None
  }

  def getInnermostContext() : Option[B] = getContext(_ => true)
  def getStack() : Seq[A] = stack.flatMap(_.left.toSeq)
  def isComplete() = stack.forall(p => p.isLeft)
}