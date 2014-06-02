/* CoqEditorAssistants.scala
 * Miscellaneous helper classes for the Coq editor
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.jface.text.{IDocument, TextUtilities => TU, DocumentCommand,
  IAutoEditStrategy, DefaultIndentLineAutoEditStrategy}

class CoqAutoEditStrategy extends IAutoEditStrategy {
  override def customizeDocumentCommand(
      d : IDocument, c : DocumentCommand) = {
    val t = Option(c.text)
    if (c.length == 0 &&
        t.exists(TU.endsWith(d.getLegalLineDelimiters, _) != -1))
      CoqAutoEditStrategy.adjustIndentation(d, c)
  }
}
object CoqAutoEditStrategy extends CoqAutoEditStrategy {
  val indent = new DefaultIndentLineAutoEditStrategy

  object MatchStartFragment {
    val expr = ("^\\s*match\\s+(.*)\\s+with").r
    def unapply(input : CharSequence) = input match {
      case expr(ident) => Some(ident)
      case _ => None
    }
  }

  def quickScanBack(d : IDocument, offset : Int) : (Int, String) = {
    import org.eclipse.jface.text.FindReplaceDocumentAdapter
    val findAdapter = new FindReplaceDocumentAdapter(d)

    val newStart = Option(findAdapter.find(
        offset - 1, """\.\s""", false, false, false, true)).
            map(_.getOffset + 1).getOrElse(0)

    val t = d.get(newStart, offset - newStart)
    val li = t.lastIndexOf("*)")
    (newStart, if (li == -1) t else t.substring(li + 2))
  }

  private def getHelpfulLeadingWhitespace(s : String) : String = {
    for (t <- s.lines;
         head <- Some(t) if head.exists(!_.isWhitespace))
      return head.takeWhile(_.isWhitespace)
    ""
  }

  private def adjustIndentation(d : IDocument, c : DocumentCommand) = {
    import dk.itu.coqoon.core.utilities.Substring
    import dk.itu.coqoon.core.coqtop.CoqSentence
    import CoqSentence.Classifier._

    var qedCount = 0
    var prStart : Int = c.offset
    var containingAssertion : Option[String] = None
    while (prStart != 0) {
      val (newStart, sentence) = quickScanBack(d, prStart)
      val whitespace = getHelpfulLeadingWhitespace(sentence)
      prStart = sentence match {
        case DefinitionSentence(_) | ProofStartSentence(_) =>
          newStart /* keep going */
        case AssertionSentence(_, _, _) if qedCount == 0 =>
          containingAssertion = Some(sentence)
          0 /* stop */
        case AssertionSentence(_, _, _) =>
          qedCount -= 1
          newStart /* keep going */
        case ProofEndSentence(_) if prStart != c.offset =>
          qedCount += 1
          newStart /* keep going */
        case _ if qedCount < 0 || whitespace.length == 0 =>
          0 /* stop */
        case _ =>
          newStart /* keep going */
      }
    }

    val outerIdt = containingAssertion.map(
        getHelpfulLeadingWhitespace).getOrElse("")
    val innerIdt = containingAssertion.map(_ => outerIdt + "  ").getOrElse("")

    val lineInfo = d.getLineInformationOfOffset(c.offset)
    val line = d.get(lineInfo.getOffset, c.offset - lineInfo.getOffset)

    line match {
      case DefinitionSentence(_) =>
        indent.customizeDocumentCommand(d, c)
      case AssertionSentence(keyword, identifier, body) =>
        /* XXX: don't hard-code two spaces */
        c.text += outerIdt + "Proof.\n" + innerIdt
      case ProofStartSentence(keyword) =>
        c.text += innerIdt
      case ProofEndSentence(keyword)
          if containingAssertion != None =>
        val trimmedLine = line.dropWhile(_.isWhitespace)
        val fixedLine = outerIdt + trimmedLine
        if (fixedLine != line) {
          d.replace(lineInfo.getOffset, lineInfo.getLength, fixedLine)
          c.offset = lineInfo.getOffset + fixedLine.length
        }
        c.text += outerIdt
      case _ =>
        indent.customizeDocumentCommand(d, c)
    }
  }
}

import org.eclipse.jface.text.quickassist.{
  IQuickAssistProcessor, IQuickAssistInvocationContext}
import org.eclipse.jface.text.contentassist.ICompletionProposal

class CoqQuickAssistProcessor extends IQuickAssistProcessor {
  import org.eclipse.jface.text.source.Annotation

  override def canAssist(context : IQuickAssistInvocationContext) = {
    println(this + ".canAssist(" + context + ")"); false
  }
  override def canFix(i : Annotation) = {
    println(this + ".canFix(" + i + ")"); false
  }
  override def computeQuickAssistProposals(
      context : IQuickAssistInvocationContext) : Array[ICompletionProposal] = {
    Array()
  }
  override def getErrorMessage = null
}

import utilities.UIUtils
import org.eclipse.ui.{IMarkerResolution, IMarkerResolutionGenerator}
import org.eclipse.core.resources.IMarker

object ConfigureCoqPathResolution extends IMarkerResolution {
  override def getLabel = "Configure the path to Coq"

  override def run(r : IMarker) = {
    import org.eclipse.ui.dialogs.PreferencesUtil
    PreferencesUtil.createPreferenceDialogOn(UIUtils.getActiveShell,
      "Kopitiam.settings", null, null).open
  }
}

class ResolutionGenerator extends IMarkerResolutionGenerator {
  override def getResolutions(m : IMarker) = Array(ConfigureCoqPathResolution)
}

import org.eclipse.jface.text.formatter.{
  IFormattingStrategy, IFormattingStrategyExtension}

class DummyFormattingStrategy extends IFormattingStrategy {
  override def formatterStarts(initialIndentation : String) : Unit = ???
  override def format(a : String,
      b : Boolean, c : String, d : Array[Int]) : String = ???
  override def formatterStops() : Unit = ???
}

abstract class FormattingStrategyBase
    extends DummyFormattingStrategy with IFormattingStrategyExtension {
  import org.eclipse.jface.text.formatter.IFormattingContext

  private var context : Option[IFormattingContext] = None
  protected def getContext() = context

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jface.text.formatter.FormattingContextProperties._

  private def propertyAs[A](t : String)(implicit a0 : Manifest[A]) =
    context.map(_.getProperty(t)).flatMap(TryCast[A])

  import org.eclipse.jface.text.{IRegion, TypedPosition}
  import java.util.{Map => JMap}
  import scala.collection.JavaConversions._
  protected def getDocument() = propertyAs[Boolean](CONTEXT_DOCUMENT)
  protected def getPartition() = propertyAs[TypedPosition](CONTEXT_PARTITION)
  protected def getPreferences() = propertyAs[JMap[_, _]](
      CONTEXT_PREFERENCES).map(f => mapAsScalaMap(f).toMap)
  protected def getRegion() = propertyAs[IRegion](CONTEXT_REGION)
  protected def getMedium() = propertyAs[IDocument](CONTEXT_MEDIUM)

  override def formatterStarts(c : IFormattingContext) = (context = Option(c))
  override def formatterStops = (context = None)
}

class CoqMasterFormattingStrategy extends FormattingStrategyBase {
  override protected def format() = ()
}

class CoqSubservientFormattingStrategy extends FormattingStrategyBase {
  override protected def format() = ()
}

class CommentSubservientFormattingStrategy extends FormattingStrategyBase {
  override protected def format() = ()
}

import org.eclipse.jface.text.rules.{
  IRule, Token, IToken, IWordDetector, ICharacterScanner}

trait IFuturisticWordDetector extends IWordDetector {
  def isWordEnd(character : Char) : Boolean
}

class CoqWordDetector extends IFuturisticWordDetector {
  override def isWordStart(character : Char) = isUnicodeLetter(character)

  override def isWordPart(character : Char) =
    isWordEnd(character) ||
    character == '.' // Not actually true, but good for highlighting

  override def isWordEnd(character : Char) =
    isWordStart(character) || isUnicodeIdPart(character)

  def isUnicodeLetter(character : Char) =
    character.getType == Character.UPPERCASE_LETTER ||
    character.getType == Character.LOWERCASE_LETTER ||
    character.getType == Character.TITLECASE_LETTER ||
    character.getType == Character.OTHER_LETTER ||
    character == '_' || character == '\u00a0'

  def isUnicodeIdPart(character : Char) =
    character.getType == Character.DECIMAL_DIGIT_NUMBER ||
    character.getType == Character.LETTER_NUMBER ||
    character.getType == Character.OTHER_NUMBER ||
    character == '\u0027' // According to Coq, this is a "special space"...
}
object CoqWordDetector extends CoqWordDetector

class FuturisticWordRule(detector : IFuturisticWordDetector,
    default : IToken = Token.UNDEFINED) extends IRule {
  private var tokens : Map[String, IToken] = Map()

  def addWord(word : String, token : IToken) = (tokens += word -> token)

  override def evaluate(scanner : ICharacterScanner) : IToken = {
    var content : Seq[Char] = Seq()
    var i_ = scanner.read()

    while (i_ != -1) {
      val i = i_.asInstanceOf[Char]
      if (content.isEmpty) {
        if (detector.isWordStart(i)) {
          content :+= i
          i_ = scanner.read()
        } else {
          scanner.unread()
          return Token.UNDEFINED
        }
      } else if (detector.isWordPart(i)) {
        content :+= i
        i_ = scanner.read()
      } else {
        scanner.unread()
        i_ = -1
      }
    }

    while (!content.isEmpty && !detector.isWordEnd(content.last)) {
      scanner.unread()
      content = content.dropRight(1)
    }

    if (!content.isEmpty) {
      tokens.get(content.mkString).getOrElse(default)
    } else Token.UNDEFINED
  }
}

import dk.itu.coqoon.core.model.{StateRule, CharacterScanner}
import org.eclipse.jface.text.rules.Token

class BasicRule(label : String = "<anonymous>")
    extends StateRule(label, Token.UNDEFINED) with IRule {
  override def evaluate(scanner : ICharacterScanner) =
    super.evaluate(BasicRule.Scanner(scanner))
}
private object BasicRule {
  case class Scanner(
      scanner : ICharacterScanner) extends CharacterScanner {
    override def read() = scanner.read
    override def unread() = scanner.unread
  }
}