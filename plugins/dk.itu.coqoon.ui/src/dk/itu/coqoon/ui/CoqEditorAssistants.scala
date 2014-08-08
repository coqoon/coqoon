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
    if (c.length == 0 && c.offset < d.getLength &&
        t.exists(TU.endsWith(d.getLegalLineDelimiters, _) != -1) &&
        CoqoonUIPreferences.AutomaticFormatting.get)
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
    /* Filter out offset == 0 (FindReplaceDocumentAdapter does funny things
     * when startOffset is -1) */
    if (offset == 0)
      return (0, "")

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
    val wsCount = CoqoonUIPreferences.SpacesPerIndentationLevel.get
    val innerIdt = containingAssertion.map(
        _ => outerIdt + (" " * wsCount)).getOrElse("")

    val sentenceInfo = {
      import org.eclipse.jface.text.Region
      val (_, content) = quickScanBack(d, c.offset)
      new Region(c.offset - content.length, content.length)
    }
    val sentence = d.get(sentenceInfo.getOffset, sentenceInfo.getLength)

    sentence match {
      case DefinitionSentence(_) =>
        indent.customizeDocumentCommand(d, c)
      case AssertionSentence(keyword, identifier, body) =>
        /* XXX: don't hard-code two spaces */
        c.text += outerIdt + "Proof.\n" + innerIdt
      case ProofStartSentence(keyword) =>
        c.text += innerIdt
      case sentence @ ProofEndSentence(keyword)
          if containingAssertion != None =>
        /* As a safety precaution to avoid mangling lines containing multiple
         * sentences, require that the last sentence contain a newline
         * character before we try to overwrite it */
        if (sentence.contains('\n')) {
          val nlCount = sentence.takeWhile(c => c == '\r' || c == '\n').size
          val trimmedLine =
            sentence.substring(nlCount).dropWhile(_.isWhitespace)
          val fixedSentence = outerIdt + trimmedLine
          if (fixedSentence != sentence) {
            d.replace(sentenceInfo.getOffset + nlCount,
                sentenceInfo.getLength - nlCount, fixedSentence)
            c.offset = sentenceInfo.getOffset + nlCount + fixedSentence.length
          }
        }
        /* Appropriately indenting the next line is always a good idea */
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
  import CoqMasterFormattingStrategy._

  import dk.itu.coqoon.core.model._

  private var builder : Option[StringBuilder] = None

  private def out(startAt : Int,
      indentationLevel : Int, sentence : ICoqScriptSentence) =
    if (sentence.getOffset >= startAt)
      for (t <- normalise(sentence.getText))
        builder.get ++=
          (if (!onlyWhitespace(t)) {
            val wsCount = CoqoonUIPreferences.SpacesPerIndentationLevel.get
            (" " * wsCount * indentationLevel) + t + "\n"
          } else "\n")

  private def loop(startAt : Int,
      indentationLevel : Int, element : ICoqScriptElement) : Unit = {
    element match {
      case s : ICoqScriptSentence =>
        import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier._
        s.getText match {
          case ProofStartSentence(_) | ProofEndSentence(_) |
              IdentifiedEndSentence(_) =>
            /* These should appear in the enclosing scope */
            out(startAt, indentationLevel - 1, s)
          case _ =>
            out(startAt, indentationLevel, s)
        }
      case s : ICoqScriptGroup =>
        out(startAt, indentationLevel, s.getDeterminingSentence)
        for (i <- s.getChildren.tail)
          loop(startAt, indentationLevel + 1, i)
    }
  }

  override protected def format() = {
    import dk.itu.coqoon.core.coqtop.CoqSentence
    import org.eclipse.jface.text.Region

    val document = getMedium.get
    val rawRegion = getRegion.get
    val rawEnd = rawRegion.getOffset + rawRegion.getLength
    val sentences =
      CoqSentence.getNextSentences(document.get, 0, rawEnd).map(_._1)
    val firstSentence = sentences.find(
        s => s.start + s.takeWhile(
            _.isWhitespace).length >= rawRegion.getOffset)
    if (firstSentence != None) {
      val start = firstSentence.get.start
      val region = new Region(start, sentences.last.end - start)
      val content = sentences.mkString
      val dummy = IDetachedCoqVernacFile.createDummy
      val initialWhitespace =
        firstSentence.get.toString.takeWhile(_.isWhitespace)
      dummy.setContents(content)
      builder = Some(new StringBuilder)
      for (i <- dummy.getChildren)
        loop(start, 0, i)
      document.replace(region.getOffset,
          region.getLength, initialWhitespace + builder.get.result.trim)
    }
  }
}
private object CoqMasterFormattingStrategy {
  private def leading(lines : Seq[String]) = {
    var firstActual = lines.find(!onlyWhitespace(_))
    firstActual.map(_.takeWhile(_.isWhitespace)).getOrElse("")
  }
  private def normalise(sentence : String) = {
    val lines = sentence.lines.toStream match {
      case f #:: tail if f.forall(_.isWhitespace) =>
        tail
      case l => l
    }
    var leadingWhitespace = leading(lines)
    for (i <- lines)
      yield i.stripPrefix(leadingWhitespace)
  }

  import java.util.regex.Pattern
  private final val ws = Pattern.compile("""^\s*$""")
  private def onlyWhitespace(s : String) = ws.matcher(s).matches()
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

import dk.itu.coqoon.core.model.{Scanner, StateRule}
import org.eclipse.jface.text.rules.Token

class BasicRule(label : String = "<anonymous>")
    extends StateRule[Char, IToken, BasicRule.BasicState](
        label, Token.UNDEFINED, new BasicRule.BasicState) with IRule {
  override def evaluate(scanner : ICharacterScanner) =
    super.evaluate(BasicRule.BasicScanner(scanner))
}
object BasicRule {
  class BasicState extends StateRule.State[Char, BasicState]
      with StateRule.TokenState[Char, IToken, BasicState]
          with StateRule.FallbackState[Char, BasicState]
  case class BasicScanner(
      scanner : ICharacterScanner) extends Scanner[Char] {
    override def read() =
      Option(scanner.read).filter(
          _ != ICharacterScanner.EOF).map(_.asInstanceOf[Char])
    override def unread() = scanner.unread
  }
}