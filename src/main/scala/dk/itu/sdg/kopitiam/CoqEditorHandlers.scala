/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

abstract class CoqCommand(val text : String) {
  def run(coqTop : CoqTopIdeSlave_v20120710) : CoqTypes.value[String] =
    coqTop.interp(false, true, text)
}

case class CoqStep(
    val offset : Int,
    override val text : String,
    val synthetic : Boolean) extends CoqCommand(text) {
  override def run(coqTop : CoqTopIdeSlave_v20120710) = if (!synthetic) {
    super.run(coqTop)
  } else CoqTypes.Good("")
}

import scala.collection.mutable.Stack

abstract class CoqEditorHandler extends EditorHandler {
  override def calculateEnabled = (editor != null && !editor.busy)
  override def editor : CoqEditor = TryCast[CoqEditor](super.editor).orNull
}
object CoqEditorHandler {
  final val CommentStart = """^\(\*""".r.unanchored
  final val CommentEnd = """^\*\)""".r.unanchored
  final val QuotationMark = "^\"".r.unanchored
  final val Bullet = """^(\+|-|\*)""".r.unanchored
  final val CurlyBracket = """^(\{|\})(\s|$)""".r.unanchored
  final val FullStop = """^\.(\s|$)""".r.unanchored
  final val Ellipsis = """^\.\.\.(\s|$)""".r.unanchored
  
  final val DotRun = """^(\.+)(\s|$)""".r.unanchored
  final val WhitespaceRun = """^(\s+)""".r.unanchored
  
  def getNextCommand(
      doc : String, offset : Int = 0) : Option[(Substring, Boolean)] = {
    var i = offset
    var commentDepth = 0
    var inString = false
    var content = false
    while (i < doc.length) Substring(doc, i) match {
      case CommentStart() if !inString =>
        commentDepth += 1
        i += 2
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
        content = true
        i += 1
    }
    None
  }
  
  def makeStep(doc : String, offset : Int) : Option[CoqStep] =
    getNextCommand(doc, offset).map(s => CoqStep(offset, s._1.toString, s._2))
  
  def makeSteps(
      doc : String, from : Int, to : Int) : List[CoqStep] = {
    val steps = List.newBuilder[CoqStep]
    var offset = from
    while (offset <= to) {
      makeStep(doc, offset) match {
        case Some(step) =>
          offset = step.offset + step.text.length()
          if (offset <= to)
            steps += step
        case _ => offset = Int.MaxValue
      }
    }
    steps.result
  }
  
  def getStepBackPair[A <: CoqCommand](
      steps : Stack[A], f : Stack[A] => Int) : (Int, Option[A]) = {
    var count : Int = 0
    var mostRecent : Option[A] = None
    steps.synchronized {
      count = f(steps)
      if (count > 0 && steps.length - count > 0)
        mostRecent = Some(steps(count))
    }
    (count, mostRecent)
  }
  
  def doStepBack(
      editor : CoqEditor, f : Stack[CoqStep] => Int, reveal : Boolean = true) = {
    val p = getStepBackPair(editor.steps, f)
    if (p._1 > 0) {
      editor.setUnderway(p._2 match {
        case None => 0
        case Some(x) => x.offset + x.text.length
      })
      editor.setBusy(true)
      new CoqStepBackJob(editor, p._1, reveal).schedule()
    }
  }
}

import org.eclipse.core.commands.ExecutionEvent

class CoqStepForwardHandler extends CoqEditorHandler {
  /* Don't check whether the editor's coqtop instance is busy */
  override def isEnabled = (editor != null)
  
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      CoqEditorHandler.makeStep(editor.document.get, editor.underway).foreach(
          step => {
        // We're running in the UI thread, so always move the underway marker
        editor.setUnderway(step.offset + step.text.length())
        scheduleJob(new CoqStepForwardJob(editor, List(step)))
      })
    null
  }
}

class CoqStepAllHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val doc = editor.document.get
      val steps = CoqEditorHandler.makeSteps(doc, editor.underway, doc.length)
      if (steps.length > 0) {
        editor.setUnderway(steps.last.offset + steps.last.text.length)
        scheduleJob(new CoqStepForwardJob(editor, steps))
      }
    }
    null
  }
}

class CoqStepToCursorHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val underwayPos = editor.underway
      val cursorPos = editor.cursorPosition
      if (cursorPos > underwayPos) { // Forwards!
        val steps = CoqEditorHandler.makeSteps(
          editor.document.get, editor.underway, editor.cursorPosition)
        if (steps.length > 0) {
          editor.setUnderway(steps.last.offset + steps.last.text.length)
          scheduleJob(new CoqStepForwardJob(editor, steps))
        }
      } else if (cursorPos < underwayPos) { // Backwards!
        CoqEditorHandler.doStepBack(editor,
            _.prefixLength(a => (cursorPos < (a.offset + a.text.length))))
      }
    }
    null
  }
}

class CoqStepBackHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      CoqEditorHandler.doStepBack(editor, a => if (a.length > 0) 1 else 0)
    null
  }
  
  override def calculateEnabled =
    super.calculateEnabled && (editor.steps.length > 0)
}

class CoqRetractAllHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      CoqEditorHandler.doStepBack(editor, _.length)
    null
  }
  
  override def calculateEnabled =
    super.calculateEnabled && (editor.steps.length > 0)
}

class RestartCoqHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      new RestartCoqJob(editor).schedule()
    null
  }
}

class CompileCoqHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    import org.eclipse.ui.IFileEditorInput
    if (isEnabled())
      editor.file.foreach(f => new CompileCoqJob(f).schedule())
    null
  }
}