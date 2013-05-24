/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

abstract class CoqCommand(val text : String) {
  def run(coqTop : CoqTopIdeSlave_v20120710) : CoqTypes.value[String] =
    coqTop.interp(false, false, text)
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
trait Editor extends CoqTopContainer with org.eclipse.ui.IEditorPart {
  def steps : Stack[CoqStep]
  
  def document : String
  def cursorPosition : Int
  
  def underway : Int
  def setUnderway(offset : Int)
  
  def completed : Int
  def setCompleted(offset : Int)
}

abstract class CoqEditorHandler extends EditorHandler {
  override def calculateEnabled = (editor != null && !editor.busy)
  override def editor : Editor = {
    if (super.editor.isInstanceOf[Editor]) {
      super.editor.asInstanceOf[Editor]
    } else null
  }
}
object CoqEditorHandler {
  def makeStep(doc : String, offset : Int) : Option[CoqStep] = {
    val length = CoqTop.findNextCommand(doc.substring(offset))
    if (length != -1) {
      Some(CoqStep(offset, doc.substring(offset, offset + length), false))
    } else None
  }
  
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
  
  def doStepBack(editor : Editor, f : Stack[CoqStep] => Int) = {
    val p = getStepBackPair(editor.steps, f)
    if (p._1 > 0) {
      editor.setUnderway(p._2 match {
        case None => 0
        case Some(x) => x.offset + x.text.length
      })
      editor.setBusy(true)
      new StepBackJob(editor, p._1).schedule()
    }
  }
}

import org.eclipse.core.commands.ExecutionEvent

class StepForwardHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      CoqEditorHandler.makeStep(editor.document, editor.underway) match {
        case Some(step) =>
          // We're running in the UI thread, so always move the underway marker
          editor.setUnderway(step.offset + step.text.length())
          scheduleJob(new StepForwardJob(editor, List(step)))
        case _ =>
      }
    }
    null
  }
}

class StepAllHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val doc = editor.document
      val steps = CoqEditorHandler.makeSteps(doc, editor.underway, doc.length)
      if (steps.length > 0) {
        editor.setUnderway(steps.last.offset + steps.last.text.length)
        scheduleJob(new StepForwardJob(editor, steps))
      }
    }
    null
  }
}

class StepToCursorHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val underwayPos = editor.underway
      val cursorPos = editor.cursorPosition
      if (cursorPos > underwayPos) { // Forwards!
        val steps = CoqEditorHandler.makeSteps(
          editor.document, editor.underway, editor.cursorPosition)
        if (steps.length > 0) {
          editor.setUnderway(steps.last.offset + steps.last.text.length)
          scheduleJob(new StepForwardJob(editor, steps))
        }
      } else if (cursorPos < underwayPos) { // Backwards!
        CoqEditorHandler.doStepBack(editor,
            _.prefixLength(a => (cursorPos < (a.offset + a.text.length))))
      }
    }
    null
  }
}

class StepBackHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      CoqEditorHandler.doStepBack(editor, a => if (a.length > 0) 1 else 0)
    null
  }
  
  override def calculateEnabled =
    super.calculateEnabled && (editor.steps.length > 0)
}

class RetractAllHandler extends CoqEditorHandler {
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
    if (isEnabled()) {
      val f = editor.getEditorInput().asInstanceOf[IFileEditorInput].getFile
      new CoqCompileJob(f).schedule()
    }
    null
  }
}