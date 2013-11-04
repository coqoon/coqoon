/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.coqtop.CoqSentence
import dk.itu.coqoon.core.utilities.{TryCast, Substring}

import scala.collection.mutable.Stack

abstract class CoqEditorHandler extends EditorHandler {
  override def calculateEnabled = (editor != null && !editor.busy)
  override def editor : CoqEditor = TryCast[CoqEditor](super.editor).orNull
}
object CoqEditorHandler {
  def makeStep(doc : String, offset : Int) : Option[CoqStep] =
    CoqSentence.getNextSentence(doc, offset).map(
        s => CoqStep(s._1.start, s._1.toString, s._2))

  def makeSteps(doc : String, from : Int, to : Int) : Seq[CoqStep] =
    CoqSentence.getNextSentences(doc, from, to).map(
        s => CoqStep(s._1.start, s._1.toString, s._2))

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

class StopCoqHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      new StopCoqRunner(editor).run(null)
    null
  }

  override def calculateEnabled = (getCoqTopContainer != null &&
      getCoqTopContainer.testFlag(CoqEditor.FLAG_INITIALISED))
}
