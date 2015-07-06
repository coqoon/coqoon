/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.coqtop.{CoqTypes, CoqSentence}
import dk.itu.coqoon.core.utilities.{TryCast, Substring}
import org.eclipse.ui.IEditorPart

import scala.collection.mutable.Stack

abstract class CoqTopEditorHandler extends EditorHandler {
  protected def getCoqTopContainer() = adaptEditor[CoqTopContainer]

  import org.eclipse.core.runtime.jobs.Job
  protected def scheduleJob(j : Job) = {
    getCoqTopContainer.foreach(_.setBusy(true))
    j.schedule
  }
}

import org.eclipse.core.commands.ExecutionEvent

class InterruptCoqHandler extends CoqTopEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      getCoqTopContainer.foreach(_.coqTop.interrupt)
    null
  }

  override def calculateEnabled = getCoqTopContainer.exists(_.busy)
}

import org.eclipse.ui.menus.UIElement
import org.eclipse.ui.commands.IElementUpdater

class ToggleCoqFlagHandler extends CoqTopEditorHandler with IElementUpdater {
  import CoqTypes._

  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val name = ev.getParameter(
          ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME).
              split(" ").toList
      getCoqTopContainer.flatMap(_.coqTop.getOptionValue(name)) match {
        case Some(BoolValue(v)) => scheduleJob(
            new SetCoqOptionJob(name, BoolValue(!v), getCoqTopContainer.get))
        case _ =>
      }
    }
    null
  }

  /* The states of the UI elements associated with this handler must be updated
   * whenever the active editor changes */
  override protected def editorChanged(
      o : Option[IEditorPart], n : Option[IEditorPart]) =
    if (n != None)
      UIUtils.refreshElements(ManifestIdentifiers.COMMAND_TOGGLE_COQ_FLAG)

  override def updateElement(
      element : UIElement, map : java.util.Map[_, _]) = {
    TryCast[String](map.get(
        ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME)) match {
      case Some(name_) =>
        val name = name_.split(" ").toList
        getCoqTopContainer.flatMap(_.coqTop.getOptionValue(name)) match {
          case Some(BoolValue(v)) => element.setChecked(v)
          case _ =>
        }
      case _ =>
    }
  }
}

abstract class CoqEditorHandler extends CoqTopEditorHandler {
  override def calculateEnabled = getEditor.exists(editor => !editor.busy)
  override def getEditor() : Option[CoqEditor] = super.getEditor.flatMap(TryCast[CoqEditor])
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

class CoqStepForwardHandler extends CoqEditorHandler {
  /* Don't check whether the editor's coqtop instance is busy */
  override def calculateEnabled = (getEditor != None)

  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val editor = getEditor.get
      CoqEditorHandler.makeStep(editor.document.get, editor.underway).foreach(
          step => {
        // We're running in the UI thread, so always move the underway marker
        editor.setUnderway(step.offset + step.text.length())
        scheduleJob(new CoqStepForwardJob(editor, List(step)))
      })
    }
    null
  }
}

class CoqStepAllHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val editor = getEditor.get
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
      val editor = getEditor.get
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
      CoqEditorHandler.doStepBack(
          getEditor.get, a => if (a.length > 0) 1 else 0)
    null
  }

  override def calculateEnabled =
    super.calculateEnabled && getEditor.exists(_.steps.length > 0)
}

class CoqRetractAllHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      CoqEditorHandler.doStepBack(getEditor.get, _.length)
    null
  }

  override def calculateEnabled =
    super.calculateEnabled && getEditor.exists(_.steps.length > 0)
}

class StopCoqHandler extends CoqEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      getEditor.foreach(editor => new StopCoqRunner(editor).run(null))
    null
  }

  override def calculateEnabled = (getCoqTopContainer != null &&
      getCoqTopContainer.exists(_.testFlag(CoqEditor.FLAG_INITIALISED)))
}
