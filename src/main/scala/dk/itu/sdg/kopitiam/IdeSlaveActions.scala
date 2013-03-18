/* IdeSlaveActions.scala
 * Eclipse Action wrappers for coqtop functionality
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

case class CoqStep(
    offset : Int,
    text : String,
    undo : Pair[Int, Option[String]]) // (rewind-steps, undo-command)

import scala.collection.mutable.Stack
trait Editor extends org.eclipse.ui.IEditorPart {
  def steps : Stack[CoqStep]
  def coqTop : CoqTopIdeSlave_v20120710
  
  def document : String
  def cursorPosition : Int
  
  def underway : Int
  def setUnderway(offset : Int)
  
  def completed : Int
  def setCompleted(offset : Int)
  
  def goals : CoqTypes.goals
  def setGoals(goals : CoqTypes.goals)
}

class NewEditorAction(
    protected val editor : Editor) extends org.eclipse.jface.action.Action

object NewEditorAction {
  def makeStep(doc : String, offset : Int) : Option[CoqStep] = {
    Some(CoqStep(offset, "", (1, None)))
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
}
    
class NewStepOneForwardAction(editor : Editor)
    extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled()) {
      NewEditorAction.makeStep(editor.document, editor.underway) match {
        case Some(step) =>
          // We're running in the UI thread, so always move the underway marker
          editor.setUnderway(step.offset + step.text.length())
          new StepOneForwardJob(editor, step).schedule()
        case _ =>
      }
    }
  }
}

class NewStepToCursorAction(editor : Editor) extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled()) {
      val underwayPos = editor.underway
      val cursorPos = editor.cursorPosition
      if (cursorPos > underwayPos) { // Forwards!
        val steps = NewEditorAction.makeSteps(
          editor.document, editor.underway, editor.cursorPosition)
        if (steps.length > 0) {
          editor.setUnderway(steps.last.offset + steps.last.text.length)
          for (val i <- steps)
            new StepOneForwardJob(editor, i).schedule()
        }
      } else if (cursorPos < underwayPos) { // Backwards!
        
      }
    }
  }
}

class NewRestartCoqAction(editor : Editor) extends NewEditorAction(editor) {
  override def run = {
    if (isEnabled())
      new RestartCoqJob(editor).schedule()
  }
}

import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status}
import org.eclipse.core.runtime.jobs.ISchedulingRule
case class EditorRule(
    editor : Editor) extends ISchedulingRule {
  override def contains(i : ISchedulingRule) = (this == i)
  override def isConflicting(i : ISchedulingRule) = (this == i)
}

abstract class CoqJob(
    name : String,
    editor : Editor) extends org.eclipse.core.runtime.jobs.Job(name) {
  /* Two CoqJobs operating on the same Editor should conflict */
  setRule(EditorRule(editor))
}

private object CoqJob {
  import java.util.{Timer, TimerTask}
  
  var t : Timer = null
  
  def schedule(f : () => Unit) : Unit = {
    if (t != null)
      t.cancel()
    t = new Timer("Kopitiam UI update", true)
    t.schedule(new TimerTask() {
      override def run = f
    }, 200)
  }

  implicit def funcToRunnable(f : () => Unit) : Runnable = new Runnable() {
    override def run = f
  }
  
  def asyncExec(f : () => Unit) =
    org.eclipse.ui.PlatformUI.getWorkbench().getDisplay.asyncExec(f)
}

class RestartCoqJob(editor : Editor) extends CoqJob("Restart Coq", editor) {
  override def run(monitor : IProgressMonitor) = {
    editor.steps.clear
    editor.coqTop.restart
    // CoqJob.schedule(() => StepJob.update(editor, None))
    Status.OK_STATUS
  }
}

abstract class StepJob(
    title : String,
    editor : Editor) extends CoqJob(title, editor) {
  protected def update(editor : Editor, payload : Option[() => Unit]) = {
    val goals = editor.coqTop.goals match {
      case CoqTypes.Good(Some(g)) => g
      case _ => null
    }
    CoqJob.asyncExec(() => {
      val head = editor.steps.synchronized { editor.steps.head }
      editor.setGoals(goals)
      editor.setCompleted(head.offset + head.text.length())
      payload match {
        case Some(f) => f()
        case _ =>
      }
    })
  }
  
  protected def failPayload(ep : Pair[CoqTypes.location, String]) = {
    editor.getEditorSite().getActionBars().
      getStatusLineManager().setErrorMessage(ep._2)
  }
}

class StepOneForwardJob(
    editor : Editor,
    step : CoqStep) extends StepJob("Step forward", editor) {
  override def run(monitor : IProgressMonitor) = {
    editor.coqTop.interp(false, false, step.text) match {
      case CoqTypes.Good(s) =>
        editor.steps.synchronized { editor.steps.push(step) }
        CoqJob.schedule(() =>
          update(editor, None))
      case CoqTypes.Fail(ep) =>
        CoqJob.schedule(() =>
          update(editor,
              Some(() => failPayload(ep._1, ep._2))))
    }
    Status.OK_STATUS
  }
}
