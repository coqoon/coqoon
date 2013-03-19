package dk.itu.sdg.kopitiam

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