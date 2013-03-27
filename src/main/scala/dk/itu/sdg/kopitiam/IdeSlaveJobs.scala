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
  /* Make sure that this editor's coqtop instance has been initialised */
  editor.coqTop
}

class InitialiseCoqJob(editor : Editor)
    extends CoqJob("Initialise Coq", editor) {
  override def run(monitor : IProgressMonitor) = {
    editor.preExecuteJob
    val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
    editor.coqTop.interp(false, false, "Add LoadPath \"" + loadp + "\".")
    
    import org.eclipse.ui.IFileEditorInput
    import org.eclipse.core.resources.IResource
    
    val input = editor.getEditorInput
    val res : Option[IResource] =
      if (input.isInstanceOf[IFileEditorInput]) {
        Some(input.asInstanceOf[IFileEditorInput].getFile)
      } else None
      
    res match {
      case Some(r) =>
        editor.coqTop.interp(false, false,
            "Add LoadPath \"" + r.getProject.getLocation.toOSString + "\".")
      case None =>
        Console.println("shouldn't happen - trying to get ProjectDir from " +
            input + ", which is not an IFileEditorInput")
    }

    editor.postExecuteJob
    Status.OK_STATUS
  }
}

private object CoqJob {
  def asyncExec(f : => Unit) =
    org.eclipse.ui.PlatformUI.getWorkbench.getDisplay.asyncExec(
        new Runnable() {
      override def run = f
    })
}

class RestartCoqJob(editor : Editor) extends CoqJob("Restart Coq", editor) {
  override def run(monitor : IProgressMonitor) = {
    editor.steps.synchronized { editor.steps.clear }
    CoqJob.asyncExec {
      editor.setGoals(null)
      editor.setUnderway(0)
    }
    editor.coqTop.restart
    new InitialiseCoqJob(editor).schedule()
    Status.OK_STATUS
  }
}

abstract class StepJob(
    title : String,
    editor : Editor) extends CoqJob(title, editor) {
  protected def doCancel = {
    CoqJob.asyncExec {
      editor.setUnderway(editor.completed)
    }
    editor.postExecuteJob
    Status.CANCEL_STATUS
  }
}

class StepBackJob(
    editor : Editor,
    stepCount : Int) extends StepJob("Step back", editor) {
  override def run(monitor : IProgressMonitor) : IStatus = {
    monitor.beginTask("Step back", stepCount)
    var steps = editor.steps.synchronized { editor.steps.take(stepCount)}
    println(steps)
    Status.OK_STATUS
  }
}

class StepForwardJob(
    editor : Editor,
    steps : List[CoqStep]) extends StepJob("Step forward", editor) {
  override def run(monitor : IProgressMonitor) : IStatus = {
    monitor.beginTask("Step forward", steps.length)
    editor.preExecuteJob
    for (step <- steps) {
      if (monitor.isCanceled())
        return doCancel
      monitor.subTask(step.text.trim)
      editor.coqTop.interp(false, false, step.text) match {
        case CoqTypes.Good(s) =>
          editor.steps.synchronized { editor.steps.push(step) }
          monitor.worked(1)
          CoqJob.asyncExec {
            editor.setCompleted(step.offset + step.text.length)
          }
        case CoqTypes.Fail(ep) =>
          val error = ep._2.trim
          CoqJob.asyncExec {
            editor.setUnderway(editor.completed)
          }
          editor.postExecuteJob
          return new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", error)
      }
    }
    val goals = editor.coqTop.goals match {
      case CoqTypes.Good(Some(g)) => g
      case _ => null
    }
    CoqJob.asyncExec {
      editor.setGoals(goals)
    }
    editor.postExecuteJob
    Status.OK_STATUS
  }
}