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
  protected val partialCCB = Some(() => editor.setUnderway(editor.completed))
  
  protected def doCancel = doComplete(partialCCB, Status.CANCEL_STATUS)
  
  protected def doStep(step : CoqStep) : CoqTypes.value[String] =
    if (step.synthetic)
      CoqTypes.Good("")
    else
      editor.coqTop.interp(false, false, step.text)
  
  protected def doComplete(
      f : Option[() => Unit], status : IStatus) : IStatus = {
    val goals = editor.coqTop.goals match {
      case CoqTypes.Good(Some(g)) => g
      case _ => null
    }
    CoqJob.asyncExec {
      f.map { _() }
      editor.setGoals(goals)
    }
    editor.postExecuteJob
    status
  }
}

class StepBackJob(
    editor : Editor,
    stepCount : Int) extends StepJob("Step back", editor) {
  override def run(monitor : IProgressMonitor) : IStatus = {
    monitor.beginTask("Step back", stepCount)
    val steps = editor.steps.synchronized { editor.steps.take(stepCount) }
    val rewindCount = steps.count(_.synthetic == false)
    editor.coqTop.rewind(rewindCount) match {
      case CoqTypes.Good(extra) =>
        editor.steps.synchronized {
          for (step <- steps)
            editor.steps.pop
          
          if (extra > 0) {
            // XXX: synthetic steps
            var redoSteps = editor.steps.take(extra).toList.reverse
            for (step <- redoSteps)
              editor.steps.pop
            new StepForwardJob(editor, redoSteps).schedule()
          }
        }
      case CoqTypes.Fail(ep) =>
        return doComplete(Some(() => {
          val completed = editor.steps.synchronized {
            editor.steps.headOption match {
              case None => 0
              case Some(step) => step.offset + step.text.length
            }
          }
          editor.setUnderway(completed)
          editor.setCompleted(completed)
        }), new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", ep._2.trim))
    }
    return doComplete(None, Status.OK_STATUS)
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
      doStep(step) match {
        case CoqTypes.Good(s) =>
          editor.steps.synchronized { editor.steps.push(step) }
          monitor.worked(1)
          CoqJob.asyncExec {
            editor.setCompleted(step.offset + step.text.length)
          }
        case CoqTypes.Fail(ep) =>
          return doComplete(partialCCB,
              new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", ep._2.trim))
      }
    }
    return doComplete(None, Status.OK_STATUS)
  }
}