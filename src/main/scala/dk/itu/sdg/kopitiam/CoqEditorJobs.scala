package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}

abstract class CoqJob(
    name : String,
    editor : Editor) extends org.eclipse.core.runtime.jobs.Job(name) {
  /* Two CoqJobs operating on the same Editor should conflict */
  setRule(ObjectRule(editor))
  /* Make sure that this editor's coqtop instance has been initialised */
  editor.coqTop
}

class InitialiseCoqJob(editor : Editor)
    extends CoqJob("Initialise Coq", editor) {
  override def run(monitor : IProgressMonitor) =
    InitialiseCoqJob.run(editor, monitor)
}
object InitialiseCoqJob {
  def run(editor : Editor, monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(monitor_, "Initialising Coq", 2)
    try {
      monitor.subTask("Adding global loadpath entries")
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      editor.coqTop.interp(false, false, "Add LoadPath \"" + loadp + "\".")
      monitor.worked(1)
      
      monitor.subTask("Adding project loadpath entries")
      
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
              "Add Rec LoadPath \"" +
              r.getProject.getFolder("src").getLocation.toOSString + "\".")
        case None =>
          Console.println("shouldn't happen - trying to get ProjectDir from " +
              input + ", which is not an IFileEditorInput")
      }

      monitor.worked(1)
      
      Status.OK_STATUS
    } finally monitor_.done
  }
}

class RestartCoqJob(editor : Editor) extends CoqJob("Restarting Coq", editor) {
  override def run(monitor : IProgressMonitor) =
    RestartCoqJob.run(editor, monitor)
}
object RestartCoqJob {
  def run(editor : Editor, monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(monitor_, "Restarting Coq", 3)
    try {
      monitor.subTask("Clearing state")
      editor.steps.synchronized { editor.steps.clear }
      UIUtils.asyncExec {
        editor.setGoals(None)
        editor.setUnderway(0)
      }
      monitor.worked(1)
      
      monitor.subTask("Restarting Coq")
      editor.coqTop.restart
      monitor.worked(1)
      
      monitor.subTask("Re-initialising Coq")
      InitialiseCoqJob.run(editor, monitor.newChild(1))
      
      Status.OK_STATUS
    } finally monitor_.done
  }
}

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource

abstract class StepJob(
    title : String,
    editor : Editor) extends CoqJob(title, editor) {
  protected val partialCCB = Some(() => editor.setUnderway(editor.completed))
  
  protected def failCB(step : CoqStep, ep : (CoqTypes.location, String)) =
    () => {
      partialCCB.map { _() }
      val file = editor.getEditorInput.asInstanceOf[IFileEditorInput].getFile()
      CreateErrorMarkerJob(file, step, ep).schedule()
    }
  
  protected def doCancel = doComplete(partialCCB, Status.CANCEL_STATUS)
  
  protected def doComplete(
      f : Option[() => Unit], status : IStatus) : IStatus = {
    val goals = editor.coqTop.goals match {
      case CoqTypes.Good(g) => g
      case _ => None
    }
    UIUtils.asyncExec {
      f.map { _() }
      editor.setGoals(goals)
    }
    editor.setBusy(false)
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
    for (step <- steps) {
      if (monitor.isCanceled())
        return doCancel
      monitor.subTask(step.text.trim)
      step.run(editor.coqTop) match {
        case CoqTypes.Good(s) =>
          editor.steps.synchronized { editor.steps.push(step) }
          monitor.worked(1)
          UIUtils.asyncExec {
            editor.setCompleted(step.offset + step.text.length)
          }
        case CoqTypes.Fail(err) =>
          return doComplete(Some(failCB(step, err)), Status.OK_STATUS)
      }
    }
    return doComplete(None, Status.OK_STATUS)
  }
}
