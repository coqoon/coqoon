package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}

class InitialiseCoqJob(editor : Editor) extends JobBase("Initialise Coq") {
  setRule(ObjectRule(editor))
  override def runner = new InitialiseCoqRunner(editor)
}
class InitialiseCoqRunner(editor : Editor) extends SimpleJobRunner {
  override def doOperation(monitor : SubMonitor) : IStatus = {
    monitor.beginTask("Initialising Coq", 2)
    
    monitor.subTask("Adding global loadpath entries")
    val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
    editor.coqTop.interp(false, false, "Add Rec LoadPath \"" + loadp + "\".")
    monitor.worked(1)

    monitor.subTask("Adding project loadpath entries")

    import org.eclipse.ui.IFileEditorInput
    import org.eclipse.core.resources.IResource

    val input = editor.getEditorInput
    val res: Option[IResource] =
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
  }
}

abstract class CoqEditorJob(
    name : String, editor : Editor) extends CoqJobBase(name, editor) {
  /* Make sure that this editor's coqtop instance has been initialised */
  editor.coqTop
}

class RestartCoqJob(editor : Editor)
    extends CoqEditorJob("Restarting Coq", editor) {
  override def runner = new RestartCoqRunner(editor)
}
class RestartCoqRunner(editor : Editor) extends SimpleJobRunner {
  override protected def doOperation(monitor : SubMonitor) : IStatus = {
    monitor.beginTask("Restarting Coq", 3)
    
    monitor.subTask("Clearing state")
    editor.steps.synchronized { editor.steps.clear }
    UIUtils.asyncExec {
      editor.setGoals(None)
      editor.setUnderway(0)
    }
    monitor.worked(1)

    monitor.subTask("Stopping Coq")
    editor.coqTop.kill
    monitor.worked(1)

    monitor.subTask("Starting Coq")
    new InitialiseCoqRunner(editor).run(monitor.newChild(1))

    Status.OK_STATUS
  }
}

class StepBackJob(
    editor : Editor,
    stepCount : Int) extends CoqEditorJob("Step back", editor) {
  override def runner = new StepBackRunner(editor, stepCount)
}

class StepBackRunner(editor : Editor, stepCount : Int)
    extends CoqStepRunner[String](editor) {
  override protected def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Step back", 2)
    
    val steps = editor.steps.synchronized { editor.steps.take(stepCount) }
    val rewindCount = steps.count(_.synthetic == false)
    editor.coqTop.rewind(rewindCount) match {
      case CoqTypes.Good(extra) =>
        monitor.worked(1)
        editor.steps.synchronized {
          for (step <- steps)
            editor.steps.pop
          
          if (extra > 0) {
            // XXX: synthetic steps
            var redoSteps = editor.steps.take(extra).toList.reverse
            for (step <- redoSteps)
              editor.steps.pop
            new StepForwardRunner(editor, redoSteps).doOperation(monitor.newChild(1))
          }
        }
        CoqTypes.Good("")
      case CoqTypes.Fail(ep) =>
        val completed = editor.steps.synchronized {
          editor.steps.headOption match {
            case None => 0
            case Some(step) => step.offset + step.text.length
          }
        }
        UIUtils.asyncExec { editor.setUnderway(completed) }
        CoqTypes.Fail(ep)
    }
  }
}

class StepForwardJob(
    editor : Editor,
    steps : List[CoqStep]) extends CoqEditorJob("Step forward", editor) {
  override def runner = new StepForwardRunner(editor, steps)
}

class StepForwardRunner(
    editor : Editor,
    steps : List[CoqStep]) extends CoqStepForwardRunner(editor, steps) {
  override protected def onGood(step : CoqStep) = {
    editor.steps.synchronized { editor.steps.push(step) }
    UIUtils.asyncExec { editor.setCompleted(step.offset + step.text.length) }
  }
  
  override protected def onFail(step : CoqStep) =
    UIUtils.asyncExec { editor.setUnderway(editor.completed) }
}
