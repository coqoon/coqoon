package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}

class InitialiseCoqRunner(editor : CoqEditor) extends JobRunner[Unit] {
  override def doOperation(monitor : SubMonitor) = {
    monitor.beginTask("Initialising Coq", 2)
    
    editor.coqTop.transaction[Unit](ct => {
      monitor.subTask("Adding global loadpath entries")
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      if (loadp != null && loadp.length > 0)
        ct.interp(false, false, "Add Rec LoadPath \"" + loadp + "\".")
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
          ct.interp(false, false, "Add Rec LoadPath \"" +
              r.getProject.getFolder("src").getLocation.toOSString + "\".")
        case None =>
          Console.println("shouldn't happen - trying to get ProjectDir from " +
            input + ", which is not an IFileEditorInput")
      }

      monitor.worked(1)
    }) match {
      case CoqTypes.Fail((_, message)) =>
        editor.clearFlag(CoqEditor.FLAG_INITIALISED)
        fail(new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", message))
      case _ =>
        editor.setFlag(CoqEditor.FLAG_INITIALISED)
    }
  }
}

abstract class CoqEditorJob(name : String, runner : JobRunner[_],
    editor : CoqEditor) extends ContainerJobBase(name, runner, editor)

class RestartCoqJob(editor : CoqEditor) extends CoqEditorJob(
    "Restarting Coq", new RestartCoqRunner(editor), editor)
class RestartCoqRunner(editor : CoqEditor) extends JobRunner[Unit] {
  override protected def doOperation(monitor : SubMonitor) = {
    monitor.beginTask("Restarting Coq", 3)
    
    monitor.subTask("Clearing state")
    editor.steps.synchronized { editor.steps.clear }
    editor.setUnderway(0)
    UIUtils.asyncExec { editor.setGoals(None) }
    monitor.worked(1)

    monitor.subTask("Stopping Coq")
    editor.coqTop.kill
    monitor.worked(1)

    monitor.subTask("Starting Coq")
    new InitialiseCoqRunner(editor).run(monitor.newChild(1))
  }
}

class CoqStepBackJob(editor : CoqEditor, stepCount : Int) extends CoqEditorJob(
    "Stepping back", new CoqStepBackRunner(editor, stepCount), editor)
class CoqStepBackRunner(editor : CoqEditor, stepCount : Int)
    extends StepRunner[String](editor) {
  override protected def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Stepping back", 2)
    
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
            new CoqStepForwardRunner(editor, redoSteps).doOperation(monitor.newChild(1))
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
        editor.setUnderway(completed)
        CoqTypes.Fail(ep)
    }
  }
}

class CoqStepForwardJob(editor : CoqEditor, steps : List[CoqStep])
    extends CoqEditorJob("Step forward",
        new CoqStepForwardRunner(editor, steps), editor)
class CoqStepForwardRunner(
    editor : CoqEditor,
    steps : List[CoqStep]) extends StepForwardRunner(editor, steps) {
  override protected def finish = {
    editor.setUnderway(editor.completed)
    super.finish
  }
  
  override protected def onGood(
      step : CoqStep, result : CoqTypes.Good[String]) = {
    editor.steps.synchronized { editor.steps.push(step) }
    editor.setCompleted(step.offset + step.text.length)
  }
  
  override protected def onFail(
      step : CoqStep, result : CoqTypes.Fail[String]) = {
    import org.eclipse.ui.IFileEditorInput
    CreateErrorMarkerJob(
        editor.getEditorInput.asInstanceOf[IFileEditorInput].getFile,
        step, result.value).schedule
  }
  
  override protected def initialise = {
    super.initialise
    if (!editor.testFlag(CoqEditor.FLAG_INITIALISED))
      try {
        new InitialiseCoqRunner(editor).run(null)
      } catch {
        case t : Throwable =>
          UIUtils.asyncExec {
            UIUtils.Dialog.question("Initialisation failed",
                "Coq initialisation failed " +
                "(\"" + t.getMessage.trim + "\").\n\n" +
                "Open the Coq preferences dialog now?") match {
              case true =>
                import org.eclipse.ui.dialogs.PreferencesUtil
                PreferencesUtil.createPreferenceDialogOn(
                    UIUtils.getActiveShell,
                    "Kopitiam.settings", null, null).open
              case false =>
            }
            ()
          }
          fail(Status.OK_STATUS)
      }
  }
}
