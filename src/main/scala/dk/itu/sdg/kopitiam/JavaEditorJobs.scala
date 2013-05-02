package dk.itu.sdg.kopitiam

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends Job("Initialising Java proof mode") {
  
  override def run(monitor_ : IProgressMonitor) =
    JavaProofInitialisationJob.run(jes, monitor_)
}
object JavaProofInitialisationJob {
  def run(jes : JavaEditorState, monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Initialising Java proof mode", 4)
    try {
      monitor.subTask("Performing custom Coq initialisation")
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      jes.coqTop.interp(false, false, "Add LoadPath \"" + loadp + "\".")

      import org.eclipse.core.resources.IResource

      val input = jes.editor.getEditorInput
      val res: Option[IResource] =
        if (input.isInstanceOf[IFileEditorInput]) {
          Some(input.asInstanceOf[IFileEditorInput].getFile)
        } else None

      res match {
        case Some(r) =>
          jes.coqTop.interp(false, false,
            "Add Rec LoadPath \"" +
              r.getProject.getFolder("src").getLocation.toOSString + "\".")
        case None =>
          Console.println("shouldn't happen - trying to get ProjectDir from " +
            input + ", which is not an IFileEditorInput")
      }
      monitor.worked(1)

      monitor.subTask("Preparing model")
      val fei = jes.editor.getEditorInput().asInstanceOf[IFileEditorInput]
      val proj = fei.getFile.getParent
      val basename = fei.getFile.getName().dropRight(5)
      val model = proj.getFile(new Path(basename + "_model.v"))
      if (!model.exists) {
        EclipseBoilerPlate.warnUser("Model file missing",
          "Please write a model file for this Java file named '" +
            basename + "_model'.")
        return Status.OK_STATUS
      } else {
        val ccj = CoqCompileJob.run(model, monitor.newChild(1))
        if (ccj != Status.OK_STATUS)
          return ccj
      }
      monitor.setWorkRemaining(2)
      
      monitor.subTask("Setting up definitions and specification")
      //send over definition and spec
      jes.compilationUnit match {
        case Some(x) =>
          val pdef = EclipseJavaASTProperties.getDefinition(x).get
          val spec = EclipseJavaASTProperties.getSpecification(x).get
          val steps = pdef ++ spec
          val loopProgress = monitor.newChild(1,
              SubMonitor.SUPPRESS_ALL_LABELS).setWorkRemaining(steps.length)
          for (s <- pdef ++ spec) {
            jes.coqTop.interp(true, false, s)
            loopProgress.worked(1)
          }
        case None =>
      }
      
      monitor.subTask("Setting up method proof environment")
      //send over beginning of proof
      jes.method match {
        case Some(meth) =>
          val prfhead = meth.getProperty(EclipseJavaASTProperties.coqProof).
              asInstanceOf[List[String]]
          val loopProgress = monitor.newChild(1,
              SubMonitor.SUPPRESS_ALL_LABELS).setWorkRemaining(prfhead.length)
          for (s <- prfhead) {
            jes.coqTop.interp(true, false, s)
            loopProgress.worked(1)
          }
        case None =>
      }
      
      val goals = jes.coqTop.goals match {
        case CoqTypes.Good(a) => a
        case _ => None
      }
      CoqJob.asyncExec {
        jes.setGoals(goals)
      }
      //register handlers!
      jes.activateHandler("Kopitiam.step_forward", new JavaStepForwardHandler)
      Status.OK_STATUS
    } finally monitor_.done
  }
}

private object JavaJob {
  def asyncExec(f : => Unit) =
    org.eclipse.ui.PlatformUI.getWorkbench.getDisplay.asyncExec(
        new Runnable() {
      override def run = f
    })
}

class JavaStepJob(step : String, jes : JavaEditorState)
    extends Job("Stepping forward") {
  override def run(monitor_ : IProgressMonitor) =
    JavaStepJob.run(step, jes, monitor_)
}
object JavaStepJob {
  def run(
      step : String, jes : JavaEditorState,
      monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Java step", 1)
    try {
      monitor.subTask(step)
      jes.coqTop.interp(false, false, step) match {
        case CoqTypes.Good(msg) =>
          JavaJob.asyncExec { jes.setComplete(jes.underway) }
          Status.OK_STATUS
        case CoqTypes.Fail(ep) =>
          JavaJob.asyncExec { jes.setUnderway(jes.complete) }
          new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", ep._2.trim)
      }
    } finally {
      jes.coqTop.goals match {
        case CoqTypes.Good(goals) =>
          goals match {
            case Some(goals)
                if goals.fg_goals.isEmpty && goals.bg_goals.isEmpty =>
              jes.coqTop.interp(false, false, "Qed.") match {
                case CoqTypes.Good(s) =>
                  val method = jes.method.get
                  jes.completedMethods :+= method
                case _ =>
              }
              /* Whether we succeeded or not, there's nothing more to do */
              jes.setMethod(None)
              jes.setUnderway(None)
              jes.annotateCompletedMethods
            case _ =>
          }
          JavaJob.asyncExec { jes.setGoals(goals) }
        case _ =>
          JavaJob.asyncExec { jes.setGoals(None) }
      }
      jes.setBusy(false)
      monitor_.done
    }
  }
}