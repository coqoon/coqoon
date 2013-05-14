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
      UIUtils.asyncExec {
        jes.setGoals(goals)
      }
      //register handlers!
      jes.activateHandler("Kopitiam.step_forward", new JavaStepForwardHandler)
      jes.activateHandler("Kopitiam.step_all", new JavaStepAllHandler)
      jes.activateHandler("Kopitiam.step_cursor", new JavaStepToCursorHandler)
      jes.activateHandler("Kopitiam.step_backward", new JavaStepBackHandler)
      jes.activateHandler("Kopitiam.retract", new JavaRetractAllHandler)
      Status.OK_STATUS
    } finally monitor_.done
  }
}

class JavaStepForwardJob(steps : List[JavaStep], jes : JavaEditorState)
    extends Job("Stepping forward") {
  override def run(monitor_ : IProgressMonitor) : IStatus = {
    try {
      JavaStepForwardJob.run(steps, jes, monitor_)
    } finally jes.setBusy(false)
  }
}
object JavaStepForwardJob {
  private def doSteps(
      steps : List[JavaStep], jes : JavaEditorState,
      monitor : IProgressMonitor) : IStatus = {
    for (step <- steps) {
      if (monitor.isCanceled())
        return Status.CANCEL_STATUS
      monitor.subTask(step.text)
      jes.coqTop.interp(false, false, step.text) match {
        case CoqTypes.Good(msg) =>
          jes.steps.synchronized { jes.steps.push(step) }
          UIUtils.asyncExec { jes.setComplete(Some(step.node)) }
        case CoqTypes.Unsafe(msg) =>
          jes.steps.synchronized { jes.steps.push(step) }
          UIUtils.asyncExec { jes.setComplete(Some(step.node)) }
        case CoqTypes.Fail(ep) =>
          UIUtils.asyncExec { jes.setUnderway(jes.complete) }
          return new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", ep._2.trim)
      }
      monitor.worked(1)
    }
    Status.OK_STATUS
  }
  def run(
      steps : List[JavaStep], jes : JavaEditorState,
      monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Java step", steps.size)
    try {
      doSteps(steps, jes, monitor)
    } finally {
      jes.coqTop.goals match {
        case CoqTypes.Good(goals) =>
          goals match {
            case Some(goals)
                if goals.fg_goals.isEmpty && goals.bg_goals.isEmpty =>
              monitor.subTask("Finishing proof")
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
          UIUtils.asyncExec { jes.setGoals(goals) }
        case _ =>
          UIUtils.asyncExec { jes.setGoals(None) }
      }
      monitor_.done
    }
  }
}

class JavaStepBackJob(jes : JavaEditorState, stepCount : Int)
    extends Job("Stepping forward") {
  override def run(monitor_ : IProgressMonitor) : IStatus = {
    try {
      JavaStepBackJob.run(jes, stepCount, monitor_)
    } finally jes.setBusy(false)
  }
}
object JavaStepBackJob {
  def run(
      jes : JavaEditorState, stepCount : Int,
      monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Java step back", 1)
    try {
      Status.OK_STATUS
    } finally monitor.done
  }
}