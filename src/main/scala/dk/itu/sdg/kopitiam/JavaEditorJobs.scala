package dk.itu.sdg.kopitiam

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends CoqJobBase("Initialising Java proof mode", jes) {
  override def runner = new JavaProofInitialisationRunner(jes)
}
class JavaProofInitialisationRunner(
    jes : JavaEditorState) extends SimpleJobRunner {
  override def doOperation(monitor : SubMonitor) : IStatus = {
    monitor.beginTask("Initialising Java proof mode", 4)
    monitor.subTask("Performing custom Coq initialisation")
    val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
    jes.coqTop.interp(false, false, "Add Rec LoadPath \"" + loadp + "\".")

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
      val ccj = new CoqCompileRunner(model).run(monitor.newChild(1))._1
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
        for (s <- steps) {
          jes.coqTop.interp(false, false, s)
          loopProgress.worked(1)
        }
      case None =>
    }

    monitor.subTask("Setting up method proof environment")
    //send over beginning of proof
    jes.method match {
      case Some(meth) =>
        val prfhead = EclipseJavaASTProperties.getProof(meth).get
        val loopProgress = monitor.newChild(1,
          SubMonitor.SUPPRESS_ALL_LABELS).setWorkRemaining(prfhead.length)
        for (s <- prfhead) {
          jes.coqTop.interp(false, false, s)
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
  }
}

class JavaStepForwardJob(steps : List[JavaStep], jes : JavaEditorState)
    extends CoqJobBase("Stepping forward", jes) {
  override def runner = new JavaStepForwardRunner(jes, steps)
}
class JavaStepForwardRunner(jes : JavaEditorState, steps : List[JavaStep])
    extends CoqStepForwardRunner(jes, steps) {
  override protected def onGood(step : JavaStep) = {
    jes.steps.synchronized { jes.steps.push(step) }
    UIUtils.asyncExec { jes.setComplete(Some(step.node)) }
  }
  
  override protected def onFail(step : JavaStep) =
    UIUtils.asyncExec { jes.setUnderway(jes.complete) }
  
  override def finish(
      result : CoqTypes.value[String], monitor : SubMonitor) = {
    updateGoals match {
      case Some(goals) if goals.fg_goals.isEmpty && goals.bg_goals.isEmpty =>
        monitor.subTask("Finishing proof")
        jes.coqTop.interp(false, false, "Qed.") match {
          case CoqTypes.Good(s) =>
            val method = jes.method.get
            jes.completedMethods :+= method
            jes.steps.synchronized { jes.steps.clear }
            
            val c = jes.compilationUnit.get
            if (jes.completedMethods.size == JavaASTUtils.countMethods(c)) {
              println(
                  "*** Program is correct, proof certificate follows:\n" +
                  jes.createCertificate)
            }
            
            UIUtils.asyncExec {
              jes.setMethod(None)
              jes.annotateCompletedMethods
            }
          case _ =>
        }
      case _ =>
    }
    (CoqStepRunner.valueToStatus(result), result)
  }
}

class JavaStepBackJob(jes : JavaEditorState, stepCount : Int)
    extends CoqJobBase("Stepping forward", jes) {
  override def runner = new JavaStepBackRunner(jes, stepCount)
}
class JavaStepBackRunner(jes : JavaEditorState, stepCount : Int)
    extends CoqStepRunner[String](jes) {
  override protected def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Java step back", 2)
    
    val steps = jes.steps.synchronized { jes.steps.take(stepCount) }
    val rewindCount = steps.size /* XXX: synthetic steps? */
    jes.coqTop.rewind(rewindCount) match {
      case CoqTypes.Good(extra) =>
        monitor.worked(1)
        jes.steps.synchronized {
          for (step <- steps)
            jes.steps.pop
          
          if (extra > 0) {
            // XXX: synthetic steps
            var redoSteps = jes.steps.take(extra).toList.reverse
            for (step <- redoSteps)
              jes.steps.pop
            new JavaStepForwardRunner(jes, redoSteps).run(monitor.newChild(1))
          }
        }
        CoqTypes.Good("")
      case CoqTypes.Fail(ep) =>
        val completed = jes.steps.synchronized {
          jes.steps.headOption match {
            case None => None
            case Some(step) => Some(step.node)
          }
        }
        UIUtils.asyncExec { jes.setUnderway(completed) }
        CoqTypes.Fail(ep)
    }
  }
}