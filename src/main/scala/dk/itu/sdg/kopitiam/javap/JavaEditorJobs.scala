package dk.itu.sdg.kopitiam.javap
import dk.itu.sdg.kopitiam._

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.runtime.{IProgressMonitor, IStatus, SubMonitor}
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends ContainerJobBase("Initialising Java proof mode",
        new JavaProofInitialisationRunner(jes), jes)
class JavaProofInitialisationRunner(
    jes : JavaEditorState) extends JobRunner[Unit] {
  override def doOperation(monitor : SubMonitor) : Unit = {
    monitor.beginTask("Initialising Java proof mode", 4)
    
    jes.coqTop.transaction[Unit](ct => {
      monitor.subTask("Performing custom Coq initialisation")

      import org.eclipse.core.resources.IResource
      val input = jes.editor.getEditorInput
      TryCast[IFileEditorInput](input).map(_.getFile).foreach(f => {
        val cp = ICoqModel.toCoqProject(f.getProject)
        cp.getLoadPath.foreach(lpe => ct.interp(false, false, lpe.asCommand))
      })
      monitor.worked(1)

      monitor.subTask("Preparing model")
      val f = jes.file.get
      val proj = f.getParent
      val basename = f.getName().dropRight(5)
      val model = proj.getFile(new Path(basename + "_model.v"))
      if (!model.exists) {
        UIUtils.exec {
          UIUtils.Dialog.warning("Model file missing",
              "Please write a model file for this Java file named '" +
              basename + "_model'.")
        }
        return
      } else new CompileCoqRunner(model, None).run(monitor.newChild(1))
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
            ct.interp(false, false, s)
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
            ct.interp(false, false, s)
            loopProgress.worked(1)
          }
        case None =>
      }

      val goals = ct.goals match {
        case CoqTypes.Good(a) => a
        case _ => None
      }
      UIUtils.asyncExec {
        jes.setComplete(
            jes.method.flatMap(EclipseJavaASTProperties.getPostcondition))
        jes.setGoals(goals)
      }
      //register handlers!
      jes.activateHandler("Kopitiam.step_forward", new JavaStepForwardHandler)
      jes.activateHandler("Kopitiam.step_all", new JavaStepAllHandler)
      jes.activateHandler("Kopitiam.step_cursor", new JavaStepToCursorHandler)
      jes.activateHandler("Kopitiam.step_backward", new JavaStepBackHandler)
      jes.activateHandler("Kopitiam.retract", new JavaRetractAllHandler)
    }) match {
      case CoqTypes.Fail((_, message)) => fail(
          Activator.makeStatus(IStatus.ERROR, message))
      case _ =>
    }
  }
}

class JavaStepForwardJob(steps : List[JavaStep], jes : JavaEditorState)
    extends ContainerJobBase(
        "Stepping forward", new JavaStepForwardRunner(jes, steps), jes)
class JavaStepForwardRunner(jes : JavaEditorState, steps : List[JavaStep])
    extends StepForwardRunner(jes, steps) {
  override protected def onGood(
      step : JavaStep, result : CoqTypes.Good[String]) = {
    jes.steps.synchronized { jes.steps.push(step) }
    UIUtils.asyncExec { jes.setComplete(Some(step.node)) }
  }
  
  override protected def onFail(
      step : JavaStep, result : CoqTypes.Fail[String]) = {
    import org.eclipse.ui.IFileEditorInput
    UIUtils.asyncExec { jes.setUnderway(jes.complete) }
    val ep = result.value
    import org.eclipse.jdt.core.dom.EmptyStatement
    val range = ep._1 match {
      case Some((start, end)) if step.node.isInstanceOf[EmptyStatement] =>
        (step.node.getStartPosition + 2 + start,
            step.node.getStartPosition + 2 + end)
      case _ =>
        (step.node.getStartPosition,
            step.node.getStartPosition + step.node.getLength)
    }
    jes.file.foreach(file => CreateErrorMarkerJob(file, range, ep._2).schedule)
  }
  
  override def finish = updateGoals.foreach(goals => {
    if (goals.fg_goals.isEmpty && goals.bg_goals.isEmpty)
      jes.coqTop.interp(false, false, "Qed.") match {
        case CoqTypes.Good(s) =>
          val method = jes.method.get
          jes.completedMethods :+= method
          jes.steps.synchronized { jes.steps.clear }

          UIUtils.asyncExec {
            jes.setMethod(None)
            jes.markCompletedMethods

            val c = jes.compilationUnit.get
            if (jes.completedMethods.size == JavaASTUtils.countMethods(c)) {
              if (UIUtils.Dialog.question("Proof completed",
                "All method proofs have been completed.\n\n" +
                  "Save the proof certificate now?")) {
                import org.eclipse.ui.handlers.IHandlerService
                TryService[IHandlerService](UIUtils.getWorkbench).foreach(
                  _.executeCommand("Kopitiam.save_proof_certificate", null))
              }
            }
            ()
          }
        case _ =>
      }
  })
}

class JavaStepBackJob(jes : JavaEditorState, stepCount : Int)
    extends ContainerJobBase(
        "Stepping back", new JavaStepBackRunner(jes, stepCount), jes)
class JavaStepBackRunner(jes : JavaEditorState, stepCount : Int)
    extends StepRunner[String](jes) {
  override protected def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Stepping back", 2)
    
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
