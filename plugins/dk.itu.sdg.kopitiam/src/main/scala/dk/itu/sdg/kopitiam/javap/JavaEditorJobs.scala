package dk.itu.sdg.kopitiam.javap

import dk.itu.coqoon.ui.{
  StepRunner, ContainerJobBase, StepForwardRunner, CreateErrorMarkerJob}
import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.coqtop.CoqTypes
import dk.itu.coqoon.core.utilities.{JobRunner, ObjectRule, TryService}

import dk.itu.sdg.kopitiam._

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.runtime.{IProgressMonitor, IStatus, SubMonitor}
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends ContainerJobBase("Initialising Java proof mode",
        new JavaProofInitialisationRunner(jes), jes) {
  import dk.itu.coqoon.core.utilities.JobUtilities._
  import org.eclipse.core.resources.ResourcesPlugin
  setRule(MultiRule(ObjectRule(jes),
      getRuleFactory.buildRule, ResourcesPlugin.getWorkspace.getRoot))
}
class JavaProofInitialisationRunner(
    jes : JavaEditorState) extends JobRunner[Unit] {
  override def doOperation(monitor : SubMonitor) : Unit = {
    monitor.beginTask("Initialising Java proof mode", 5)

    val file = jes.file.get
    val project = file.getProject
    val description = project.getDescription

    import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}

    monitor.subTask("Configuring Java project")

    /* Configure the project with the Coq nature (if necessary) */
    if (!description.hasNature(CMI.NATURE_COQ)) {
      val r = UIUtils.exec {
        UIUtils.Dialog.question("Coq support missing",
            "Support for Coq must be added to this Java project " +
            "before you can use Kopitiam with it.\n\n" +
            "Add Coq support now?")
      }
      if (r) {
        import org.eclipse.core.resources.IResource

        ICoqProject.configureDescription(description)
        project.setDescription(description, IResource.NONE, null)
      } else return
    }

    /* Add Charge! to the project's load path (if necessary) */
    val cp = ICoqModel.toCoqProject(file.getProject)
    val clp = AbstractLoadPath(ChargeLibrary.ID)
    val clpp = cp.getLoadPathProviders
    if (!cp.getLoadPathProviders.contains(clp))
      cp.setLoadPathProviders(clpp :+ clp, null)

    monitor.worked(1)

    jes.coqTop.transaction[Unit](ct => {
      monitor.subTask("Performing custom Coq initialisation")

      monitor.subTask("Adding project loadpath entries")
      cp.getLoadPath.foreach(
          lpe => lpe.asCommands.foreach(ct.interp(true, false, _)))
      monitor.worked(1)

      monitor.subTask("Preparing model")
      import org.eclipse.core.resources.IncrementalProjectBuilder
      val parent = file.getParent
      val basename = file.getName().dropRight(5)
      val model = parent.getFile(new Path(basename + "_model.v"))
      if (!model.exists) {
        UIUtils.exec {
          UIUtils.Dialog.warning("Model file missing",
              "Please write a model file for this Java file named '" +
              basename + "_model'.")
        }
        return
      } else project.build(
          IncrementalProjectBuilder.INCREMENTAL_BUILD, monitor.newChild(1))
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
        jes.setComplete(None) /* Trigger an annotation redraw */
        jes.setGoals(goals)
      }
      //register handlers!
      jes.activateHandler(
          "dk.itu.coqoon.ui.commands.step_forward", new JavaStepForwardHandler)
      jes.activateHandler(
          "dk.itu.coqoon.ui.commands.step_all", new JavaStepAllHandler)
      jes.activateHandler(
          "dk.itu.coqoon.ui.commands.step_cursor", new JavaStepToCursorHandler)
      jes.activateHandler(
          "dk.itu.coqoon.ui.commands.step_backward", new JavaStepBackHandler)
      jes.activateHandler(
          "dk.itu.coqoon.ui.commands.retract", new JavaRetractAllHandler)
    }) match {
      case CoqTypes.Fail((_, message)) =>
        jes.clearFlag(dk.itu.coqoon.ui.CoqEditor.FLAG_INITIALISED)
        fail(Activator.makeStatus(IStatus.ERROR, message))
      case _ =>
        jes.setFlag(dk.itu.coqoon.ui.CoqEditor.FLAG_INITIALISED)
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
    jes.setComplete(Some(step.end))
  }

  override protected def preCheck = {
    val nextStep = JavaStepForwardHandler.collectProofScript(
        jes, false, jes.complete).headOption
    if (nextStep != steps.headOption) {
      Some(CoqTypes.Fail((None, "(missing step)")))
    } else None
  }

  override protected def onFail(
      step : JavaStep, result : CoqTypes.Fail[String]) = {
    import org.eclipse.ui.IFileEditorInput
    jes.setUnderway(jes.complete)
    val ep = result.value
    import org.eclipse.jdt.core.dom.EmptyStatement
    val range = ep._1 match {
      case Some((start, end)) if step.node.isInstanceOf[EmptyStatement] =>
        (step.start + start, step.start + end)
      case _ =>
        (step.start, step.end)
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
    val rewindCount = steps.count(_.synthetic == false)
    jes.coqTop.rewind(rewindCount) match {
      case CoqTypes.Good(extra) =>
        monitor.worked(1)
        jes.steps.synchronized {
          for (step <- steps)
            jes.steps.pop

          if (extra > 0) {
            var i = 0
            var redoSteps = jes.steps.takeWhile(a => {
              if (i < extra) {
                if (!a.synthetic)
                  i += 1
                true
              } else false
            }).toList.reverse
            for (step <- redoSteps)
              jes.steps.pop
            new JavaStepForwardRunner(
                jes, redoSteps).run(monitor.newChild(1))
          }
        }
        CoqTypes.Good("")
      case CoqTypes.Fail(ep) =>
        val completed = jes.steps.synchronized {
          jes.steps.headOption.map(_.end)
        }
        UIUtils.asyncExec { jes.setUnderway(completed) }
        CoqTypes.Fail(ep)
    }
  }
}
