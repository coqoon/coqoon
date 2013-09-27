/* CommonJobs.scala
 * Base classes, utility objects and helpers for jobs, and some shared jobs
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.{
  Status, IStatus, CoreException, IProgressMonitor}
import org.eclipse.core.resources.{IMarker, IResource}
import org.eclipse.core.runtime.jobs.ISchedulingRule

case class ObjectRule(obj : AnyRef) extends ISchedulingRule {
  override def contains(i : ISchedulingRule) = (this == i)
  override def isConflicting(i : ISchedulingRule) = (this == i)
}

object JobUtilities {
  import org.eclipse.core.resources.ResourcesPlugin
  def getRuleFactory = ResourcesPlugin.getWorkspace().getRuleFactory()
  
  object MultiRule {
    def apply(a : ISchedulingRule*) =
      org.eclipse.core.runtime.jobs.MultiRule.combine(a.toArray)
  }
}

import org.eclipse.core.resources.WorkspaceJob

abstract class ResourceJob(name : String, resource : IResource,
    ruleProvider : IResource => ISchedulingRule) extends WorkspaceJob(name) {
  setRule(ruleProvider(resource))
  setSystem(true) /* Don't show this job in the UI */
  
  protected def doOperation(monitor : IProgressMonitor)
  
  override def runInWorkspace(monitor : IProgressMonitor) =
    JobBase.wrap { doOperation(monitor) }
}

import org.eclipse.ui.IMarkerResolution

object ConfigureCoqPathResolution extends IMarkerResolution {
  override def getLabel = "Configure the path to Coq"

  override def run(r : IMarker) = {
    import org.eclipse.ui.dialogs.PreferencesUtil
    PreferencesUtil.createPreferenceDialogOn(UIUtils.getActiveShell,
      "Kopitiam.settings", null, null).open
  }
}

import org.eclipse.ui.{IMarkerResolutionGenerator}
class ResolutionGenerator extends IMarkerResolutionGenerator {
  override def getResolutions(m : IMarker) = Array(ConfigureCoqPathResolution)
}

abstract class MarkerJob(resource : IResource) extends ResourceJob(
    "Update markers", resource, JobUtilities.getRuleFactory.markerRule)

class DeleteMarkersJob(
    resource : IResource, type_ : String,
    includeSubtypes : Boolean, depth : Int) extends MarkerJob(resource) {
  override protected def doOperation(monitor : IProgressMonitor) = 
    resource.deleteMarkers(type_, includeSubtypes, depth)
}

class CreateMarkerJob(
    resource : IResource, region : (Int, Int), message : String,
    type_ : String, severity : Int) extends MarkerJob(resource) {
  import scala.collection.JavaConversions._
  override protected def doOperation(monitor : IProgressMonitor) =
    resource.createMarker(type_).setAttributes(Map(
        (IMarker.MESSAGE, message.replaceAll("\\s+", " ").trim),
        (IMarker.SEVERITY, severity),
        (IMarker.CHAR_START, region._1),
        (IMarker.CHAR_END, region._2),
        (IMarker.TRANSIENT, true)))
}

class CreateErrorMarkerJob(
    resource : IResource, region : (Int, Int), message : String)
    extends CreateMarkerJob(resource, region,
        message, ManifestIdentifiers.MARKER_PROBLEM, IMarker.SEVERITY_ERROR)
object CreateErrorMarkerJob {
  def apply(
      resource : IResource, step : CoqStep, ep : (CoqTypes.location, String)) : CreateErrorMarkerJob = {
    val offsets = ep._1 match {
      case Some((begin, end)) => (step.offset + begin, step.offset + end)
      case None => (step.offset, step.offset + step.text.length)
    }
    CreateErrorMarkerJob(resource, offsets, ep._2)
  }
  
  def apply(
      resource : IResource, region : (Int, Int), message : String) : CreateErrorMarkerJob =
    new CreateErrorMarkerJob(resource, region, message)
}

import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.SubMonitor

abstract class JobBase(
    name : String, runner : JobRunner[_]) extends Job(name) {
  override def run(monitor_ : IProgressMonitor) : IStatus =
    JobBase.wrap { runner.run(monitor_) }
}
object JobBase {
  def wrap(f : => Any) : IStatus = try {
    f
    Status.OK_STATUS
  } catch {
    case e : CoreException =>
      Activator.makeStatus(e.getStatus.getSeverity, "Execution failed.", e)
  }
}

abstract class ContainerJobBase(name : String, runner : JobRunner[_],
    container : CoqTopContainer) extends JobBase(name, runner) {
  setRule(ObjectRule(container))
  
  override def run(monitor_ : IProgressMonitor) : IStatus =
    try {
      super.run(monitor_)
    } finally container.setBusy(false)
}

trait JobRunner[A] {
  protected def preCheck : Option[A] = None
  protected def finish : Unit = ()
  protected def doOperation(monitor : SubMonitor) : A
  
  def run(monitor_ : IProgressMonitor) : A = {
    preCheck.foreach(a => return a)
    val monitor = SubMonitor.convert(monitor_)
    try {
      doOperation(monitor)
    } finally {
      try {
        finish
      } catch {
        case t : Throwable => /* do nothing */
      }
      monitor.done
    }
  }
  
  protected def fail(status : IStatus) = throw new CoreException(status)
  protected def cancel = fail(Status.CANCEL_STATUS)
}

abstract class StepRunner[A](container : CoqTopContainer)
    extends JobRunner[CoqTypes.value[A]] {
  protected def updateGoals = {
    val goals = container.coqTop.goals match {
      case CoqTypes.Good(g) => g
      case _ => None
    }
    UIUtils.asyncExec {
      container.setGoals(goals)
    }
    goals
  }
  
  override protected def finish = updateGoals
}

abstract class StepForwardRunner[A <: CoqCommand](
    container : CoqTopContainer, steps : Seq[A])
    extends StepRunner[String](container) {
  protected def onFail(step : A, result : CoqTypes.Fail[String])
  protected def onGood(step : A, result : CoqTypes.Good[String])
  protected def onUnsafe(step : A, result : CoqTypes.Unsafe[String]) =
    onGood(step, CoqTypes.Good[String](result.value))
  protected def initialise = ()
  
  private def perhapsPrint(
      s : org.eclipse.ui.console.MessageConsoleStream, msg_ : String) = {
    if (msg_ != null) {
      val msg = msg_.trim
      if (msg.length != 0)
        s.println(msg)
    }
  }
      
  override def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Stepping forward", steps.length)
    initialise
    for (step <- steps) {
      if (monitor.isCanceled)
        cancel
      monitor.subTask(step.text.trim)
      step.run(container.coqTop) match {
        case r @ CoqTypes.Good(msg) =>
          perhapsPrint(EclipseConsole.out, msg)
          onGood(step, r)
        case r @ CoqTypes.Unsafe(msg) =>
          perhapsPrint(EclipseConsole.out, msg)
          onUnsafe(step, r)
        case r : CoqTypes.Fail[String] =>
          perhapsPrint(EclipseConsole.err, r.value._2)
          onFail(step, r)
          return r
      }
      monitor.worked(1)
    }
    CoqTypes.Good("")
  }
}

import CoqTypes.{option_name, option_value}

class SetCoqOptionJob(name : option_name, value : option_value,
    container : CoqTopContainer) extends ContainerJobBase("Set " + name,
        new SetCoqOptionRunner(name, value, container), container)
class SetCoqOptionRunner(name : option_name, value : option_value,
    container : CoqTopContainer) extends StepRunner[Unit](container) {
  override def finish = {
    super.finish
    UIUtils.asyncExec {
      UIUtils.refreshElements(ManifestIdentifiers.COMMAND_TOGGLE_COQ_FLAG)
    }
  }

  override protected def doOperation(
      monitor : SubMonitor) : CoqTypes.value[Unit] =
    container.coqTop.set_options(List((name, value)))
}