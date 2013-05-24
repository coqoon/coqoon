/* CommonJobs.scala
 * Base classes, utility objects and helpers for jobs, and some shared jobs
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.runtime.{Status, IStatus, IProgressMonitor}
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

abstract class MarkerJob(
    resource : IResource) extends WorkspaceJob("Update markers") {
  setRule(JobUtilities.getRuleFactory.markerRule(resource))
  setSystem(true) /* Don't show this job in the UI */
}

class DeleteErrorMarkersJob(
    resource : IResource, type_ : String,
    includeSubtypes : Boolean, depth : Int) extends MarkerJob(resource) {
  override def runInWorkspace(monitor : IProgressMonitor) : IStatus = {
    resource.deleteMarkers(type_, includeSubtypes, depth)
    Status.OK_STATUS
  }
}

class CreateErrorMarkerJob(
    resource : IResource, region : (Int, Int), message : String)
    extends MarkerJob(resource) {
  override def runInWorkspace(monitor : IProgressMonitor) : IStatus = {
    val m = resource.createMarker(IMarker.PROBLEM)
    import scala.collection.JavaConversions._
    m.setAttributes(Map(
        (IMarker.MESSAGE, message),
        (IMarker.LOCATION, resource.toString),
        (IMarker.SEVERITY, IMarker.SEVERITY_ERROR),
        (IMarker.CHAR_START, region._1),
        (IMarker.CHAR_END, region._2),
        (IMarker.TRANSIENT, true)))
    Status.OK_STATUS
  }
}
object CreateErrorMarkerJob {
  def apply(
      resource : IResource, step : CoqStep, ep : (CoqTypes.location, String)) : CreateErrorMarkerJob = {
    val offsets = ep._1 match {
      case Some((begin, end)) => (step.offset + begin, step.offset + end)
      case None => (step.offset, step.offset + step.text.length)
    }
    CreateErrorMarkerJob(resource, offsets, ep._2.trim)
  }
  
  def apply(
      resource : IResource, region : (Int, Int), message : String) : CreateErrorMarkerJob =
    new CreateErrorMarkerJob(resource, region, message)
}

import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.SubMonitor

abstract class JobBase(name : String) extends Job(name) {
  protected def runner : JobRunner[_]
  
  override def run(monitor_ : IProgressMonitor) : IStatus =
    runner.run(monitor_)._1
}

abstract class CoqJobBase(name : String) extends JobBase(name) {
  protected def container : CoqTopContainer
  setRule(ObjectRule(container))
  
  override def run(monitor_ : IProgressMonitor) : IStatus =
    try {
      super.run(monitor_)
    } finally container.setBusy(false)
}

trait JobRunner[A] {
  protected def finish(result : A, monitor : SubMonitor) : (IStatus, A)
  protected def doOperation(monitor : SubMonitor) : A
  
  def run(monitor_ : IProgressMonitor) : (IStatus, A) = {
    val monitor = SubMonitor.convert(monitor_)
    try {
      wrapOperation(monitor)
    } finally monitor.done
  }
  
  final protected def wrapOperation(monitor : SubMonitor) : (IStatus, A) =
    finish(doOperation(monitor), monitor)
}

trait SimpleJobRunner extends JobRunner[IStatus] {
  override protected def finish(result : IStatus, monitor : SubMonitor) =
    (result, result)
}

abstract class CoqStepRunner[A](container : CoqTopContainer)
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
  
  override def finish(
      result : CoqTypes.value[A], monitor : SubMonitor) = {
    updateGoals
    (if (!monitor.isCanceled()) {
      CoqStepRunner.valueToStatus(result)
    } else Status.CANCEL_STATUS, result)
  }
}
object CoqStepRunner {
  def valueToStatus(value : CoqTypes.value[_]) = value match {
    case CoqTypes.Good(_) => Status.OK_STATUS
    case CoqTypes.Unsafe(_) => Status.OK_STATUS
    case CoqTypes.Fail(ep) =>
      new Status(IStatus.ERROR, "dk.itu.sdg.kopitiam", ep._2.trim)
  }
}

abstract class CoqStepForwardRunner[A <: CoqCommand](
    container : CoqTopContainer, steps : Seq[A])
    extends CoqStepRunner[String](container) {
  protected def onFail(step : A)
  protected def onGood(step : A)
  protected def onUnsafe(step : A) = onGood(step)
  
  override def doOperation(
      monitor : SubMonitor) : CoqTypes.value[String] = {
    monitor.beginTask("Step forward", steps.length)
    for (step <- steps) {
      if (monitor.isCanceled())
        return CoqTypes.Good("(cancelled)")
      monitor.subTask(step.text.trim)
      step.run(container.coqTop) match {
        case CoqTypes.Good(msg) => onGood(step)
        case CoqTypes.Unsafe(msg) => onUnsafe(step)
        case CoqTypes.Fail(ep) => onFail(step); return CoqTypes.Fail(ep)
      }
      monitor.worked(1)
    }
    CoqTypes.Good("")
  }
}