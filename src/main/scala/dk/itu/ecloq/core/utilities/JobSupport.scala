package dk.itu.ecloq.core.utilities

import dk.itu.ecloq.core.ManifestIdentifiers

import org.eclipse.core.runtime.{
  Status, IStatus, SubMonitor, CoreException, IProgressMonitor}
import org.eclipse.core.runtime.jobs.ISchedulingRule

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

import org.eclipse.core.runtime.jobs.Job

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
    case e : CoreException => new Status(e.getStatus.getSeverity,
        ManifestIdentifiers.PLUGIN, "Execution failed.", e)
  }
}

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