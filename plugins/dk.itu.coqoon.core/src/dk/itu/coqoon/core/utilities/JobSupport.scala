/* JobSupport.scala
 * Classes for managing and executing Eclipse platform jobs
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core.utilities

import dk.itu.coqoon.core.ManifestIdentifiers

import org.eclipse.core.runtime.{
  Status, IStatus, SubMonitor, CoreException, IProgressMonitor}
import org.eclipse.core.runtime.jobs.{Job, ISchedulingRule}

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