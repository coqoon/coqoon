/* Configuration.scala
 * Glue to add OcaIDE builder support to Coqoon projects
 * Copyright © 2015 Alexander Faithfull
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

package dk.itu.coqoon.ocaide

import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.ISources
import org.eclipse.core.runtime.{Path, Status, IProgressMonitor}
import org.eclipse.core.commands.{ExecutionEvent, AbstractHandler}
import org.eclipse.core.resources.{
  ICommand, IProject, WorkspaceJob, IProjectDescription}
import org.eclipse.core.expressions.IEvaluationContext
import org.eclipse.jface.viewers.IStructuredSelection
import ocaml.natures.OcamlbuildNature.{ID => OCBNatureID}
import ocaml.build.ocamlbuild.OcamlbuildBuilder.{ID => OCBBuilderID}

class ConfigureHandler extends AbstractHandler {
  import SharedConfigurationUtilities._

  override def execute(ev : ExecutionEvent) = {
    getSelection(ev.getApplicationContext).flatMap(TryCast[IProject]).foreach {
      case p =>
        val d = p.getDescription
        var ns = d.getNatureIds
        if (!ns.exists(isOCBNature)) {
          d.setNatureIds(ns :+ OCBNatureID)
          new UpdateProjectDescription(p, d, install = true).schedule
        }
    }
    null
  }
}

class UnconfigureHandler extends AbstractHandler {
  import SharedConfigurationUtilities._
  override def execute(ev : ExecutionEvent) = {
    getSelection(ev.getApplicationContext).flatMap(TryCast[IProject]).foreach {
      case p =>
        val d = p.getDescription
        d.setBuildSpec(d.getBuildSpec.filterNot(isOCBBuilder))
        d.setNatureIds(d.getNatureIds.filterNot(isOCBNature))
        new UpdateProjectDescription(p, d, install = false).schedule
    }
    null
  }
}

private class UpdateProjectDescription(
    project : IProject, description : IProjectDescription, install : Boolean)
        extends WorkspaceJob(s"Reconfiguring project ${project.getName}") {
  setRule(project.getWorkspace.getRoot)

  override def runInWorkspace(monitor : IProgressMonitor) = {
    project.setDescription(description, monitor)

    val bsHandle = project.getFile("myocamlbuild.ml")
    if (install) {
      try {
        import org.eclipse.core.runtime.FileLocator
        val s = FileLocator.find(Activator.getDefault.getBundle,
            new Path("lib/myocamlbuild.ml"), null).openStream
        if (bsHandle.exists) {
          bsHandle.setContents(s, 0, null)
        } else bsHandle.create(s, 0, null)
      } catch {
        case e : Exception =>
          println(e)
          throw e
      }
    } else bsHandle.delete(0, null)

    Status.OK_STATUS
  }
}

private object SharedConfigurationUtilities {
  def isOCBNature(a : String) = (OCBNatureID == a)
  def isOCBBuilder(a : ICommand) = (OCBBuilderID == a.getBuilderName)

  def getSelection(ec : Object) : Seq[Any] = {
    TryCast[IEvaluationContext](ec) match {
      case Some(e) =>
        TryCast[IStructuredSelection](
            e.getVariable(ISources.ACTIVE_CURRENT_SELECTION_NAME)) match {
          case Some(s) =>
            import scala.collection.JavaConversions.asScalaBuffer
            s.toList
          case None =>
            Seq()
        }
      case _ =>
        Seq()
    }
  }
}