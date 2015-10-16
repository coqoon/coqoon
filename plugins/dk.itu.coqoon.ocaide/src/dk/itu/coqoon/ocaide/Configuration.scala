package dk.itu.coqoon.ocaide

import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.ISources
import org.eclipse.core.runtime.{Status, IProgressMonitor}
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
          new UpdateProjectDescription(p, d).schedule
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
        new UpdateProjectDescription(p, d).schedule
    }
    null
  }
}

private class UpdateProjectDescription(
    project : IProject, description : IProjectDescription)
        extends WorkspaceJob(s"Reconfiguring project ${project.getName}") {
  setRule(project.getWorkspace.getRoot)

  override def runInWorkspace(monitor : IProgressMonitor) = {
    project.setDescription(description, monitor)
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