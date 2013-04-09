/* Project.scala
 * Supporting code and wizards for Coq projects and resources
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.resources.IResourceDelta
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.resources.{IProject, IResource, IProjectDescription}
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.resources.IProjectNature

class CoqBuilder extends IncrementalProjectBuilder {
  type BuildArgs = java.util.Map[java.lang.String, java.lang.String]
  
  override def build(kind : Int,
      args : BuildArgs, monitor : IProgressMonitor) : Array[IProject] = {
    println(this + ".build(" + kind + ", " + args + ", " + monitor + ")")
    println("\tdelta is " + getDelta(getProject()))
    null
  }
  
  override def toString = "(CoqBuilder for " + getProject + ")"
}

object CoqBuilder {
  final val BUILDER_ID = "dk.itu.sdg.kopitiam.CoqBuilder"
}

import org.eclipse.ui.INewWizard
import org.eclipse.jface.wizard.Wizard
class NewCoqProjectWizard extends Wizard with INewWizard {
  import org.eclipse.ui.dialogs.WizardNewProjectCreationPage
  
  class NewCoqProjectCreationPage
      extends WizardNewProjectCreationPage("Coq project") {
    
  }
  
  import org.eclipse.ui.IWorkbench
  import org.eclipse.jface.viewers.IStructuredSelection
  override def init(w : IWorkbench, s : IStructuredSelection) = ()
  
  private val creationPage = new NewCoqProjectCreationPage()
  
  override def addPages = {
    addPage(creationPage)
  }
  
  class ProjectCreator(private val description : IProjectDescription)
      extends IRunnableWithProgress {
    import org.eclipse.core.commands.ExecutionException
    import org.eclipse.ui.ide.undo.{CreateProjectOperation, WorkspaceUndoUtil}
    override def run(monitor : IProgressMonitor) = {
      new CreateProjectOperation(description, "Coq project").
          execute(monitor, WorkspaceUndoUtil.getUIInfoAdapter(getShell()))
    }
  }
  
  def createProject : IProject = {
    val project = creationPage.getProjectHandle()
    if (!project.exists()) {
      val description =
          project.getWorkspace.newProjectDescription(project.getName())
      import org.eclipse.core.resources.ICommand
      description.setNatureIds(Array(CoqProject.NATURE_ID))
      description.setBuildSpec(Array(
          CoqProject.makeBuilderCommand(description)))
      getContainer().run(true, true, new ProjectCreator(description));
    }
    return project
  }
  
  import org.eclipse.ui.PlatformUI
  
  override def performFinish = {
    val project = createProject
    PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(
        project, creationPage.getSelectedWorkingSets())
    true
  }
}

class CoqProject extends IProjectNature {
  import CoqProject._
  import org.eclipse.core.resources.ICommand
  
  private var project : IProject = null
  
  override def setProject(project : IProject) = {
    this.project = project
  }
  
  override def getProject = project
  
  private def coqBuilderP(a : ICommand) =
    (CoqBuilder.BUILDER_ID == a.getBuilderName)
  
  override def configure = {
    val d = project.getDescription()
    val bs = d.getBuildSpec
    if (!bs.exists(coqBuilderP)) {
      d.setBuildSpec(bs :+ makeBuilderCommand(d))
      project.setDescription(d, null)
    }
  }
  
  override def deconfigure = {
    val d = project.getDescription()
    val bs = d.getBuildSpec
    if (bs.exists(coqBuilderP)) {
      d.setBuildSpec(bs.filterNot(coqBuilderP))
      project.setDescription(d, null)
    }
  }
}
object CoqProject {
  final val NATURE_ID = "dk.itu.sdg.kopitiam.CoqProject"
  
  def makeBuilderCommand(d : IProjectDescription) = {
    val c = d.newCommand()
    c.setBuilderName(CoqBuilder.BUILDER_ID)
    c
  }
}
