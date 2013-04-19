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
  private var workbench : IWorkbench = null
  private var selection : IStructuredSelection = null
  override def init(w : IWorkbench, s : IStructuredSelection) = {
    workbench = w
    selection = s
  }
  
  private val creationPage = new NewCoqProjectCreationPage()
  
  override def addPages = {
    addPage(creationPage)
  }
  
  class ProjectCreator(private val project : ICoqProject)
      extends IRunnableWithProgress {
    import org.eclipse.core.runtime.SubMonitor
    import org.eclipse.core.resources.{IWorkspace, IWorkspaceRunnable}
    import org.eclipse.ui.ide.undo.WorkspaceUndoUtil
    
    private class DerivedRunnable(r : IResource) extends IWorkspaceRunnable {
      override def run(monitor : IProgressMonitor) = {
        r.setHidden(true)
        r.setDerived(true, monitor)
      }
    }
    
    override def run(monitor_ : IProgressMonitor) = {
      val monitor = SubMonitor.convert(monitor_, 4)
      
      monitor.beginTask("New Coq project", 4)
      import PathUtilities.Implicits._
      val infoAdapter = WorkspaceUndoUtil.getUIInfoAdapter(getShell)
      project.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val src = project.getPackageFragmentRoot("src")
      src.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val bin = project.getPackageFragmentRoot("bin")
      bin.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      
      val binFolder = bin.getCorrespondingResource.get
      val ws = binFolder.getWorkspace
      ws.run(new DerivedRunnable(binFolder),
          ws.getRuleFactory.derivedRule(binFolder),
          IWorkspace.AVOID_UPDATE, monitor.newChild(1))
    }
  }
  
  def createProject : IProject = {
    val project = creationPage.getProjectHandle()
    if (!project.exists()) {
      val mm = ICoqModel.create(project.getWorkspace.getRoot)
      getContainer().run(
          true, true, new ProjectCreator(mm.getProject(project.getName)))
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

class CoqNature extends IProjectNature {
  import CoqNature._
  import org.eclipse.core.resources.ICommand
  
  private var project : IProject = null
  
  override def setProject(project : IProject) = {
    this.project = project
  }
  
  override def getProject = project
  
  override def configure = {
    project.setDescription(
        ICoqProject.configureDescription(project.getDescription), null)
  }
  
  override def deconfigure = {
    project.setDescription(
        ICoqProject.deconfigureDescription(project.getDescription), null)
  }
}
object CoqNature {
  final val NATURE_ID = "dk.itu.sdg.kopitiam.CoqProject"
}