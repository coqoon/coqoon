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
import org.eclipse.core.resources.{
  IFile, IProject, IResource, IContainer, IProjectDescription}
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.resources.IProjectNature

class CoqBuilder extends IncrementalProjectBuilder {
  type BuildArgs = java.util.Map[java.lang.String, java.lang.String]
  
  private class DependencyGraph
  private var deps : Option[DependencyGraph] = None
  
  private def partBuild(
      args : BuildArgs, monitor : IProgressMonitor) : Array[IProject] = {
    println("Part build for " + getProject)
    if (deps == None)
      return fullBuild(args, monitor)
    val delta = getDelta(getProject())
    Array()
  }
  
  private def recurse[A <: IResource](folder : IContainer,
      filter : IResource => Option[A], f : A => Unit) : Unit = {
    for (i <- folder.members) {
      filter(i).map(f)
      i match {
        case i : IContainer => recurse(i, filter, f)
        case _ =>
      }
    }
  }
  
  private def vFileFilter(r : IResource) : Option[IFile] = 
    if (r.isInstanceOf[IFile]) {
      Some(r.asInstanceOf[IFile]).filter(_.getFileExtension == "v")
    } else None
  
  private def fullBuild(
      args : BuildArgs, monitor : IProgressMonitor) : Array[IProject] = {
    println("Full build for " + getProject)
    val dg = new DependencyGraph
    recurse[IFile](getProject, vFileFilter, a => println(a))
    deps = Some(dg)
    Array()
  }
  
  override def build(kind : Int,
      args : BuildArgs, monitor : IProgressMonitor) : Array[IProject] = {
    println(this + ".build(" + kind + ", " + args + ", " + monitor + ")")
    val delta = getDelta(getProject())
    kind match {
      case IncrementalProjectBuilder.AUTO_BUILD if delta != null =>
        partBuild(args, monitor)
      case IncrementalProjectBuilder.INCREMENTAL_BUILD if delta != null =>
        partBuild(args, monitor)
      case _ =>
        fullBuild(args, monitor)
    }
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

import org.eclipse.jface.viewers.{
  LabelProvider, ILabelProviderListener, ITreeContentProvider, Viewer}

private class LoadPathLabelProvider extends LabelProvider

private class LoadPathContentProvider extends ITreeContentProvider {
  override def dispose = ()
  
  override def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) =
    viewer.refresh
  
  override def getElements(input : Any) = input match {
    case a : ICoqProject => a.getLoadPath.toArray
    case _ => Array()
  }
  
  override def getChildren(parent : Any) = parent match {
    case a : ICoqLoadPath => Array((a.coqdir, a.path))
    case _ => Array()
  }
  
  override def getParent(child : Any) = null
  override def hasChildren(parent : Any) = true
}

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.jface.preference.PreferencePage
import org.eclipse.jface.viewers.TreeViewer

class LoadPathConfigurationPage
    extends PreferencePage with IWorkbenchPropertyPage {
  private var element : IAdaptable = null
  override def getElement : IProject = element.asInstanceOf[IProject]
  override def setElement(element : IAdaptable) = (this.element = element)
  override def createContents(c : Composite) = {
    val tf = new TabFolder(c, SWT.NONE)
    
    val t1 = new TabItem(tf, SWT.NONE)
    t1.setText("Source")
    val c1 = new Composite(tf, SWT.NONE)
    t1.setControl(c1)
    c1.setLayout(new FillLayout(SWT.HORIZONTAL))
    
    val c1l = new Composite(c1, SWT.NONE)
    c1l.setLayout(new FillLayout(SWT.VERTICAL))
    
    val tv = new TreeViewer(c1l)
    tv.setLabelProvider(new LoadPathLabelProvider)
    tv.setContentProvider(new LoadPathContentProvider)
    tv.setInput(ICoqModel.create(
        getElement.getWorkspace.getRoot).getProject(getElement.getName()))
    
    val c1r = new Composite(c1, SWT.NONE)
    c1r.setLayout(new FillLayout(SWT.VERTICAL))
    
    new Button(c1r, SWT.NONE).setText("I'm a button!")
    new Button(c1r, SWT.NONE).setText("I'm also a button!")
    
    val t2 = new TabItem(tf, SWT.NONE)
    t2.setText("Projects")
    
    val t3 = new TabItem(tf, SWT.NONE)
    t3.setText("Libraries")
    
    tf.pack
    tf
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
  final val NATURE_ID = "dk.itu.sdg.kopitiam.CoqNature"
}