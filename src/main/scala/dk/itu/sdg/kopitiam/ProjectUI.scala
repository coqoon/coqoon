/* ProjectUI.scala
 * User interfaces for Coq project creation and manipulation
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.resources.{IFile, IProject, IResource}
import org.eclipse.core.runtime.IProgressMonitor

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
      import org.eclipse.core.runtime.Path
      val infoAdapter = WorkspaceUndoUtil.getUIInfoAdapter(getShell)
      project.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val src = project.getPackageFragmentRoot(new Path("src"))
      src.getCreateOperation.execute(monitor.newChild(1), infoAdapter)
      val bin = project.getPackageFragmentRoot(new Path("bin"))
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

private class LoadPathContentProvider(
    filter : ICoqLoadPathProvider => Boolean) extends ITreeContentProvider {
  override def dispose = ()
  
  override def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) =
    viewer.refresh
  
  override def getElements(input : Any) = input match {
    case a : ICoqProject => a.getLoadPathProviders.filter(filter).toArray
    case _ => Array()
  }
  
  override def getChildren(parent : Any) = parent match {
    case a : ICoqLoadPathProvider => a.getLoadPath.toArray
    case _ => Array()
  }
  
  override def getParent(child : Any) = null
  override def hasChildren(parent : Any) =
    (TryCast[ICoqLoadPathProvider](parent) != None)
}

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{
  Text, Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.jface.preference.PreferencePage
import org.eclipse.jface.viewers.TreeViewer

class LoadPathConfigurationPage
    extends PreferencePage with IWorkbenchPropertyPage {
  private var element : IAdaptable = null
  override def getElement : IProject = TryCast[IProject](element).get
  override def setElement(element : IAdaptable) = (this.element = element)
  override def createContents(c : Composite) = {
    import org.eclipse.swt.layout._
    import org.eclipse.jface.layout._

    val folder = new TabFolder(c, SWT.NONE)

    val t1 = new TabItem(folder, SWT.NONE)
    t1.setText("Source")

    val c1 = new Composite(folder, SWT.NONE)
    t1.setControl(c1)
    c1.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv1 = new TreeViewer(c1)
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[SourceLoadPath]))
    tv1.setInput(ICoqModel.toCoqProject(getElement))
    tv1.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c1r = new Composite(c1, SWT.NONE)
    c1r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c1r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    new Button(c1r, SWT.NONE).setText("I'm a button!")
    new Button(c1r, SWT.NONE).setText("I'm also a button!")

    val oll = new Label(c1, SWT.NONE)
    oll.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).span(2, 1).create)
    oll.setText("Default output location:")

    val olt = new Text(c1, SWT.BORDER)
    olt.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    val olb = new Button(c1, SWT.NONE)
    olb.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)
    olb.setText("Browse...")

    val t2 = new TabItem(folder, SWT.NONE)
    t2.setText("Projects")

    val c2 = new Composite(folder, SWT.NONE)
    t2.setControl(c2)
    c2.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv2 = new TreeViewer(c2)
    tv2.setLabelProvider(new LoadPathLabelProvider)
    tv2.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[ProjectLoadPath]))
    tv2.setInput(ICoqModel.toCoqProject(getElement))
    tv2.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c2r = new Composite(c2, SWT.NONE)
    c2r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c2r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    new Button(c2r, SWT.NONE).setText("I'm a button!")
    new Button(c2r, SWT.NONE).setText("I'm also a button!")

    val t3 = new TabItem(folder, SWT.NONE)
    t3.setText("Libraries")

    val c3 = new Composite(folder, SWT.NONE)
    t3.setControl(c3)
    c3.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv3 = new TreeViewer(c3)
    tv3.setLabelProvider(new LoadPathLabelProvider)
    tv3.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[ExternalLoadPath]))
    tv3.setInput(ICoqModel.toCoqProject(getElement))
    tv3.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c3r = new Composite(c3, SWT.NONE)
    c3r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c3r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    new Button(c3r, SWT.NONE).setText("I'm a button!")
    new Button(c3r, SWT.NONE).setText("I'm also a button!")

    folder.pack
    folder
  }
}

import org.eclipse.core.commands.{ExecutionEvent, AbstractHandler}
class ExportCoqMakefileHandler extends AbstractHandler {
  private var project : Option[IProject] = None
  
  import org.eclipse.ui.ISources
  import org.eclipse.core.expressions.IEvaluationContext
  import org.eclipse.jface.viewers.IStructuredSelection
  override protected def setEnabled(evaluationContext_ : Any) : Unit = {
    val evaluationContext = TryCast[IEvaluationContext](evaluationContext_)
    val selection = TryCast[IStructuredSelection](evaluationContext match {
      case Some(a) => a.getVariable(ISources.ACTIVE_CURRENT_SELECTION_NAME)
      case _ => UIUtils.getWorkbench.
          getActiveWorkbenchWindow.getSelectionService.getSelection
    })
    selection match {
      case Some(a : IStructuredSelection) =>
        project = TryCast[IProject](a.getFirstElement)
        project match {
          case Some(a) =>
            setBaseEnabled(true)
            return
          case _ =>
        }
      case _ =>
    }
    setBaseEnabled(false)
  }
  
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      new ExportCoqMakefileJob(project.get).schedule
    null
  }
}

class ExportCoqMakefileJob(project : IProject) extends JobBase(
    "Export Coq project Makefile", new ExportCoqMakefileRunner(project)) {
  setRule(project)
}
class ExportCoqMakefileRunner(project : IProject) extends JobRunner[Unit] {
  import org.eclipse.core.runtime.SubMonitor
  override def doOperation(monitor : SubMonitor) = {
    monitor.beginTask("Export Coq project Makefile", 1)
    import java.io.ByteArrayInputStream
    val contents = new ByteArrayInputStream(
        CoqBuilder.generateMakefile(project).getBytes)
    project.getFile("KopitiamMakefile") match {
      case f : IFile if f.exists =>
        f.setContents(contents, IResource.NONE, monitor)
      case f : IFile =>
        f.create(contents, IResource.NONE, monitor)
    }
  }
}