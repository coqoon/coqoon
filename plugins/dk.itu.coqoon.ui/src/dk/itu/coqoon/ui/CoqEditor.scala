/* (c) 2010-2011 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.{UIUtils, SupersedableTask}
import dk.itu.coqoon.core
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.coqtop.CoqTopIdeSlave_v20120710
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

import org.eclipse.ui.IFileEditorInput
import org.eclipse.ui.editors.text.TextEditor

import org.eclipse.jface.text.IDocument

class CoqEditor extends BaseCoqEditor with CoqTopEditorContainer {
  private object ModelListener extends CoqElementChangeListener {
    override def coqElementChanged(ev : CoqElementEvent) = ev match {
      case CoqProjectLoadPathChangedEvent(project)
          if project.getCorrespondingResource == file.map(_.getProject) =>
        clearFlag(CoqEditor.FLAG_INITIALISED)
      case _ =>
    }
  }
  ICoqModel.getInstance.addListener(ModelListener)

  private val lock = new Object

  override def editor = this

  import scala.collection.mutable.Stack
  private var stepsV : Stack[CoqStep] = Stack()
  override def steps = stepsV

  private val annotateTask = new SupersedableTask(50)

  private var underwayV : Int = 0
  def underway = lock synchronized { underwayV }
  def setUnderway(offset : Int) = lock synchronized {
    if (offset < completedV)
      completedV = offset
    underwayV = offset
    annotateTask.schedule {
      UIUtils.asyncExec { addAnnotations(completed, underway) }
    }
  }

  private var completedV : Int = 0
  def completed = lock synchronized { completedV }
  def setCompleted(offset : Int) = lock synchronized {
    completedV = offset
    annotateTask.schedule {
      UIUtils.asyncExec { addAnnotations(completed, underway) }
    }
  }

  private var coqTopV : CoqTopIdeSlave_v20120710 = null
  override def coqTop = {
    if (coqTopV == null)
      coqTopV = CoqTopIdeSlave_v20120710().orNull
    coqTopV
  }

  import org.eclipse.jface.text.reconciler.MonoReconciler
  private val reconciler =
    new MonoReconciler(new CoqProofReconcilingStrategy(this), true)
  reconciler.setDelay(1)

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.IVerticalRuler
  override protected def createSourceViewer(
      parent : Composite, ruler : IVerticalRuler, styles : Int) = {
    val viewer = super.createSourceViewer(parent, ruler, styles)
    reconciler.install(viewer)
    viewer
  }

  override def dispose = {
    if (coqTopV != null) {
      coqTopV.kill
      coqTopV = null
    }
    ICoqModel.getInstance.removeListener(ModelListener)
    reconciler.uninstall
    super.dispose
  }

  final def getViewer = super.getSourceViewer

  override protected def initializeEditor() = {
    super.initializeEditor
    setSourceViewerConfiguration(new CoqSourceViewerConfiguration(this))
  }

  private def addAnnotations (first : Int, second : Int) : Unit =
    doConnectedToAnnotationModel(model =>
        doSplitAnnotations(CoqTopEditorContainer.getSplitAnnotationRanges(
            Some(0), Some(first), Some(second)), model))

  def invalidate () : Unit = UIUtils.asyncExec {
    getSourceViewer.invalidateTextPresentation
  }

  override def initializeKeyBindingScopes =
    setKeyBindingScopes(Array("dk.itu.coqoon.ui.contexts.coq"))

  import org.eclipse.ui.views.contentoutline.IContentOutlinePage
  // Support getting outline pages
  var outlinePage : Option[CoqContentOutlinePage] = None
  private def createOutlinePage() : CoqContentOutlinePage = {
    val page = new CoqContentOutlinePage
    page.setInput(workingCopy.get)
    page
  }

  override def getAdapter(adapter : Class[_]) =
    if (adapter == classOf[IContentOutlinePage]) {
      if (outlinePage == None && getSourceViewer != null)
        outlinePage = Some(createOutlinePage)
      outlinePage.orNull
    } else super.getAdapter(adapter)
}
object CoqEditor {
  final val FLAG_INITIALISED = "CoqEditor.initialised"
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
private class CoqProofReconcilingStrategy(
    editor : CoqEditor) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IRegion, Region}
  import org.eclipse.jface.text.reconciler.DirtyRegion

  import org.eclipse.core.resources.{IMarker,IResource}

  override def reconcile(r : IRegion) : Unit = {
    editor.file.foreach(file => {
      if (file.findMarkers(core.ManifestIdentifiers.MARKER_PROBLEM,
          true, IResource.DEPTH_ZERO).length > 0)
        new DeleteMarkersJob(file, core.ManifestIdentifiers.MARKER_PROBLEM,
            true, IResource.DEPTH_ZERO).schedule
    })

    val off = r.getOffset
    if (off <= editor.underway) {
      if (editor.busy)
        editor.coqTop.interrupt
      if (off < editor.completed ||
          (off == editor.completed && !doc.forall(
               doc => off >= doc.getLength || doc.getChar(off).isWhitespace)))
        /* We can't finish reconciliation until the job is scheduled */
        UIUtils.exec {
          CoqEditorHandler.doStepBack(editor,
              _.prefixLength(a => (off <= (a.offset + a.text.length))), false)
        }
    }
  }

  override def reconcile(dr : DirtyRegion, r : IRegion) = reconcile(dr)

  private var doc : Option[IDocument] = None
  override def setDocument(newDocument : IDocument) =
    doc = Option(newDocument)
}

import org.eclipse.ui.views.contentoutline.ContentOutlinePage

class CoqContentOutlinePage extends ContentOutlinePage {
  import org.eclipse.jface.viewers.{
    TreeViewer, SelectionChangedEvent, IStructuredSelection}
  import org.eclipse.swt.widgets.Composite

  override def selectionChanged(event : SelectionChangedEvent) : Unit = {
    super.selectionChanged(event)

    TryCast[IStructuredSelection](
        event.getSelection).flatMap(sel => Option(sel.getFirstElement)) match {
      case Some(e : ICoqScriptElement) =>
        OpenDeclarationHandler.highlightElement(e)
      case _ =>
    }
  }

  override def createControl(parent : Composite) : Unit = {
    super.createControl(parent)

    val viewer = getTreeViewer()
    viewer.setContentProvider(new ModelContentProvider)
    viewer.setLabelProvider(new ModelLabelProvider)
    viewer.setInput(input.orNull)
  }

  private var input : Option[ICoqElement] = None
  def setInput(input : ICoqElement) = {
    this.input = Option(input)
    Option(getTreeViewer).foreach(_.setInput(input))
  }
}

// Provide a Coq option in the New menu
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.ui.dialogs.WizardNewFileCreationPage
class NewCoqFileWizardPage (selection: IStructuredSelection) extends WizardNewFileCreationPage("NewCoqFileWizardPage", selection) {
  setTitle("Coq File")
  setDescription("Creates a new Coq file")
  setFileExtension("v")
}

import org.eclipse.jface.wizard.Wizard
import org.eclipse.ui.INewWizard
class NewCoqFileWizard extends Wizard with INewWizard {
  import org.eclipse.core.resources.IFile
  import org.eclipse.ui.{IWorkbench, PlatformUI, IWorkbenchPage}
  import org.eclipse.jface.viewers.IStructuredSelection

  setWindowTitle("New Coq File")

  var workbench : Option[IWorkbench] = None
  var selection : Option[IStructuredSelection] = None
  var page : Option[NewCoqFileWizardPage] = None

  def init (wkbnch : IWorkbench, sel : IStructuredSelection) = {
    workbench = Some(wkbnch)
    selection = Some(sel)
  }

  override def addPages () : Unit = {
    page = selection map (new NewCoqFileWizardPage(_))
    page foreach addPage
  }

  override def performFinish () : Boolean = {
    import org.eclipse.ui.PartInitException
    import org.eclipse.ui.ide.IDE

    val file = page map (_.createNewFile)
    for (f <- file ; p <- page) {
      val workbenchPage : IWorkbenchPage = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage
      try {
        IDE.openEditor(workbenchPage, f, true)
      } catch {
        case e : PartInitException =>
      }
    }
    !file.isEmpty
  }
}
