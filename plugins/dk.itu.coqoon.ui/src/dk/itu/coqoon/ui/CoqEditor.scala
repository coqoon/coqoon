/* (c) 2010-2011 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.{UIUtils, SupersedableTask}
import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.coqtop.CoqTopIdeSlave_v20120710
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

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

  override protected def createSourceViewerConfiguration =
    new CoqSourceViewerConfiguration(this)

  private def addAnnotations (first : Int, second : Int) : Unit =
    doConnectedToAnnotationModel(model =>
        doSplitAnnotations(CoqTopEditorContainer.getSplitAnnotationRanges(
            Some(0), Some(first), Some(second)), model))

  def invalidate () : Unit = UIUtils.asyncExec {
    getSourceViewer.invalidateTextPresentation
  }
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
      if (file.findMarkers(CMI.MARKER_PROBLEM,
          true, IResource.DEPTH_ZERO).length > 0)
        new DeleteMarkersJob(file, CMI.MARKER_PROBLEM,
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

class CoqSourceViewerConfiguration(
    editor : CoqEditor) extends BaseCoqSourceViewerConfiguration(editor) {
  import org.eclipse.jface.text.source.ISourceViewer
  import org.eclipse.jface.text.contentassist.{
    IContentAssistant,ContentAssistant}
  override def getContentAssistant(v : ISourceViewer) : IContentAssistant = {
    val assistant = new ContentAssistant
    assistant.setDocumentPartitioning(CoqPartitions.ID)
    val assistantProcessor = new CoqContentAssistantProcessor(editor)
    assistant.setContentAssistProcessor(
        assistantProcessor, CoqPartitions.Types.COQ)
    assistant
  }
}

import org.eclipse.jface.text.contentassist.{
  ICompletionProposal, IContentAssistProcessor}

class CoqContentAssistantProcessor(
    container : CoqTopContainer) extends IContentAssistProcessor {
  import org.eclipse.jface.text.contentassist.{IContextInformationValidator,IContextInformation,CompletionProposal,ContextInformation}
  import org.eclipse.jface.text.ITextViewer
  import java.text.MessageFormat
  import scala.collection.mutable.HashMap

  private val staticCompletions = Array("Admitted","apply","assumption","compute","Defined","destruct","Fixpoint","induction","intros","inversion","Lemma","reflexivity","rewrite","simpl","Theorem","unfold")

  def getPrefix (doc : IDocument, offset : Int) : String = {
    val prefix = new StringBuffer
    if (doc != null && doc.getLength > 0) {
      var index = offset - 1
      while (index >= 0 && Character.isWhitespace(doc.getChar(index)) == false) {
        prefix.insert(0, doc.getChar(index))
        index -= 1
      }
    }
    prefix.toString
  }

  def getCompletionProposal (completion : String, moreinfo : String, prefix : String, offset : Int) : ICompletionProposal = {
    val info = new ContextInformation(completion, MessageFormat.format("CompletionProcessor.Proposal.ContextInfo.pattern"))
    val more = if (moreinfo == null) completion else completion + " : " + moreinfo
    new CompletionProposal(completion, offset - prefix.length, prefix.length, completion.length(), null, more, info, MessageFormat.format("CompletionProcessor.Proposal.hoverinfo.pattern"))
  }

  def computeCompletionProposals (viewer : ITextViewer, documentOffset : Int) : Array[ICompletionProposal] = {
    val prefix = getPrefix(viewer.getDocument, documentOffset)

    import dk.itu.coqoon.core.coqtop.CoqTypes._

    val results =
      if (prefix.length > 1) {
        container.coqTop.search(List(
            (Name_Pattern("^" + prefix), true))) match {
          case Good(results) =>
            results.map(a => {
              (a.coq_object_qualid.mkString("."),
                  a.coq_object_object.replaceAll("\\s+", " "))
            })
          case _ => List()
        }
      } else List()

    val tst : String => Boolean = prefix.length == 0 || _.startsWith(prefix)

    val filteredStatic = staticCompletions.filter(tst)
    val proposals = new Array[ICompletionProposal](filteredStatic.size + results.size)
    val mid = filteredStatic.length
    Range(0, mid).map(x => proposals(x) = getCompletionProposal(filteredStatic(x), null, prefix, documentOffset))
    Range(mid, proposals.length).map(x => {
      val pr = results(x - mid)
      proposals(x) = getCompletionProposal(pr._1, pr._2, prefix, documentOffset)
    })
    proposals
  }

  def computeContextInformation (viewer : ITextViewer, offset : Int) : Array[IContextInformation] = null
  def getCompletionProposalAutoActivationCharacters () : Array[Char] = Array('.')
  def getContextInformationAutoActivationCharacters () : Array[Char] = null
  def getContextInformationValidator () : IContextInformationValidator = null
  def getErrorMessage () : String = "not yet implemented"
}
