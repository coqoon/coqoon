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

class CoqEditor extends TextEditor with CoqTopEditorContainer {
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

  import org.eclipse.jface.text.source.{Annotation, ISourceViewer, IVerticalRuler}
  import org.eclipse.jface.text.{IDocument, Position}
  import org.eclipse.swt.widgets.Composite
  import org.eclipse.ui.IEditorInput
  import org.eclipse.ui.views.contentoutline.{ContentOutlinePage, IContentOutlinePage}
  import org.eclipse.jface.text.source.projection.{ProjectionAnnotation, ProjectionAnnotationModel}

  var annotationModel : Option[ProjectionAnnotationModel] = None

  override protected def initializeEditor () : Unit = {
    //Console.println("initializeEditor was called")
    setDocumentProvider(CoqDocumentProvider)
    //Console.println(" - document provider set")
    setSourceViewerConfiguration(new CoqSourceViewerConfiguration(this))
    //Console.println(" - source viewer configuration set")
    super.initializeEditor()
    //Console.println(" - initializeEditor called super")
  }

  private var parent : Composite = null
  override def createPartControl (par : Composite) : Unit = {
    import org.eclipse.jface.text.source.projection.{ProjectionSupport, ProjectionViewer}
    super.createPartControl(par)

    parent = par
    //Create the necessary infrastructure for code folding
    val projViewer : ProjectionViewer = getSourceViewer.asInstanceOf[ProjectionViewer]
    val projectionSupport = new ProjectionSupport(projViewer, getAnnotationAccess(), getSharedColors())
    projectionSupport.install()

    //turn projection mode on
    projViewer.doOperation(ProjectionViewer.TOGGLE)

    annotationModel = Option(projViewer.getProjectionAnnotationModel)

    val doc = this.getDocumentProvider.getDocument(getEditorInput)
    file.flatMap(ICoqModel.getInstance.toCoqElement).flatMap(
        TryCast[ICoqVernacFile]).foreach(updateFolding(_, doc))
  }

  //Create the source viewer as one that supports folding
  override def createSourceViewer (parent : Composite, ruler : IVerticalRuler, styles : Int) : ISourceViewer = {
    import org.eclipse.jface.text.source.projection.ProjectionViewer
    val viewer : ISourceViewer = new ProjectionViewer(
        parent, ruler, getOverviewRuler(), isOverviewRulerVisible(), styles)
    getSourceViewerDecorationSupport(viewer)
    reconciler.install(viewer)
    viewer
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

  // Support getting outline pages
  var outlinePage : Option[CoqContentOutlinePage] = None
  override def getAdapter (required : java.lang.Class[_]) : AnyRef = {
    //Console.println("Getting adapter for " + required + " on CoqEditor")
    if (required == classOf[IContentOutlinePage]) {
      outlinePage = outlinePage.orElse(
          file.flatMap(ICoqModel.getInstance.toCoqElement).flatMap(
              TryCast[ICoqVernacFile]).map(new CoqContentOutlinePage(_)))
      outlinePage.get
    }
    else super.getAdapter(required)
  }

  override def doSetInput (input : IEditorInput) : Unit = {
    super.doSetInput(input)
    /* XXX: flush outline page */
  }

  //Necessary for Eclipse API cruft
  var oldAnnotations : Array[Annotation] = Array.empty

  def updateFolding (root : ICoqVernacFile, document : IDocument) : Unit = ()
}
object CoqEditor {
  final val FLAG_INITIALISED = "CoqEditor.initialised"
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
private class CoqProofReconcilingStrategy(
    editor : CoqEditor) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IRegion, Region, IDocument}
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
    if (off < editor.underway) {
      if (editor.busy)
        editor.coqTop.interrupt
      if (off < editor.completed)
        /* We can't finish reconciliation until the job is scheduled */
        UIUtils.exec {
          CoqEditorHandler.doStepBack(editor,
              _.prefixLength(a => (off < (a.offset + a.text.length))), false)
        }
    }
  }

  override def reconcile(dr : DirtyRegion, r : IRegion) = reconcile(dr)

  override def setDocument(newDocument : IDocument) = ()
}

import org.eclipse.ui.editors.text.TextFileDocumentProvider

object CoqDocumentProvider extends TextFileDocumentProvider {
  import org.eclipse.jface.text.IDocument

  override def getDefaultEncoding () : String = "UTF-8"
}

import org.eclipse.jface.text.rules.IWordDetector

object CoqWordDetector extends IWordDetector {
  // TODO: Look up in spec...
  def isWordStart(character : Char) =
    (character >= 'a' && character <= 'z') ||
    (character >= 'A' && character <= 'Z') ||
    (character >= '0' && character <= '9')

  def isWordPart(character : Char) = isWordStart(character)
}

import org.eclipse.jface.text.rules.RuleBasedScanner

object CoqTokenScanner extends RuleBasedScanner {
  import org.eclipse.jface.text.rules.{IToken, MultiLineRule, SingleLineRule, Token, WordRule}
  import org.eclipse.jface.text.{IDocument, TextAttribute}
  import org.eclipse.swt.SWT.{BOLD, ITALIC}

  //Console.println("Initializing CoqTokenScanner")

  private val black = UIUtils.Color(0, 0, 0)
  private val white = UIUtils.Color(255, 255, 255)

  private val keywordToken : IToken = new Token(new TextAttribute(UIUtils.Color.fromPreference("coqKeywordFg"), white, 0))
  private val definerToken : IToken = new Token(new TextAttribute(UIUtils.Color.fromPreference("coqKeywordFg"), white, 0))
  private val opToken : IToken = new Token(new TextAttribute(UIUtils.Color(0, 0, 30), white, 0))
  private val commentToken : IToken = new Token(new TextAttribute(UIUtils.Color(30, 30, 0), white, ITALIC))
  private val otherToken : IToken = new Token(new TextAttribute(black, white, 0))

  private val rules = Seq(
    new MultiLineRule("(*", "*)", commentToken),
    new SingleLineRule("(*", "*)", commentToken)
  )

  import coqparser.VernacularReserved._
  private val wordRule = new WordRule(CoqWordDetector, otherToken)
  for (k <- keyword) wordRule.addWord(k, definerToken)
  for (k <- keywords) wordRule.addWord(k, keywordToken)
  for (o <- operator) wordRule.addWord(o, opToken)

  setRules((rules ++ Seq(wordRule)).toArray)
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
import org.eclipse.jface.text.IDocument
class CoqOutlineReconcilingStrategy(var document : IDocument, editor : CoqEditor) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IDocument, IRegion, Position}
  import org.eclipse.jface.text.reconciler._
  import org.eclipse.ui.views.contentoutline.IContentOutlinePage

  def reconcile (dr : DirtyRegion, r : IRegion) : Unit = reconcile(dr)

  def reconcile (partition : IRegion) : Unit = ()

  def setDocument(doc : IDocument) : Unit = {
    document = doc
  }
}

import org.eclipse.ui.editors.text.TextSourceViewerConfiguration

class CoqSourceViewerConfiguration(editor : CoqEditor) extends TextSourceViewerConfiguration {
  import org.eclipse.jface.text.presentation.{IPresentationReconciler, PresentationReconciler}
  import org.eclipse.jface.text.rules.DefaultDamagerRepairer
  import org.eclipse.jface.text.{TextAttribute,IDocument}
  import org.eclipse.jface.text.reconciler.{IReconciler, MonoReconciler}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.jface.text.source.ISourceViewer
  import org.eclipse.jface.text.contentassist.{IContentAssistant,ContentAssistant}

  override def getAutoEditStrategies(v : ISourceViewer, ct : String) =
    Array(new CoqAutoEditStrategy)

  override def getContentAssistant(v : ISourceViewer) : IContentAssistant = {
    val assistant= new ContentAssistant
    val assistantProcessor = new CoqContentAssistantProcessor(editor)
    assistant.setContentAssistProcessor(assistantProcessor, IDocument.DEFAULT_CONTENT_TYPE)
    assistant
  }

  override def getReconciler (v : ISourceViewer) : IReconciler = {
    val strategy = new CoqOutlineReconcilingStrategy(v.getDocument, editor)
    val reconciler = new MonoReconciler(strategy, false)
    reconciler
  }

  override def getPresentationReconciler (v : ISourceViewer) : IPresentationReconciler = {
    val pr = new PresentationReconciler
    //println("About to create damager/repairer")
    val ddr = new DefaultDamagerRepairer(CoqTokenScanner)
    //println("Created damager/repairer successfully")
    pr.setDamager(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr.setRepairer(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr
  }

  import org.eclipse.jface.text.quickassist.QuickAssistAssistant

  override def getQuickAssistAssistant(v : ISourceViewer) = {
    val qa = new QuickAssistAssistant
    qa.setQuickAssistProcessor(new CoqQuickAssistProcessor)
    qa
  }
}

import org.eclipse.jface.text.contentassist.{
  ICompletionProposal, IContentAssistProcessor}

class CoqContentAssistantProcessor(
    val editor : CoqEditor) extends IContentAssistProcessor {
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
    	editor.coqTop.search(List(
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

import org.eclipse.jface.text.quickassist.IQuickAssistProcessor

class CoqQuickAssistProcessor extends IQuickAssistProcessor {
  import org.eclipse.jface.text.source.Annotation
  import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext

  override def canAssist(context : IQuickAssistInvocationContext) = false
  override def canFix(i : Annotation) = false
  override def computeQuickAssistProposals(
      context : IQuickAssistInvocationContext) : Array[ICompletionProposal] = {
    Array()
  }
  override def getErrorMessage = null
}

import org.eclipse.ui.views.contentoutline.ContentOutlinePage

class CoqContentOutlinePage(element : ICoqElement) extends ContentOutlinePage {
  import org.eclipse.jface.viewers.{
    TreeViewer, SelectionChangedEvent, IStructuredSelection}
  import org.eclipse.swt.widgets.Composite

  override def selectionChanged(event : SelectionChangedEvent) : Unit = {
    super.selectionChanged(event)

    TryCast[IStructuredSelection](
        event.getSelection).flatMap(sel => Option(sel.getFirstElement)) match {
      case Some(s : ICoqScriptSentence) =>
        OpenDeclarationHandler.highlightSentence(s)
      case _ =>
    }
  }

  override def createControl(parent : Composite) : Unit = {
    super.createControl(parent)

    val viewer = getTreeViewer()
    viewer.setContentProvider(new ModelContentProvider)
    viewer.setLabelProvider(new ModelLabelProvider)
    viewer.setInput(element)
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
