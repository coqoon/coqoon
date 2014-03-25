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
  import org.eclipse.ui.editors.text.EditorsUI
  import org.eclipse.ui.texteditor.ChainedPreferenceStore
  setPreferenceStore(new ChainedPreferenceStore(Array(
      Activator.getDefault.getPreferenceStore,
      EditorsUI.getPreferenceStore)))

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
    updateFolding()
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

  import org.eclipse.ui.texteditor.SourceViewerDecorationSupport
  import org.eclipse.jface.text.source.DefaultCharacterPairMatcher
  override def configureSourceViewerDecorationSupport(
      support : SourceViewerDecorationSupport) = {
    import CoqoonUIPreferences._
    import org.eclipse.jface.text.IDocumentExtension3
    super.configureSourceViewerDecorationSupport(support)
    support.setCharacterPairMatcher(new DefaultCharacterPairMatcher(
        Array('(', ')', '{', '}', '<', '>', '[', ']'),
        IDocumentExtension3.DEFAULT_PARTITIONING, true))
    support.setMatchingCharacterPainterPreferenceKeys(
        MATCHING_BRACKETS, MATCHING_BRACKETS_COLOR)
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

  import dk.itu.coqoon.core.utilities.CacheSlot
  val workingCopy = CacheSlot[IDetachedCoqVernacFile] {
    ICoqModel.getInstance.toCoqElement(file.get).flatMap(
        TryCast[ICoqVernacFile]).get.detach
  }

  override def doSetInput (input : IEditorInput) : Unit = {
    super.doSetInput(input)
    /* XXX: flush outline page */
  }

  var oldAnnotations : Array[Annotation] = Array()
  def updateFolding() : Unit = {
    import dk.itu.coqoon.core.utilities.Substring

    import scala.collection.JavaConversions._
    var positions : Seq[Position] = Seq()
    workingCopy.get.accept(_ match {
      case f : ICoqScriptGroup
          if f.getChildren.size > 1 =>
        val padding = f.getText.takeWhile(_.isWhitespace).length
        if (Substring(f.getText, padding).count(_ == '\n') < 3) {
          false
        } else {
          positions +:=
            new Position(f.getOffset + padding, f.getLength - padding)
          true
        }
      case f : IParent => true
      case _ => false
    })

    val newAnnotations = Map(positions.map(
        p => (new ProjectionAnnotation -> p)) : _*)

    annotationModel.foreach(
        _.modifyAnnotations(oldAnnotations, newAnnotations, null))
    oldAnnotations = newAnnotations.map(_._1).toArray
  }
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

import org.eclipse.jface.text.rules.RuleBasedScanner

object CoqTokenScanner extends RuleBasedScanner {
  import org.eclipse.jface.text.rules.{IToken, Token, WordRule}
  import org.eclipse.jface.text.{IDocument, TextAttribute}
  import org.eclipse.swt.SWT.{BOLD, ITALIC}

  //Console.println("Initializing CoqTokenScanner")

  private val black = UIUtils.Color(0, 0, 0)
  private val white = UIUtils.Color(255, 255, 255)

  private val keywordToken : IToken = new Token(new TextAttribute(UIUtils.Color.fromPreference("coqKeywordFg"), white, BOLD))
  private val definerToken : IToken = new Token(new TextAttribute(UIUtils.Color.fromPreference("coqKeywordFg"), white, BOLD))
  private val opToken : IToken = new Token(new TextAttribute(UIUtils.Color(0, 0, 128), white, 0))
  private val commentToken : IToken = new Token(new TextAttribute(UIUtils.Color(30, 30, 0), white, ITALIC))
  private val stringToken : IToken = new Token(new TextAttribute(UIUtils.Color(0, 0, 255), white, 0))
  private val otherToken : IToken = new Token(new TextAttribute(black, white, 0))

  private val keyword =
    Seq("Axiom", "Conjecture", "Parameter", "Parameters", "Variable",
        "Variables", "Hypothesis", "Hypotheses", "Definition", "Example",
        "Inductive", "CoInductive", "Fixpoint", "CoFixpoint", "Program",
        "Goal", "Let", "Remark", "Fact", "Corollary", "Proposition", "Lemma",
        "Instance", "Theorem", "Tactic", "Ltac", "Notation", "Infix", "Add",
        "Record", "Section", "Module", "Require", "Import", "Export", "Open",
        "Proof", "End", "Qed", "Admitted", "Save", "Defined", "Print", "Eval",
        "Check", "Hint")

  private val operator =
    Seq("!", "%", "&", "&&", "(", "()", ")", "*", "+", "++", ",", "-", "->",
        ".", ".(", "..", "/", "/\\", ":", "::", ":<", ":=", ":>", ";", "<",
        "<-", "<->", "<:", "<=", "<>", "=", "=>", "=_D", ">", ">->", ">=", "?",
        "?=", "@", "[", "\\/", "]", "^", "{", "|", "|-", "||", "}", "~")

  // The reserved words as listed in the reference manual
  private val keywords =
    Seq("_", "as", "at", "cofix", "else", "end", "exists", "exists2", "fix",
        "for", "forall", "fun", "if", "IF", "in", "let", "match", "mod",
        "Prop", "return", "Set", "then", "Type", "using", "where", "with")

  private val wordRule = new FuturisticWordRule(CoqWordDetector, otherToken)
  for (k <- keyword) wordRule.addWord(k, definerToken)
  for (k <- keywords) wordRule.addWord(k, keywordToken)
  private val opRule = new BasicRule
  for (o <- operator) opRule.recognise(o, opToken)

  private val stringRule = new BasicRule
  val s1 = stringRule.getStartState
  val s2 = new BasicRule.State /* in string */
  s1.add('"', s2)

  s2.setFallback(s2)

  val s3 = new BasicRule.State /* escape */
  s2.add('\\', s3)
  s3.setFallback(s2)
  val s4 = new BasicRule.State /* out of string */
  s4.setToken(stringToken)
  s2.add('"', s4)

  private val commentRule = new BasicRule
  val c1 = commentRule.getStartState
  val c2 = c1.require('(').require('*') /* in comment */
  c2.setFallback(c2)
  val c3 = c2.require('*').require(')')
  c2.get('*').foreach(_.setFallback(c2))
  c3.setToken(commentToken)

  setRules(Seq(commentRule, stringRule, wordRule, opRule).toArray)
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
import org.eclipse.jface.text.IDocument
class CoqWorkingCopyReconcilingStrategy(var document : IDocument,
    editor : CoqEditor) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IDocument, IRegion, Position}
  import org.eclipse.jface.text.reconciler._

  override def reconcile(dr : DirtyRegion, r : IRegion) = reconcile(dr)

  override def reconcile(partition : IRegion) = UIUtils.asyncExec {
    editor.workingCopy.get.setContents(document.get)
    editor.updateFolding
    editor.outlinePage.foreach(_.setElement(editor.workingCopy.get))
  }

  override def setDocument(doc : IDocument) = (document = doc)
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
    val strategy = new CoqWorkingCopyReconcilingStrategy(v.getDocument, editor)
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

import org.eclipse.ui.views.contentoutline.ContentOutlinePage

class CoqContentOutlinePage(
    private var element : ICoqElement) extends ContentOutlinePage {
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

    setElement(element)
  }

  def setElement(element : ICoqElement) = {
    this.element = element
    Option(getTreeViewer).foreach(_.setInput(element))
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
