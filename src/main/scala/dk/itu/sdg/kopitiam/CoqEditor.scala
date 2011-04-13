/* (c) 2010-2011 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.editors.text.TextEditor

class CoqEditor extends TextEditor with EclipseUtils {
  import dk.itu.sdg.coqparser.VernacularRegion
  import org.eclipse.jface.text.source.{Annotation, IAnnotationModel, ISourceViewer, IVerticalRuler}
  import org.eclipse.jface.text.{IDocument, Position}
  import org.eclipse.swt.widgets.Composite
  import org.eclipse.ui.IEditorInput
  import org.eclipse.ui.views.contentoutline.{ContentOutlinePage, IContentOutlinePage}
  import org.eclipse.jface.text.source.projection.{ProjectionAnnotation, ProjectionAnnotationModel}

  var annotationModel : ProjectionAnnotationModel = null

  override protected def initializeEditor () : Unit = {
    Console.println("initializeEditor was called")
    setDocumentProvider(CoqJavaDocumentProvider)
    Console.println(" - document provider set")
    setSourceViewerConfiguration(new CoqSourceViewerConfiguration(this))
    Console.println(" - source viewer configuration set")
    super.initializeEditor()
    Console.println(" - initializeEditor called super")
  }

  override def createPartControl (parent : Composite) : Unit = {
    import org.eclipse.jface.text.source.projection.{ProjectionSupport, ProjectionViewer}
    super.createPartControl(parent)

    //Create the necessary infrastructure for code folding
    val projViewer : ProjectionViewer = getSourceViewer.asInstanceOf[ProjectionViewer]
    val projectionSupport = new ProjectionSupport(projViewer, getAnnotationAccess(), getSharedColors())
    projectionSupport.install()

    //turn projection mode on
    projViewer.doOperation(ProjectionViewer.TOGGLE)

    annotationModel = projViewer.getProjectionAnnotationModel()
  }

  //Create the source viewer as one that supports folding
  override def createSourceViewer (parent : Composite, ruler : IVerticalRuler, styles : Int) : ISourceViewer = {
    import org.eclipse.jface.text.source.projection.ProjectionViewer
    val viewer : ISourceViewer = new ProjectionViewer(parent, ruler, getOverviewRuler(), isOverviewRulerVisible(), styles)
    getSourceViewerDecorationSupport(viewer)
    viewer
  }

  def getSource () : ISourceViewer = {
    getSourceViewer()
  }

  override def initializeKeyBindingScopes () : Unit = {
    setKeyBindingScopes(List("Kopitiam.context").toArray)
  }

  // Support getting outline pages
  var outlinePage : Option[CoqContentOutlinePage] = None
  override def getAdapter (required : java.lang.Class[_]) : AnyRef = {
    //Console.println("Getting adapter for " + required + " on CoqEditor")
    if (required.isInterface && required.getName.endsWith("IContentOutlinePage")) {
      outlinePage = outlinePage match {
        case p@Some(page) => p
        case None => {
          val page = new CoqContentOutlinePage(getDocumentProvider(), this)
          if (getEditorInput() != null)
            page.setInput(getEditorInput())
          Some(page)
        }
      }
      outlinePage.get
    }
    else super.getAdapter(required)
  }

  override def doSetInput (input : IEditorInput) : Unit = {
    super.doSetInput(input)
    if (input != null && outlinePage != null) {
      outlinePage foreach { _.setInput(input) }
    }
  }

  override def editorSaved () : Unit = {
    if (outlinePage != null) outlinePage foreach { _.update() }
    super.editorSaved()
  }

  //Necessary for Eclipse API cruft
  var oldAnnotations : Array[Annotation] = Array.empty

  def updateFolding (root : VernacularRegion, document : IDocument) : Unit = {
    val annotations = foldingAnnotations(root, document)
    var newAnnotations = new java.util.HashMap[Any, Any]

    for ((pos, annot) <- annotations) newAnnotations.put(annot, pos)
    annotationModel.modifyAnnotations(oldAnnotations, newAnnotations, null)
    oldAnnotations = annotations.map(_._2).toArray
    Console.println("Updated folding " + annotations.toList)
  }

  private def foldingAnnotations(region : VernacularRegion, document : IDocument) : Stream[(Position, ProjectionAnnotation)] = {
    println("Collapsable " + region.outlineName + region.outlineNameExtra)
    if (region.pos.hasPosition && document.get(region.pos.offset, region.pos.length).count(_=='\n') >= 2)
      (pos2eclipsePos(region.pos), new ProjectionAnnotation) #:: region.getOutline.flatMap(foldingAnnotations(_, document))
    else
     region.getOutline.flatMap(foldingAnnotations(_, document))
  }
}

import org.eclipse.jface.viewers.ITreeContentProvider
protected class CoqContentProvider extends ITreeContentProvider {
  import dk.itu.sdg.coqparser.VernacularRegion
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jface.viewers.Viewer
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.ui.texteditor.IDocumentProvider

  var documentProvider : Option[IDocumentProvider] = None

  def dispose() : Unit = {}

  var content : List[VernacularRegion] = Nil
  val parser = new dk.itu.sdg.coqparser.VernacularParser {}
  var root : parser.VernacularDocument = null
  def parse(document : IDocument) : Unit = {
    require(document != null)
    import scala.util.parsing.input.CharSequenceReader

    println("parse the coq")
    val result = parser.parseString(document.get) match {
      case parser.Success(doc, _) => root = doc; doc
      case r : parser.NoSuccess => println("Could not parse: " + r)
    }
    // Remove beginning whitespace from parse result positions
    if (root != null) killLeadingWhitespace(root, document.get)
    println("got " + result)
  }

  def killLeadingWhitespace(reg : VernacularRegion, doc : String) : Unit = {
    import dk.itu.sdg.parsing._
    def advance(reg : VernacularRegion) : Unit = {
      reg.pos match {
        case NoLengthPosition => ()
        case RegionPosition(off, len) if len > 0 => {
          if ((doc(off) == ' ' || doc(off) == '\t' || doc(off) == '\n' || doc(off) == '\r') && reg.advancePosStart) advance(reg)
          else if (doc.substring(off, off+2) == "(*") advanceComment(reg, 1)
        }
        case _ => ()
      }
    }
    def advanceComment(reg : VernacularRegion, depth : Int) : Unit = {
      reg.pos match {
        case NoLengthPosition => ()
        case RegionPosition(off, len) if len > 0=> {
          println("Advancing " + doc.substring(off, off+len) + " at " + off)
          if (doc.substring(off, off+2) == "(*") {
            reg.advancePosStart(2)
            advanceComment(reg, depth + 1)
          }
          else if (doc.substring(off, off+2) == "*)") {
            reg.advancePosStart(2)
            if (depth > 1) advanceComment(reg, depth - 1)
            else advance(reg)
          }
          else {
            reg.advancePosStart
            advanceComment(reg, depth)
          }
        }
        case _ => ()
      }
    }
    advance(reg)
    reg.subRegions foreach { r => killLeadingWhitespace(r, doc) }
  }

  def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) : Unit = {
    if (newInput != null) {
      println("inputChanged")
      documentProvider foreach {prov => parse(prov.getDocument(newInput))}
    }
  }

  def hasChildren (obj : Any) : Boolean = obj match {
    case something : VernacularRegion if something.getOutline.length > 0 => true
    case _ => false
  }

  def getChildren (obj : Any) : Array[AnyRef] = {
    Console.println("getChildren" + obj)
    obj match {
      case something : VernacularRegion => something.getOutline.toArray
      case something : FileEditorInput => if (root == null) Array[AnyRef]() else root.getOutline.toArray
      case _ => Array[AnyRef]()
    }
  }
  def getElements (obj : Any) : Array[AnyRef] = {
    Console.println("getElements " + obj)
    getChildren(obj)
  }

  def getParent (obj : Any) : AnyRef = {
    Console.println("getParent " + obj)
    null //FIXME: Figure out how to do this - perhaps compute a table of parents?
  }
}

import org.eclipse.ui.editors.text.FileDocumentProvider

object CoqJavaDocumentProvider extends FileDocumentProvider {
  import org.eclipse.jface.text.IDocument
  import dk.itu.sdg.javaparser.JavaAST
  import scala.util.parsing.input.CharArrayReader

  object JavaToCoq extends JavaAST { }

  override def getDefaultEncoding () : String = "UTF-8"

  import dk.itu.sdg.javaparser.FinishAST
  def updateCoqCode (coq : IDocument, s : String, name : String) : Unit = {
    val (prog, spec) = JavaToCoq.parseNoSpec(new CharArrayReader(s.toArray), name)
    //Console.println("got prog " + prog + " and spec " + spec)
    //TODO: actually do a proper diff - there might be changes from the user in that area
    //like Definition of eeqptr, elt, eand, eneqptr in Snap.v
    val old = coq.get
    val pstart = old.indexOf(prog.substring(0, 15))
    val proend = prog.lastIndexOf("End ")
    val pend = old.indexOf(prog.substring(proend, prog.length))
    Console.println("old start " + pstart + " old end " + pend) //+ ":: " + old.substring(pstart, pend + (prog.length - proend)))
    coq.replace(pstart, pend + (prog.length - proend) - pstart, prog)
    val sstart = old.indexOf(spec.substring(0, spec.indexOf(" :=")))
    val specend = spec.lastIndexOf("}}.")
    val send = old.indexOf(spec.substring(specend, spec.length))
    Console.println("old spec start " + sstart + " old spec end " + send) // + ":: " + old.substring(sstart, send + (spec.length - specend)))
    val off = if (pend + (prog.length - proend) - pstart == prog.length) 0 else 1
    coq.replace(sstart + off, send + (spec.length - specend) - sstart, spec)
  }

  // The model of Coq code, used for outline view etc.
  import dk.itu.sdg.coqparser.VernacularRegion
  private val contentProviders : collection.mutable.Map[IDocument, CoqContentProvider] =
    collection.mutable.Map.empty
  def getOutline (doc : IDocument) : Option[CoqContentProvider] = {
    val outline = contentProviders.get(doc) getOrElse {
      val cprov = new CoqContentProvider()
      cprov.documentProvider = Some(this)
      contentProviders += (doc -> cprov)
      cprov
    }
    outline.parse(doc)
    Some(outline)
  }
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
import dk.itu.sdg.coqparser.VernacularReserved

object CoqTokenScanner extends RuleBasedScanner with VernacularReserved with EclipseUtils {
  import org.eclipse.jface.text.rules.{IToken, MultiLineRule, SingleLineRule, Token, WordRule}
  import org.eclipse.jface.text.{IDocument, TextAttribute}
  import org.eclipse.swt.SWT.{BOLD, ITALIC}

  Console.println("Initializing CoqTokenScanner")

  private val black = color(0, 0, 0)
  private val white = color(255, 255, 255)

  private val keywordToken : IToken = new Token(new TextAttribute(black, white, BOLD))
  private val definerToken : IToken = new Token(new TextAttribute((0, 30, 0), white, BOLD))
  private val opToken : IToken = new Token(new TextAttribute((0, 0, 30), white, 0))
  private val commentToken : IToken = new Token(new TextAttribute((30, 30, 0), white, ITALIC))
  private val otherToken : IToken = new Token(new TextAttribute(black, white, 0))

  private val rules = Seq(
    new MultiLineRule("(*", "*)", commentToken),
    new SingleLineRule("(*", "*)", commentToken)
  )

  private val wordRule = new WordRule(CoqWordDetector, otherToken)
  for (k <- keyword) wordRule.addWord(k, definerToken)
  for (k <- keywords) wordRule.addWord(k, keywordToken)
  for (o <- operator) wordRule.addWord(o, opToken)

  setRules((rules ++ Seq(wordRule)).toArray)
}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
import org.eclipse.jface.text.IDocument
class CoqOutlineReconcilingStrategy(var document : IDocument, editor : CoqEditor) extends IReconcilingStrategy with EclipseUtils {
  import dk.itu.sdg.coqparser.VernacularRegion
  import org.eclipse.jface.text.{IDocument, IRegion, Position}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.reconciler._
  import org.eclipse.ui.views.contentoutline.IContentOutlinePage

  def reconcile (region : DirtyRegion, subregion : IRegion) : Unit = ()

  def reconcile (partition : IRegion) : Unit = {
    println("Reconciling " + partition + " with editor = " + editor + " and doc = " + document)
    val outline = CoqJavaDocumentProvider.getOutline(document) // updates model as side effect
    if (editor != null) {
      val outlinePage = editor.getAdapter(classOf[IContentOutlinePage]).asInstanceOf[CoqContentOutlinePage]
      Display.getDefault.asyncExec(new java.lang.Runnable {
        def run() : Unit = {
          outlinePage.update() //Update GUI outline
        }
      })
    } else
      println(" null editor")

    // update folding
    outline map (_.root) foreach { root =>
      if (editor != null && root != null) editor.updateFolding(root, document)
    }
  }

  def setDocument(doc : IDocument) : Unit = {
    document = doc
  }
}


import org.eclipse.jface.text.source.SourceViewerConfiguration

class CoqSourceViewerConfiguration(editor : CoqEditor) extends SourceViewerConfiguration {
  import org.eclipse.jface.text.presentation.{IPresentationReconciler, PresentationReconciler}
  import org.eclipse.jface.text.rules.DefaultDamagerRepairer
  import org.eclipse.jface.text.{TextAttribute,IDocument}
  import org.eclipse.jface.text.reconciler.{IReconciler, MonoReconciler}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.source.ISourceViewer
  import org.eclipse.jface.text.contentassist.{IContentAssistant,ContentAssistant}

  override def getContentAssistant(v : ISourceViewer) : IContentAssistant = {
    val assistant= new ContentAssistant
    assistant.setContentAssistProcessor(CoqContentAssistantProcessor, IDocument.DEFAULT_CONTENT_TYPE)
    assistant
  }

  override def getReconciler (v : ISourceViewer) : IReconciler = {
    val strategy = new CoqOutlineReconcilingStrategy(v.getDocument, editor)
    val reconciler = new MonoReconciler(strategy, false)
    reconciler
  }

  override def getPresentationReconciler (v : ISourceViewer) : IPresentationReconciler = {
    val pr = new PresentationReconciler
    println("About to create damager/repairer")
    val ddr = new DefaultDamagerRepairer(CoqTokenScanner, new TextAttribute(new Color(Display.getDefault, new RGB(0, 0, 220))))
    println("Created damager/repairer successfully")
    pr.setDamager(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr.setRepairer(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr
  }
}

import org.eclipse.jface.text.contentassist.IContentAssistProcessor

object CoqContentAssistantProcessor extends IContentAssistProcessor {
  import org.eclipse.jface.text.contentassist.{IContextInformationValidator,IContextInformation,ICompletionProposal,CompletionProposal,ContextInformation}
  import org.eclipse.jface.text.ITextViewer
//  import org.eclipse.ui.examples.javaeditor.JavaEditorMessages
  import java.text.MessageFormat

  private val completions = Array("intros", "assumption", "induction ", "apply")

  def computeCompletionProposals(viewer : ITextViewer, documentOffset : Int) : Array[ICompletionProposal] = {
    val result= new Array[ICompletionProposal](completions.length);
	var i : Int = 0
    while (i < completions.length) {
//	  val info = new ContextInformation(completions(i), MessageFormat.format(JavaEditorMessages.getString("CompletionProcessor.Proposal.ContextInfo.pattern"), Array(completions(i))));
//	  result(i)= new CompletionProposal(completions(i), documentOffset, 0, completions(i).length, null, completions(i), info, MessageFormat.format(JavaEditorMessages.getString("CompletionProcessor.Proposal.hoverinfo.pattern"), Array(completions(i))));
	  val info = new ContextInformation(completions(i), MessageFormat.format("CompletionProcessor.Proposal.ContextInfo.pattern"));
	  result(i)= new CompletionProposal(completions(i), documentOffset, 0, completions(i).length, null, completions(i), info, MessageFormat.format("CompletionProcessor.Proposal.hoverinfo.pattern"));
	  i += 1
    }
	return result;
    }

  def computeContextInformation(viewer : ITextViewer, offset : Int) : Array[IContextInformation] = null
  def getCompletionProposalAutoActivationCharacters() : Array[Char] = Array('.')
  def getContextInformationAutoActivationCharacters() : Array[Char] = null
  def getContextInformationValidator() : IContextInformationValidator = null
  def getErrorMessage() : String = "not yet implemented"
}


import org.eclipse.ui.views.contentoutline.ContentOutlinePage

class CoqContentOutlinePage extends ContentOutlinePage {
  import dk.itu.sdg.coqparser.VernacularRegion
  import org.eclipse.jface.text.{IDocument, DocumentEvent}
  import org.eclipse.jface.viewers.{ITreeContentProvider, LabelProvider, TreeViewer, Viewer, SelectionChangedEvent, StructuredSelection}
  import org.eclipse.swt.widgets.{Composite, Control}
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.ui.texteditor.{IDocumentProvider, ITextEditor}

  var documentProvider : Option[IDocumentProvider] = None
  var textEditor : ITextEditor = null
  def this(provider : org.eclipse.ui.texteditor.IDocumentProvider, editor : org.eclipse.ui.texteditor.ITextEditor) = {
    this()
    documentProvider = Some(provider)
    textEditor = editor
  }

  var input : Any = null // TODO: Make into less of a direct Java port

  def setInput (arg : AnyRef) : Unit = {
    input = arg
    update()
  }

  def update () : Unit = {
    val viewer : TreeViewer = getTreeViewer()
    println("Called update when input = [" + input + "] and viewer = [" + viewer + "]")

    if (viewer != null) {
      val control : Control = viewer.getControl()
      if (control != null && !control.isDisposed()) {
        control.setRedraw(false)
        viewer.setInput(input)
        val doc = textEditor.getDocumentProvider.getDocument(input)
        Console.println("  In update(), document is " + doc)
        CoqJavaDocumentProvider.getOutline(doc) foreach { cprov => viewer.setContentProvider(cprov) }
        viewer.expandAll()
        control.setRedraw(true)
      }
    }
  }

  override def selectionChanged(event : SelectionChangedEvent) : Unit = {
    import dk.itu.sdg.parsing.{LengthPosition, NoLengthPosition, RegionPosition}

    super.selectionChanged(event)

    val selection = event.getSelection

    if (!selection.isEmpty) {
      val sel = selection.asInstanceOf[StructuredSelection].getFirstElement.asInstanceOf[VernacularRegion]
      sel.pos match {
        case NoLengthPosition => println("missing position from parser!"); textEditor.resetHighlightRange
        case at : RegionPosition => textEditor.setHighlightRange(at.offset, at.length, true)
        case _ => ()
      }
    }
  }

  class CoqLabelProvider extends LabelProvider {
    override def getText(obj : AnyRef) : String = obj match {
      case something : VernacularRegion => something.outlineName + " " + something.outlineNameExtra
      case something => something.toString
    }
  }

  override def createControl(parent : Composite) : Unit = {
    super.createControl(parent)

    val viewer : TreeViewer = getTreeViewer()
    viewer.setContentProvider(new CoqContentProvider()) // Just to satisfy precondition - replaced later!
    viewer.setLabelProvider(new CoqLabelProvider())
    viewer.addSelectionChangedListener(this)

    if (input != null)
      viewer.setInput(input)

    update()
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
import dk.itu.sdg.util.KopitiamLogger
class NewCoqFileWizard extends Wizard with INewWizard with KopitiamLogger {
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
        case e : PartInitException => log.warning("Caught an " + e)
      }
    }
    !file.isEmpty
  }
}
