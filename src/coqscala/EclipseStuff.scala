package coqscala

import org.eclipse.ui.editors.text.TextEditor

class CoqEditor extends TextEditor {
  import org.eclipse.jface.text.source.ISourceViewer

  override protected def initializeEditor() : Unit = {
    System.setProperty("file.encoding", "UTF-8")
    Console.println("initializeEditor was called")
    super.initializeEditor();
    setDocumentProvider(CoqJavaDocumentProvider)
    setSourceViewerConfiguration(CoqSourceViewerConfiguration);
  }

  def getSource () : ISourceViewer = {
    getSourceViewer();
  }
}

//import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor
class CoqJavaEditor extends TextEditor {
  override protected def initializeEditor() : Unit = {
    System.setProperty("file.encoding", "UTF-8")
    super.initializeEditor();
    setDocumentProvider(MyJavaDocumentProvider)
  }
}

import org.eclipse.jface.text.IDocumentListener
object MyJavaDocumentListener extends IDocumentListener {
  import org.eclipse.jface.text.DocumentEvent
  override def documentAboutToBeChanged (ev : DocumentEvent) : Unit = {
  }

  override def documentChanged (ev : DocumentEvent) : Unit = {
    val doc = ev.getDocument
    val t = doc.get
    CoqJavaDocumentProvider.up(t) //t.substring(0, ev.getOffset) + ev.getText + t.substring(ev.getOffset + ev.getLength))
  }
}

import org.eclipse.ui.editors.text.FileDocumentProvider
object MyJavaDocumentProvider extends FileDocumentProvider {
  import org.eclipse.jface.text.{IDocument,Document}
  import scala.collection.mutable.HashMap

  val docTable = new HashMap[IDocument,IDocument]()

  override def getDefaultEncoding () : String = { "UTF-8" }

  override def getDocument (element : Object) : IDocument = {
    val doc = super.getDocument(element)
    if (docTable.contains(doc))
      doc
    else {
      docTable += doc -> doc
      doc.addDocumentListener(MyJavaDocumentListener)
      doc
    }
  }
}

object CoqJavaDocumentChangeListener extends IDocumentListener {
  import org.eclipse.jface.text.DocumentEvent
  import scala.collection.mutable.HashMap
  import org.eclipse.jface.text.IDocument
  private val docTS = new HashMap[IDocument,Long]()

  override def documentAboutToBeChanged (ev : DocumentEvent) : Unit = {
  }

  override def documentChanged (ev : DocumentEvent) : Unit = {
    val doc = ev.getDocument
    if (!docTS.contains(doc) | docTS(doc) < ev.getModificationStamp) {
      docTS += doc -> ev.getModificationStamp
      //ev.getDocument.removeDocumentListener(this)
      //ev.getDocument.replace(ev.getOffset, ev.getLength, ev.getText)
      //ev.getDocument.addDocumentListener(this)
    }
  }
}

object CoqJavaDocumentProvider extends FileDocumentProvider {
  import org.eclipse.jface.text.{IDocument,Document}
  import dk.itu.sdg.javaparser.JavaAST
  import scala.util.parsing.input.CharArrayReader
  import scala.collection.mutable.HashMap

  val docTable = new HashMap[IDocument,Document]()

  object JavaToCoq extends JavaAST { }

  override def getDefaultEncoding () : String = { "UTF-8" }

  override def getDocument (element : Object) : IDocument = {
    val doc = super.getDocument(element)
    if (docTable.contains(doc))
      docTable(doc)
    else {
      val source = doc.get
      val document = new Document(translate(source))
      docTable += doc -> document
      Console.println("added doc " + doc + " mapping into " + document + " into table")
      //document.addDocumentListener(CoqJavaDocumentChangeListener)
      document
    }
  }

  def translate (s : String) : String = {
    //Console.println("translating " + s)
    JavaToCoq.parse(new CharArrayReader(s.toArray))
  }
  
  import dk.itu.sdg.javaparser.FinishAST
  def up (s : String) : Unit = {
	Console.println("updating " + s)
	val (dat, off, len) = FinishAST.update(FinishAST.doitHelper(JavaToCoq.parseH(new CharArrayReader(s.toArray))))
	Console.println("received at offset " + off + "(old len " + len + ") data " + dat)
	docTable(docTable.keys.toList(0)).replace(off, len, dat)
  }
}

import org.eclipse.jface.text.rules.ITokenScanner

object CoqTokenScanner extends ITokenScanner {
  import org.eclipse.jface.text.rules.{IToken,Token}
  import org.eclipse.jface.text.IDocument

  private var off : Int = 0
  private var len : Int = 0

  override def getTokenLength () : Int = len
  override def getTokenOffset () : Int = off
  override def nextToken () : IToken = Token.UNDEFINED
  override def setRange (doc : IDocument, offset : Int, length : Int) : Unit = {
    off = offset
    len = length
  }
}

//import org.eclipse.jface.text.rules.DefaultDamageRepairer

//object CoqDamageRepairer extends DefaultDamageRepairer {
//}

import org.eclipse.jface.text.source.SourceViewerConfiguration

object CoqSourceViewerConfiguration extends SourceViewerConfiguration {
  import org.eclipse.jface.text.presentation.{IPresentationReconciler, PresentationReconciler}
  import org.eclipse.jface.text.rules.DefaultDamagerRepairer
  import org.eclipse.jface.text.{TextAttribute,IDocument}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.source.ISourceViewer
  import org.eclipse.jface.text.contentassist.{IContentAssistant,ContentAssistant}


  override def getContentAssistant(v : ISourceViewer) : IContentAssistant = {
    val assistant= new ContentAssistant
    assistant.setContentAssistProcessor(CoqContentAssistantProcessor, IDocument.DEFAULT_CONTENT_TYPE)
    assistant
  }

  override def getPresentationReconciler (v : ISourceViewer) : IPresentationReconciler = {
    val pr = new PresentationReconciler
    val ddr = new DefaultDamagerRepairer(CoqTokenScanner, new TextAttribute(new Color(Display.getDefault, new RGB(0, 0, 220))))
    pr
  }
}

object EclipseBoilerPlate {
  import org.eclipse.ui.{IWorkbenchWindow,IEditorPart}
  import org.eclipse.ui.texteditor.{ITextEditor,IDocumentProvider,AbstractTextEditor}
  import org.eclipse.jface.text.IDocument

  var window : IWorkbenchWindow = null

  def getContent () : String = {
    val editorpart = window.getActivePage.getActiveEditor
    if (editorpart.isInstanceOf[CoqEditor]) {
      val texteditor = editorpart.asInstanceOf[CoqEditor]
      val dp : IDocumentProvider = texteditor.getDocumentProvider
      val doc : IDocument = dp.getDocument(texteditor.getEditorInput)
      DocumentState.sourceview = texteditor.getSource //should only be called once, somehow!
      doc.get
    } else {
      Console.println("not a CoqEditor!")
      ""
    }
  }
  
  import org.eclipse.core.resources.{IResource, IFile}
  import org.eclipse.ui.{IEditorInput, IFileEditorInput}

  def getResource () : IFile = {
    val editorpart = window.getActivePage.getActiveEditor
    if (editorpart.isInstanceOf[CoqEditor]) {
      val texteditor = editorpart.asInstanceOf[CoqEditor]
      val ei : IEditorInput = texteditor.getEditorInput
      if (ei.isInstanceOf[IFileEditorInput]) {
        val fei = ei.asInstanceOf[IFileEditorInput]
        fei.getFile
      } else {
        Console.println("not a file editor")
        null
      }
    } else null
  }

  import org.eclipse.core.resources.IMarker

  def mark (text : String) : Unit = {
    val file = getResource
    val marker = file.createMarker(IMarker.PROBLEM)
    marker.setAttribute(IMarker.MESSAGE, text)
    marker.setAttribute(IMarker.LOCATION, file.getName)
    marker.setAttribute(IMarker.CHAR_START, DocumentState.position)
    marker.setAttribute(IMarker.CHAR_END, DocumentState.position + DocumentState.sendlen - 1) //for tha whitespace
    DocumentState.sendlen = 0
    marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR)
    marker.setAttribute(IMarker.TRANSIENT, true)
  }
}

import org.eclipse.ui.IWorkbenchWindowActionDelegate

class CoqUndoAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection

  override def init (window_ : IWorkbenchWindow) : Unit = {
    EclipseBoilerPlate.window = window_
    Console.println("init called")
  }

  override def run (action : IAction) : Unit = {
    val content = EclipseBoilerPlate.getContent()
    val l = findPrevious(content, DocumentState.position)
    //Console.println("prev (" + DocumentState.position + " [" + content(DocumentState.position) + "]): " + l)
    if (l > -1) {
      DocumentState.sendlen = DocumentState.position - l
      CoqTop.writeToCoq("Undo.")
    }
  }

  def findPrevious (content : String, pos : Int) : Int = {
    var cont = true
    var last = pos - 2
    val clco = content.lastIndexOf("*)", pos)
    if (clco > content.lastIndexOf(".", pos))
      last = content.lastIndexOf("(*", clco - 2)
    while (cont) {
      val newend = content.lastIndexOf(".", last - 1)
      if (newend <= 0) { cont = false; last = -1 }
      else {
        last = newend
        if (content(last - 1) != '.' && (content.startsWith(" ", last + 1) || content.startsWith("\n", last + 1)))
          cont = false
      }
    }
    last + 1 //don't color "."
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = { }

  override def dispose () : Unit = { }	
}

object CoqUndoAction extends CoqUndoAction { }

class CoqStepAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection

  override def init (window_ : IWorkbenchWindow) : Unit = {
  }

  override def run (action : IAction) : Unit = {
    if (! CoqTop.isStarted) {
      CoqTop.startCoq
      if (EclipseConsole.out == null)
        EclipseConsole.initConsole
      PrintActor.stream = EclipseConsole.out
    }
    val content = EclipseBoilerPlate.getContent.drop(DocumentState.position)
    if (content.length > 0) {
      val eoc = findEnd(content).min(content.length)
      DocumentState.sendlen = eoc
      Console.println("command is (" + eoc + "): " + content.take(eoc))
      CoqTop.writeToCoq(content.take(eoc))
    } else { Console.println("EOF") }
  }

  def findEnd (content : String) : Int = {
    var cont = true
    val comment = content.indexOf("(*")
    var endofcommand = 0
    if (comment < content.indexOf("."))
      endofcommand = content.indexOf("*)", comment + 2)
    while (cont) {
      val newend = content.indexOf(".", endofcommand + 1)
      if (newend == -1) cont = false
      else endofcommand = newend
      if (content(endofcommand - 1) != '.' && (content.startsWith(" ", endofcommand + 1) || content.startsWith("\n", endofcommand + 1)))
        cont = false
    }
    endofcommand + 2 //". "
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = { }

  override def dispose () : Unit = { }	
}

object CoqStepAction extends CoqStepAction { }

object DocumentState {
  import org.eclipse.jface.text.{ITextViewer}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
 
  var sourceview : ITextViewer = null
  var position : Int = 0
  var sendlen : Int = 0

  //def position : Int = position_
  //def position_= (x : Int) { Console.println("new pos is " + x + " (old was " + position_ + ")"); position_ = x }

  def undo () : Unit = {
    val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = sourceview.setTextColor(bl, position - sendlen, sendlen, true)
    });
    position -= sendlen
    sendlen = 0
  }

  def commit () : Unit = {
    val bl = new Color(Display.getDefault, new RGB(0, 0, 220))
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = sourceview.setTextColor(bl, position, sendlen, true)
    });
    position += sendlen
    sendlen = 0
  }
}

object EclipseConsole {
  import org.eclipse.ui.console.{MessageConsole,MessageConsoleStream,IConsole,IConsoleManager,ConsolePlugin}
  var out : MessageConsoleStream = null

  def initConsole () : Unit = {
    val conman : IConsoleManager = ConsolePlugin.getDefault.getConsoleManager
    val existing = conman.getConsoles
    var outputconsole : MessageConsole = null
    if (existing.length > 0) {
      Console.println("have existing console(s) : " + existing.length)
      outputconsole = existing(0).asInstanceOf[MessageConsole]
    } else {
      Console.println("needed to create new console")
      val mycon = new MessageConsole("Coq", null)
      val cons = new Array[IConsole](1)
      cons(0) = mycon
      conman.addConsoles(cons)
      outputconsole = mycon
    }
    out = outputconsole.newMessageStream
    out.setEncoding("UTF-8")
  }

//   display console in workbench!
//   IWorkbenchPage page = ...; obtain the active page
//   String id = IConsoleConstants.ID_CONSOLE_VIEW;
//   IConsoleView view = (IConsoleView) page.showView(id);
//   view.display(myConsole);
}

import org.eclipse.ui.part.ViewPart

class GoalViewer extends ViewPart {
  import org.eclipse.swt.widgets.{Composite,Label,Text}
  import org.eclipse.swt.SWT
  import org.eclipse.swt.layout.{GridData,GridLayout}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display


  var hypos : Text = null
  var goal : Label = null
  var othersubs : Text = null
  var comp : Composite = null

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new GridLayout(1, true))
    hypos = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    hypos.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //hypos.setText("foo\nbar")
    new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    goal = new Label(comp, SWT.READ_ONLY)
    goal.setBackground(new Color(Display.getDefault, new RGB(255, 255, 255)))
    goal.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //goal.setText("baz")
    val other = new Label(comp, SWT.READ_ONLY)
    other.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    other.setText("other subgoals")
    othersubs = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    othersubs.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //othersubs.setText("buz\nfoobar")
    CoqOutputDispatcher.goalviewer = this
    PrintActor.register(CoqOutputDispatcher)
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}

object GoalViewer extends GoalViewer { }

object CoqOutputDispatcher extends CoqCallback {
  import org.eclipse.swt.widgets.Display

  var goalviewer : GoalViewer = null
	
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqGoal(n, goals) => {
          val (hy, res) = goals.splitAt(goals.findIndexOf(_.contains("======")))
          val ht = if (hy.length > 0) hy.reduceLeft(_ + "\n" + _) else ""
          val subd = res.findIndexOf(_.contains("subgoal "))
          val (g, r) = if (subd > 0) res.splitAt(subd) else (res, List[String]())
          val gt = if (g.length > 1) g.drop(1).reduceLeft(_ + " " + _) else ""
          val ot = if (r.length > 0) {
            val r2 = r.map(x => { if (x.contains("subgoal ")) x.drop(1) else x })
            r2.reduceLeft(_ + "\n" + _)
          } else ""
          writeGoal(ht, gt, ot)
        }
      case CoqProofCompleted() => writeGoal("Proof completed", "", "")
      case CoqError(msg) => {
        //TODO: what if Error not found, should come up with a sensible message anyways!
        val ps = msg.drop(msg.findIndexOf(_.startsWith("Error")))
        EclipseBoilerPlate.mark(ps.reduceLeft(_ + " " + _))
      }
      case x => EclipseConsole.out.println("received: " + x)
    }
  }

  def writeGoal (assumptions : String, goal : String, othergoals : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = {
          goalviewer.hypos.setText(assumptions)
          goalviewer.goal.setText(" " + goal)
          goalviewer.othersubs.setText(othergoals)
          goalviewer.comp.layout
        }
      })
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

  def computeContextInformation(viewer : ITextViewer, offset : Int) : Array[IContextInformation] = { null }
  def getCompletionProposalAutoActivationCharacters() : Array[Char] = { Array('.') }
  def getContextInformationAutoActivationCharacters() : Array[Char] = { null }
  def getContextInformationValidator() : IContextInformationValidator = { null }
  def getErrorMessage() : String = { "not yet implemented" }
}
