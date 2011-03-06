/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.editors.text.TextEditor

class CoqEditor extends TextEditor {
  import org.eclipse.jface.text.source.ISourceViewer

  override protected def initializeEditor() : Unit = {
    System.setProperty("file.encoding", "UTF-8")
    Console.println("initializeEditor was called")
    Console.println(" - initializeEditor called super")
    setDocumentProvider(CoqJavaDocumentProvider)
    Console.println(" - document provider set")
    setSourceViewerConfiguration(CoqSourceViewerConfiguration);
    Console.println(" - source viewer configuration set")
    super.initializeEditor();
  }

  def getSource () : ISourceViewer = {
    getSourceViewer();
  }
}

import org.eclipse.jface.text.IDocumentListener

object CoqJavaDocumentChangeListener extends IDocumentListener {
  import org.eclipse.jface.text.DocumentEvent
  import scala.collection.mutable.HashMap
  import org.eclipse.jface.text.IDocument
  private val docTS = new HashMap[IDocument,Long]()

  override def documentAboutToBeChanged (ev : DocumentEvent) : Unit = ()

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

import org.eclipse.jface.text.Document
import org.eclipse.ui.editors.text.FileDocumentProvider

object CoqJavaDocumentProvider extends FileDocumentProvider {
  import org.eclipse.jface.text.IDocument
  import dk.itu.sdg.javaparser.JavaAST
  import scala.util.parsing.input.CharArrayReader
  import scala.collection.mutable.HashMap

  object JavaToCoq extends JavaAST { }

  override def getDefaultEncoding () : String = "UTF-8"

  import org.eclipse.ui.part.FileEditorInput
  override def getDocument (ele : Object) : IDocument = {
    assert(ele.isInstanceOf[FileEditorInput])
    val element = ele.asInstanceOf[FileEditorInput]
    val document = super.getDocument(element)
    val nam = element.getName
    if (nam.endsWith(".java"))
      if (EclipseTables.StringToDoc.contains(nam))
        EclipseTables.StringToDoc(nam)
      else {
        val doc = new Document(translate(document.get))
    	Console.println("adding doc " + nam + " mapping into " + doc + " into table")
    	EclipseTables.StringToDoc += nam -> doc
    	//doc.addDocumentListener(CoqJavaDocumentChangeListener)
    	doc
      }
    else
      document
  }

  def translate (s : String) : String = {
    //Console.println("translating " + s)
    JavaToCoq.parse(new CharArrayReader(s.toArray))
  }
  
  import dk.itu.sdg.javaparser.FinishAST
  def up (coq : IDocument, s : String) : Unit = {
	//Console.println("updating " + s)
	val (dat, off, len) = FinishAST.update(FinishAST.doitHelper(JavaToCoq.parseH(new CharArrayReader(s.toArray))))
	Console.println("received at offset " + off + "(old len " + len + ") data[" + dat.length + "] " + dat)
	coq.replace(off, len, dat) //somehow off-by-two...
  }
}

import org.eclipse.jface.text.rules.{ITokenScanner, RuleBasedScanner}
import dk.itu.sdg.coqparser.VernacularReserved
import org.eclipse.jface.text.rules.IWordDetector

object CoqWordDetector extends IWordDetector {
  // TODO: Look up in spec...
  def isWordStart(character : Char) =
    (character >= 'a' && character <= 'z') ||
    (character >= 'A' && character <= 'Z') ||
    (character >= '0' && character <= '9')

  def isWordPart(character : Char) = isWordStart(character)
}

object CoqTokenScanner extends RuleBasedScanner with VernacularReserved {
  import org.eclipse.jface.text.rules.{IToken, MultiLineRule, SingleLineRule, Token, WordRule}
  import org.eclipse.jface.text.{IDocument, TextAttribute}
  import org.eclipse.swt.graphics.Color
  import org.eclipse.swt.widgets.Display
  import org.eclipse.swt.SWT.{BOLD, ITALIC}

  Console.println("Initializing CoqTokenScanner")

  private val display = Display getCurrent
  private def color (r : Int, g : Int, b : Int) = new Color(display, r, g, b)
  implicit def tuple2Color (vals : (Int, Int, Int)) : Color = color(vals._1, vals._2, vals._3)
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
    println("About to create damager/repairer")
    val ddr = new DefaultDamagerRepairer(CoqTokenScanner, new TextAttribute(new Color(Display.getDefault, new RGB(0, 0, 220))))
    println("Created damager/repairer successfully")
    pr.setDamager(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr.setRepairer(ddr, IDocument.DEFAULT_CONTENT_TYPE)
    pr
  }
}

object EclipseTables {
  import scala.collection.mutable.HashMap
  import org.eclipse.jface.text.IDocument

  val DocToString = new HashMap[IDocument,String]()
  val StringToDoc = new HashMap[String,IDocument]()
}

object EclipseBoilerPlate {
  import org.eclipse.ui.{IWorkbenchWindow,IEditorPart}
  import org.eclipse.ui.texteditor.{ITextEditor,IDocumentProvider,AbstractTextEditor}
  import org.eclipse.jface.text.{IDocument,ITextSelection}

  var window : IWorkbenchWindow = null

  def getCaretPosition () : Int = {
    val sel = window.getActivePage.getActiveEditor.asInstanceOf[CoqEditor].getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
    Console.println("cursor position is " + sel.getLength + " @" + sel.getOffset)
    sel.getOffset
  }

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
    val l = CoqTop.findPreviousCommand(content, DocumentState.position)
    Console.println("prev (" + DocumentState.position + " [" + content(DocumentState.position) + "]): " + l)
    if (l > -1) {
      DocumentState.sendlen = DocumentState.position - l
      CoqTop.writeToCoq("Undo. ") //wrong - only right in Proof mode - and even there Backtrack is more popular
    }
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = ()

  override def dispose () : Unit = ()
}

object CoqUndoAction extends CoqUndoAction { }

class CoqStepAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  var fini : Boolean = false //need to set to false somewhere (when doc changed)

  override def init (window_ : IWorkbenchWindow) : Unit = ()

  override def run (action : IAction) : Unit = {
    Console.println("run called, sending a command")
    CoqStartUp.start()
    //EclipseBoilerPlate.getCaretPosition()
    Console.println("searching in content (start @" + DocumentState.position + ")")
    val content = EclipseBoilerPlate.getContent.drop(DocumentState.position)
    if (content.length > 0) {
      val eoc = CoqTop.findNextCommand(content)
      Console.println("eoc is " + eoc)
      if (eoc == -1) {
        fini = true
        Console.println("EOF")
      } else {
        DocumentState.sendlen = eoc
        Console.println("command is (" + eoc + "): " + content.take(eoc))
        CoqTop.writeToCoq(content.take(eoc)) //sends comments over the line
      }
    }
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = ()

  override def dispose () : Unit = ()
}

object CoqStepAction extends CoqStepAction { }

object CoqStartUp extends CoqCallback {
  private var st : Boolean = false

  def start () : Unit = {
    if (! CoqTop.isStarted) {
      PrintActor.register(CoqStartUp)
      CoqTop.startCoq
      if (EclipseConsole.out == null)
        EclipseConsole.initConsole
      PrintActor.stream = EclipseConsole.out
      while (!st) { }
      PrintActor.deregister(CoqStartUp)
    }
  }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(m, t) => st = true
      case y =>
    }
  }
}



class CoqStepNotifier extends CoqCallback {
  var err : Boolean = false
  var test : Option[(Int) => Boolean] = None

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqError(m) => err = true
      case CoqShellReady(monoton, tokens) =>
        if (! err) {
          DocumentState.commit
          if (test.isDefined && test.get(DocumentState.position))
            PrintActor.deregister(this)            
          else {
            CoqStepAction.run(null)
            if (CoqStepAction.fini)
              PrintActor.deregister(this)
          }
        } else
          PrintActor.deregister(this)
      case x => Console.println("got something, try again player 1 " + x)
    }
  }
}

class CoqStepAllAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection

  override def init (window_ : IWorkbenchWindow) : Unit = ()

  override def run (action : IAction) : Unit = {
    CoqStartUp.start()
    Console.println("registering CoqStepNotifier to PrintActor, now stepping")
    PrintActor.register(new CoqStepNotifier())
    //we need to provoke first message to start callback loops
    CoqStepAction.run(null)
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = ()

  override def dispose () : Unit = ()
}

object CoqStepAllAction extends CoqStepAllAction { }

class CoqStepUntilAction extends IWorkbenchWindowActionDelegate {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection

  override def init (window_ : IWorkbenchWindow) : Unit = ()

  override def run (action : IAction) : Unit = {
    CoqStartUp.start()
    //need to go back one more step
    val togo = CoqTop.findPreviousCommand(EclipseBoilerPlate.getContent, EclipseBoilerPlate.getCaretPosition + 2)
    //Console.println("togo is " + togo + ", curpos is " + EclipseBoilerPlate.getCaretPosition)
    if (DocumentState.position < togo) {
      val coqs = new CoqStepNotifier()
      coqs.test = Some((x : Int) => x >= togo)
      PrintActor.register(coqs)
      CoqStepAction.run(null)
    }
  }

  override def selectionChanged (action : IAction, selection : ISelection) : Unit = ()

  override def dispose () : Unit = ()
}

object DocumentState {
  import org.eclipse.jface.text.{ITextViewer}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
 
  var sourceview : ITextViewer = null
  var position : Int = 0
  var sendlen : Int = 0

  //def position : Int = position_
  //def position_= (x : Int) { Console.println("new pos is " + x + " (old was " + position_ + ")"); position_ = x }

  def undo () : Unit = synchronized {
    //Console.println("undo (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
      val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
      Display.getDefault.syncExec(
        new Runnable() {
          def run() = sourceview.setTextColor(bl, position - sendlen, sendlen, true)
        });
      position -= sendlen
      sendlen = 0
    }
  }

  def commit () : Unit = synchronized {
    //Console.println("commited (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
      //Console.println("commited - and doing some work")
      val bl = new Color(Display.getDefault, new RGB(0, 0, 220))
      Display.getDefault.syncExec(
        new Runnable() {
          def run() = sourceview.setTextColor(bl, position, sendlen, true)
        });
      position += sendlen
      sendlen = 0
    }
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
    Console.println("received in dispatch " + x)
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton)
          DocumentState.commit
        else
          DocumentState.undo
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

  def computeContextInformation(viewer : ITextViewer, offset : Int) : Array[IContextInformation] = null
  def getCompletionProposalAutoActivationCharacters() : Array[Char] = Array('.')
  def getContextInformationAutoActivationCharacters() : Array[Char] = null
  def getContextInformationValidator() : IContextInformationValidator = null
  def getErrorMessage() : String = "not yet implemented"
}
