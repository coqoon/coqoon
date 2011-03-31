/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.editors.text.TextEditor

object EclipseUtils {
  //Handy implicits and functions that make dealing with Eclipse less verbose
  import org.eclipse.swt.graphics.Color
  import org.eclipse.swt.widgets.Display

  private def display = Display getCurrent
  def color (r : Int, g : Int, b : Int) = new Color(display, r, g, b)

  implicit def tuple2Color (vals : (Int, Int, Int)) : Color = color(vals._1, vals._2, vals._3)
}
import EclipseUtils.{color, tuple2Color}

class CoqEditor extends TextEditor {
  import org.eclipse.jface.text.source.ISourceViewer
  import org.eclipse.ui.views.contentoutline.{ContentOutlinePage, IContentOutlinePage}

  override protected def initializeEditor () : Unit = {
    Console.println("initializeEditor was called")
    setDocumentProvider(CoqJavaDocumentProvider)
    Console.println(" - document provider set")
    setSourceViewerConfiguration(CoqSourceViewerConfiguration)
    Console.println(" - source viewer configuration set")
    super.initializeEditor()
    Console.println(" - initializeEditor called super")
  }

  def getSource () : ISourceViewer = {
    getSourceViewer()
  }

  override def initializeKeyBindingScopes () : Unit = {
    setKeyBindingScopes(List("Kopitiam.context").toArray)
  }
  
  var outlinePage : CoqContentOutlinePage = null
  override def getAdapter (required : java.lang.Class[_]) : AnyRef = {
    if (required.isInterface && required.getName.endsWith("IContentOutlinePage")) {
      if (outlinePage == null) {
        outlinePage = new CoqContentOutlinePage(getDocumentProvider(), this)
        if (getEditorInput() != null)
          outlinePage.setInput(getEditorInput())
      }
      outlinePage;
    }
    else super.getAdapter(required);
  }
  
  override def editorSaved () : Unit = {
    if (outlinePage != null) outlinePage.update()
    super.editorSaved()
  }
}

class SimpleJavaEditor extends TextEditor {
  override def initializeEditor () : Unit = {
    setDocumentProvider(SimpleJavaDocumentProvider)
    super.initializeEditor()
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
    if (ele != null) {
      Console.println(ele)
      assert(ele.isInstanceOf[FileEditorInput])
      val element = ele.asInstanceOf[FileEditorInput]
      val document = super.getDocument(element)
      val nam = element.getName
      if (nam.endsWith(".java"))
        if (EclipseTables.StringToDoc.contains(nam))
          EclipseTables.StringToDoc(nam)
        else {
          val doc = new Document(translate(document.get, nam.substring(0, nam.indexOf(".java"))))
          Console.println("adding doc " + nam + " mapping into " + doc + " into table")
    	  EclipseTables.StringToDoc += nam -> doc
    	  //doc.addDocumentListener(CoqJavaDocumentChangeListener)
    	  doc
        }
      else
        document
    } else
        null
  }
  
  def translate (s : String, name : String) : String = {
    //Console.println("translating " + s)
    JavaToCoq.parse(new CharArrayReader(s.toArray), name)
  }
  
  import dk.itu.sdg.javaparser.FinishAST
  def up (coq : IDocument, s : String, name : String) : Unit = {
    val (prog, spec) = JavaToCoq.parseNoSpec(new CharArrayReader(s.toArray), name)
    //Console.println("got prog " + prog + " and spec " + spec)
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
}

object SimpleJavaDocumentProvider extends FileDocumentProvider {
  import dk.itu.sdg.javaparser.JavaOutput
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.jface.text.IDocument
  import scala.collection.mutable.HashMap

  val docs : HashMap[String,IDocument] = new HashMap[String,IDocument]()

  override def getDefaultEncoding () : String = "UTF-8"

  override def getDocument (ele : Object) : IDocument = {
    assert(ele.isInstanceOf[FileEditorInput])
    val elem = ele.asInstanceOf[FileEditorInput]
    val nam = elem.getName
    if (docs.contains(nam))
      docs(nam)
    else {
      val document = super.getDocument(ele)
      //Console.println("getdocument received " + document.get)
      val newt = JavaOutput.parseandoutput(document.get)
      Console.println("getDocument called, translated to " + newt)
      document.set(newt)
      docs += nam -> document
      document
    }
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

//import org.eclipse.jface.text.rules.DefaultDamageRepairer

//object CoqDamageRepairer extends DefaultDamageRepairer {
//}

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
class CoqOutlineReconcilingStrategy extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IDocument, IRegion}
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.reconciler._
  import org.eclipse.ui.views.contentoutline.ContentOutlinePage
  
  var document : IDocument = null
  
  def reconcile (region : DirtyRegion, subregion : IRegion) : Unit = ()
  
  def reconcile (partition : IRegion) : Unit = {
    println("Reconciling " + partition)
  }
  
  def setDocument (doc : IDocument) : Unit = {
    document = doc
  }
  
}


import org.eclipse.jface.text.source.SourceViewerConfiguration

object CoqSourceViewerConfiguration extends SourceViewerConfiguration {
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
    val strategy = new CoqOutlineReconcilingStrategy
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

  def getProjectDir () : String = {
    getResource.getProject.getLocation.toOSString
  }

  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.dialogs.MessageDialog
  def warnUser (title : String, message : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = MessageDialog.openWarning(Display.getDefault.getActiveShell, title, message)
      })
  }

  import org.eclipse.core.runtime.IProgressMonitor
  import org.eclipse.jface.dialogs.ProgressMonitorDialog
  import org.eclipse.swt.widgets.Shell
  class MyProgressMonitorDialog (parent : Shell) extends ProgressMonitorDialog(parent) {
    import org.eclipse.swt.widgets.Button
    def getC () : Button = cancel
  }

  import org.eclipse.swt.events.{MouseListener, MouseEvent}
  private var p : IProgressMonitor = null
  private var pmd : MyProgressMonitorDialog = null
  var multistep : Boolean = false
  private val nam = "Coq interaction"
  def startProgress () : Unit = {
    //Console.println("Starting progress monitor")
    if (p == null) {
    //assert(pmd == null)
      pmd = new MyProgressMonitorDialog(Display.getDefault.getActiveShell)
      pmd.setCancelable(true)
      pmd.open
      pmd.getC.addMouseListener(new MouseListener() {
        override def mouseDoubleClick (m : MouseEvent) : Unit = ()
        override def mouseDown (m : MouseEvent) : Unit = CoqTop.interruptCoq
        override def mouseUp (m : MouseEvent) : Unit = ()
      })
      p = pmd.getProgressMonitor
      p.beginTask(nam, IProgressMonitor.UNKNOWN)
    }
  }

  def nameProgress (n : String) : Unit = {
    if (p != null)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = {
            if (p != null)
              p.setTaskName(nam + ": " + n)
          }})
  }

  def finishedProgress () : Unit = {
    //Console.println("Finished progress monitor " + p)
    if (p != null && !multistep) {
      val oldp = p
      val oldpmd = pmd
      p = null
      pmd = null
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = {
            oldp.done
            oldpmd.close
          }
        })
    }
  }

  import org.eclipse.core.resources.IMarker

  def mark (text : String) : Unit = {
    val file = getResource
    val marker = file.createMarker(IMarker.PROBLEM)
    marker.setAttribute(IMarker.MESSAGE, text)
    marker.setAttribute(IMarker.LOCATION, file.getName)
    marker.setAttribute(IMarker.CHAR_START, DocumentState.position + 1)
    marker.setAttribute(IMarker.CHAR_END, DocumentState.position + DocumentState.oldsendlen - 1) //for tha whitespace
    marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR)
    marker.setAttribute(IMarker.TRANSIENT, true)
  }

  def unmark () : Unit = {
    getResource.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
  }

  def maybeunmark (until : Int) : Unit = {
    val marks = getResource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
    marks.foreach(x => if (x.getAttribute(IMarker.CHAR_START, 0) < until) x.delete)
  }
}

object CoqStartUp extends CoqCallback {
  private var first : Boolean = true
  var fini : Boolean = false

  def start () : Unit = {
    if (! CoqTop.isStarted) {
      PrintActor.register(CoqStartUp)
      if (EclipseConsole.out == null)
        EclipseConsole.initConsole
      PrintActor.stream = EclipseConsole.out
      if (! CoqTop.startCoq)
        EclipseBoilerPlate.warnUser("No Coq", "No Coq binary found, please specify one in the Kopitiam preferences page")
      else {
        while (!fini) { }
        fini = false
      }
      PrintActor.deregister(CoqStartUp)
    }
  }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(m, id, t) =>
        if (first) {
          DocumentState.coqstart = t.globalStep
          CoqTop.writeToCoq("Add LoadPath \"" + EclipseBoilerPlate.getProjectDir + "\".")
          first = false
        } else {
          PrintActor.deregister(CoqStartUp)
          PrintActor.register(CoqOutputDispatcher)
          first = true
          fini = true
          ActionDisabler.enableMaybe
        }
      case y =>
    }
  }
}

class CoqStepNotifier extends CoqCallback {
  var err : Boolean = false
  var test : Option[(Int) => Boolean] = None
  var walker : () => Unit = CoqStepAction.doit
  var undo : Boolean = false

  import org.eclipse.swt.widgets.Display

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqError(m) => err = true
      case CoqUserInterrupt() => err = true
      case CoqShellReady(monoton, id, tokens) =>
        if (! err) {
          if (monoton) DocumentState.commit(id) else DocumentState.undo(id)
          if (test.isDefined && test.get(DocumentState.position)) {
            fini
            if (undo)
              Display.getDefault.syncExec(
                new Runnable() {
                  def run() = CoqStepUntilAction.doit
                });
          } else if (monoton || undo) {
            walker()
            val drops = DocumentState.position + DocumentState.sendlen
            if (drops >= DocumentState.totallen || CoqTop.findNextCommand(EclipseBoilerPlate.getContent.drop(drops)) == -1)
              if (! undo) {
                Console.println("in drops >= or -1 case")
                fini
              }
          } else
            fini
        } else
          fini
      case x => //Console.println("got something, try again player 1 " + x)
    }
  }

  def fini () : Unit = {
    PrintActor.deregister(this)
    EclipseBoilerPlate.multistep = false
    EclipseBoilerPlate.finishedProgress
  }
}

object DocumentState {
  import org.eclipse.jface.text.{ITextViewer}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display
 
  var sourceview : ITextViewer = null
  //var position : Int = 0
  var sendlen : Int = 0
  var totallen : Int = 0
  var coqstart : Int = 0
  var realundo : Boolean = false

  import org.eclipse.core.resources.IMarker
  var coqmarker : IMarker = null

  var position_ : Int = 0
  def position : Int = position_
  def position_= (x : Int) {
    Console.println("new pos is " + x + " (old was " + position_ + ")");
    if (coqmarker == null) {
      val file = EclipseBoilerPlate.getResource
      coqmarker = file.createMarker(IMarker.BOOKMARK)
      coqmarker.setAttribute(IMarker.MESSAGE, "coq position")
      coqmarker.setAttribute(IMarker.LOCATION, file.getName)
      coqmarker.setAttribute(IMarker.TRANSIENT, true)
      coqmarker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO)
    }
    coqmarker.setAttribute(IMarker.CHAR_START, x)
    coqmarker.setAttribute(IMarker.CHAR_END, x)
    position_ = x
  }

  def undoAll () : Unit = synchronized {
    if (sourceview != null) {
      val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
      Display.getDefault.syncExec(
        new Runnable() {
          def run() = sourceview.setTextColor(bl, 0, totallen, true)
        });
    }
  }

  var lastcommit : Int = 0
  var oldsendlen : Int = 0
  def undo (id : Int) : Unit = synchronized {
    //Console.println("undo (@" + position + ", " + sendlen + ")")
    if (sendlen != 0 && id > lastcommit) {
      lastcommit = id
      if (realundo) {
        realundo = false
        val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
        val start = scala.math.max(position - sendlen, 0)
        Display.getDefault.syncExec(
          new Runnable() {
            def run() = sourceview.setTextColor(bl, start, sendlen, true)
          });
        position = start
        sendlen = 0
      } else { //just an error
        Console.println("undo: barf")
        oldsendlen = sendlen
        sendlen = 0
      }
    }
  }

  def commit (id : Int) : Unit = synchronized {
    //Console.println("commited (@" + position + ", " + sendlen + ")")
    if (sendlen != 0 && id > lastcommit) {
      lastcommit = id
      Console.println("commited - and doing some work")
      val bl = new Color(Display.getDefault, new RGB(0, 0, 220))
      val end = scala.math.min(sendlen, totallen - position)
      //Console.println("commiting, end is " + end + " (pos + len: " + (position + sendlen) + ")" + ", pos:" + position + " sourceview: " + sourceview)
      Display.getDefault.syncExec(
        new Runnable() {
          def run() = sourceview.setTextColor(bl, position, end, true)
        });
      position += end
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
  var goal : Text = null
  var othersubs : Text = null
  var comp : Composite = null

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new GridLayout(1, true))
    hypos = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    hypos.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //hypos.setText("foo\nbar")
    new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    goal = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    goal.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //goal.setText("baz")
    val other = new Label(comp, SWT.READ_ONLY)
    other.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    other.setText("other subgoals")
    othersubs = new Text(comp, SWT.READ_ONLY | SWT.MULTI)
    othersubs.setLayoutData(new GridData(GridData.FILL_HORIZONTAL))
    //othersubs.setText("buz\nfoobar")
    CoqOutputDispatcher.goalviewer = this
  }

  def clear () : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = {
          hypos.setText("")
          goal.setText("")
          othersubs.setText("")
          comp.layout
        }
      })
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
    //Console.println("received in dispatch " + x)
    x match {
      case CoqShellReady(monoton, id, token) =>
        EclipseBoilerPlate.finishedProgress
        if (monoton) {
          DocumentState.commit(id)
          EclipseBoilerPlate.unmark
        } else
          DocumentState.undo(id)
        ActionDisabler.enableMaybe
      case CoqGoal(n, goals) => {
          val (hy, res) = goals.splitAt(goals.findIndexOf(_.contains("======")))
          val ht = if (hy.length > 0) hy.reduceLeft(_ + "\n" + _) else ""
          val subd = res.findIndexOf(_.contains("subgoal "))
          val (g, r) = if (subd > 0) res.splitAt(subd) else (res, List[String]())
          val gt = if (g.length > 1) g.drop(1).reduceLeft(_ + "\n" + _) else ""
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
          goalviewer.goal.setText(goal)
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


import org.eclipse.ui.views.contentoutline.ContentOutlinePage

class CoqContentOutlinePage extends ContentOutlinePage {
  import dk.itu.sdg.coqparser.VernacularRegion
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jface.viewers.{ITreeContentProvider, LabelProvider, TreeViewer, Viewer, SelectionChangedEvent, StructuredSelection}
  import org.eclipse.swt.widgets.{Composite, Control}
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.ui.texteditor.{IDocumentProvider, ITextEditor}
  
  var documentProvider : IDocumentProvider = null
  var textEditor : ITextEditor = null
  def this(provider : org.eclipse.ui.texteditor.IDocumentProvider, editor : org.eclipse.ui.texteditor.ITextEditor) = {
    this()
    documentProvider = provider
    textEditor = editor
  }
  
  var input : Any = null // TODO: Make into less of a direct Java port
  def setInput (arg : AnyRef) : Unit = {
    input = arg
    update()
  }
  def update () : Unit = {
    println("Called update")
    val viewer : TreeViewer = getTreeViewer()
    
    if (viewer != null) {
      val control : Control = viewer.getControl()
      if (control != null && !control.isDisposed()) {
        control.setRedraw(false)
        viewer.setInput(input)
        viewer.expandAll()
        control.setRedraw(true)
      }
    }
  }
  
  protected class ContentProvider extends ITreeContentProvider {
    def dispose() : Unit = {}
    
    var content : List[VernacularRegion] = Nil
    val parser = new dk.itu.sdg.coqparser.VernacularParser {}
    var root : parser.VernacularDocument = null
    def parse(document : IDocument) : Unit = {
      import scala.util.parsing.input.CharSequenceReader
      
      println("parse the coq")
      val result = parser.parseString(document.get) map {doc => root = doc; doc}
      println("got " + result)
    }
    

    
    def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) : Unit = {
      if (newInput != null) {
        println("inputChanged")
        parse(documentProvider.getDocument(newInput))
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

  override def selectionChanged(event : SelectionChangedEvent) : Unit = {
    import scala.util.parsing.input.{Position, NoPosition, OffsetPosition}
    
    super.selectionChanged(event)
    
    val selection = event.getSelection
    
    if (!selection.isEmpty) {
      val sel = selection.asInstanceOf[StructuredSelection].getFirstElement.asInstanceOf[VernacularRegion]
      sel.pos match {
        case NoPosition => println("missing position from parser!"); textEditor.resetHighlightRange
        case at : OffsetPosition => textEditor.setHighlightRange(at.offset, 0, true)
        case _ => ()
      }  
    }
  }
  
  class CoqLabelProvider extends LabelProvider {
    override def getText(obj : AnyRef) : String = obj match {
      case something : VernacularRegion => something.outlineName
      case something => something.toString
    }
  }
  
  override def createControl(parent : Composite) : Unit = {

    super.createControl(parent)

    val viewer : TreeViewer = getTreeViewer()
    viewer.setContentProvider(new ContentProvider())
    viewer.setLabelProvider(new CoqLabelProvider())
    viewer.addSelectionChangedListener(this)

    if (input != null)
      viewer.setInput(input)
  }
  
}

