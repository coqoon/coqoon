/* (c) 2010-2011 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.editors.text.TextEditor

//this should go away soon (rather use JDT editor)
class SimpleJavaEditor extends TextEditor {
  override def initializeEditor () : Unit = {
    setDocumentProvider(SimpleJavaDocumentProvider)
    super.initializeEditor()
  }
}

import org.eclipse.ui.editors.text.FileDocumentProvider

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
      Console.println("SimpleJava getDocument called, translated")
      document.set(newt)
      docs += nam -> document
      document
    }
  }
}

trait EclipseUtils {
  //Handy implicits and functions that make dealing with Eclipse less verbose
  import org.eclipse.jface.text.Position
  import org.eclipse.swt.graphics.Color
  import org.eclipse.swt.widgets.Display
  import dk.itu.sdg.parsing._

  private def display = Display getCurrent
  def color (r : Int, g : Int, b : Int) = new Color(display, r, g, b)

  implicit def pos2eclipsePos (pos : LengthPosition) : Position =
    pos match {
      case NoLengthPosition => new Position(0)
      case RegionPosition(off, len) => new Position(off, len)
    }

  implicit def tuple2Color (vals : (Int, Int, Int)) : Color = color(vals._1, vals._2, vals._3)
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
    //Console.println("cursor position is " + sel.getLength + " @" + sel.getOffset)
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

object DocumentState extends CoqCallback {
  import org.eclipse.jface.text.{ITextViewer}
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display

  var sourceview : ITextViewer = null

  //we're not there yet
  //var document : String = null

  import scala.collection.mutable.HashMap
  var positionToShell : HashMap[Int,CoqShellTokens] = new HashMap[Int,CoqShellTokens]()

  var sendlen : Int = 0
  var totallen_ : Int = 0
  def totallen : Int = totallen_
  def totallen_= (n : Int) {
    if (position > n) {
      Console.println("totallen was decreased (" + n + "), now even smaller than position " + position + ", adjusting to " + (n - 1))
      position = n - 1
    }
    totallen_ = n
  }
  var realundo : Boolean = false

  import org.eclipse.core.resources.IMarker
  var coqmarker : IMarker = null

  var position_ : Int = 0
  def position : Int = position_
  def position_= (x : Int) {
    //Console.println("new pos is " + x + " (old was " + position_ + ")");
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

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton) {
          commit
          Console.println("filling table [" + position + "]: " + token)
          positionToShell += position -> token
        } else
          undo
      case y =>
    }
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

  var oldsendlen : Int = 0
  private def undo () : Unit = synchronized {
    //Console.println("undo (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
      if (realundo) {
        realundo = false
        val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
        val start = scala.math.max(position - sendlen, 0)
        Console.println("undo (start " + start + " len " + sendlen + " totallen " + totallen + ")")
        if (CoqUndoAction.text == None)
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

  private def commit () : Unit = synchronized {
    //Console.println("commited (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
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

  def writeGoal (assumptions : String, cgoal : String, othergoals : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = {
          hypos.setText(assumptions)
          goal.setText(cgoal)
          othersubs.setText(othergoals)
          comp.layout
        }
      })
  }

  def setFocus() : Unit = {
  //  viewer.getControl.setFocus
  }
}

object GoalViewer extends GoalViewer { }

import org.eclipse.ui.IStartup
class Startup extends IStartup {
  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    ActionDisabler.disableAll
    DocumentMonitor.init
    PrintActor.register(DocumentState)
    CoqTop.coqpath = Activator.getDefault.getPreferenceStore.getString("coqpath") + System.getProperty("file.separator")
    CoqTop.init
  }
}
