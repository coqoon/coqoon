/* (c) 2010-2012 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam
import dk.itu.sdg.util.KopitiamLogger

import org.eclipse.ui.editors.text.TextEditor

import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor

class SimpleJavaEditor extends CompilationUnitEditor {
  //setEditorContextMenuId("#CompilationUnitEditorContext"); //$NON-NLS-1$
  //setRulerContextMenuId("#CompilationUnitRulerContext"); //$NON-NLS-1$
  //setOutlinerContextMenuId("#CompilationUnitOutlinerContext"); //$NON-NLS-1$
  // don't set help contextId, we install our own help context
  //fSavePolicy= null;

  //fJavaEditorErrorTickUpdater= new JavaEditorErrorTickUpdater(this);
  //fCorrectionCommands= null;
  setDocumentProvider(SimpleJavaDocumentProvider);


  override def initializeEditor () : Unit = {
    Console.println("initializing SimpleJavaEditor!")
    setDocumentProvider(SimpleJavaDocumentProvider)
    Console.println("set document provider")
    super.initializeEditor()
    Console.println("called super")
    setDocumentProvider(SimpleJavaDocumentProvider)
    Console.println("initialized SimpleJavaEditor!")
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
    Console.println("get document called on SimpleJavaDocumentProvider")
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
  import org.eclipse.swt.graphics.{Color, RGB}
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

  implicit def rgb2color (rgb : RGB) : Color = new Color(display, rgb)

  def getPrefColor (key : String) : RGB = {
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    PreferenceConverter.getColor(store, key)
  }
}

object EclipseTables {
  import scala.collection.mutable.HashMap
  import org.eclipse.jface.text.IDocument

  val DocToString = new HashMap[IDocument,String]()
  val StringToDoc = new HashMap[String,IDocument]()
}
/*
import akka.actor._
class MyTimer extends Actor {
  def receive = {
    case ("START", x : Int) =>
      Thread.sleep(1000);
      if (CoqProgressMonitor.tick == x) {
        //Console.println("tick " + CoqProgressMonitor.tick + " is equal to x " + x)
        CoqProgressMonitor.actor.tell("REALLY")
      }
  }
}

object CoqProgressMonitor {
  var tick : Int = 0
  var timer : ActorRef = null
  var actor : ActorRef = null
  var multistep : Boolean = false
}

class CoqProgressMonitorImplementation extends Actor {
  import org.eclipse.core.runtime.IProgressMonitor
  import org.eclipse.jface.dialogs.ProgressMonitorDialog
  import org.eclipse.swt.widgets.Shell
  import org.eclipse.swt.widgets.Display
  class MyProgressMonitorDialog (parent : Shell) extends ProgressMonitorDialog(parent) {
    import org.eclipse.swt.widgets.Button
    def getC () : Button = cancel
  }

  import org.eclipse.swt.events.{MouseListener, MouseEvent}
  private var p : IProgressMonitor = null
  private var pmd : MyProgressMonitorDialog = null
  private val nam = "Coq interaction: "
  private var title : String = ""

  def receive = {
    case ("START", n : String) =>
      title = n
      //Console.println("Starting progress monitor")
      if (p == null)
        if (CoqProgressMonitor.multistep)
          this.self.tell("REALLY")
        else
          CoqProgressMonitor.timer.tell(("START", CoqProgressMonitor.tick))
      else
        Display.getDefault.asyncExec(
          new Runnable() {
            def run() = {
              if (p != null)
                p.setTaskName(nam + ": " + n)
            }})
    case "REALLY" =>
      //assert(pmd == null)
      Display.getDefault.asyncExec(
        new Runnable() {
          def run() = {
            pmd = new MyProgressMonitorDialog(Display.getDefault.getActiveShell)
            pmd.setCancelable(true)
            pmd.open
            pmd.getC.addMouseListener(new MouseListener() {
              override def mouseDoubleClick (m : MouseEvent) : Unit = ()
              override def mouseDown (m : MouseEvent) : Unit = CoqTop.interruptCoq
              override def mouseUp (m : MouseEvent) : Unit = ()
            })
            p = pmd.getProgressMonitor
            p.beginTask(nam + title, IProgressMonitor.UNKNOWN)
          }})
    case "FINISHED" =>
      CoqProgressMonitor.tick = CoqProgressMonitor.tick + 1
      //Console.println("Finished progress monitor " + p)
      if (p != null && !CoqProgressMonitor.multistep) {
        val oldp = p
        val oldpmd = pmd
        p = null
        pmd = null
        Display.getDefault.asyncExec(
          new Runnable() {
            def run() = {
              oldp.done
              oldpmd.close
              //Clients should not call this method (the workbench calls this method at appropriate times). To have the workbench activate a part, use IWorkbenchPage.activate(IWorkbenchPart) instead.
              DocumentState.activeEditor.setFocus
            }
          })
      }
    case x => Console.println("fell through receive of CoqProgressMonitor " + x)
  }
}
*/
object EclipseBoilerPlate {
  import org.eclipse.ui.{IWorkbenchWindow,IEditorPart}
  import org.eclipse.ui.texteditor.{ITextEditor,IDocumentProvider,AbstractTextEditor}
  import org.eclipse.jface.text.{IDocument,ITextSelection}

  var window : IWorkbenchWindow = null

  def getCaretPosition () : Int = {
    val sel = DocumentState.activeEditor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
    //Console.println("cursor position is " + sel.getLength + " @" + sel.getOffset)
    sel.getOffset
  }

  def getProjectDir () : String = {
    DocumentState.resource.getProject.getLocation.toOSString
  }

  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.dialogs.MessageDialog
  def warnUser (title : String, message : String) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() = MessageDialog.openWarning(Display.getDefault.getActiveShell, title, message)
      })
  }


  import org.eclipse.core.resources.{IMarker, IResource}

  def mark (text : String, severity : Int = IMarker.SEVERITY_ERROR, advance : Boolean = false, off : Int = 0, len : Int = 0) : Unit = {
    val file = DocumentState.resource
    var spos = if (advance) DocumentState.sendlen + DocumentState.position + 1 else DocumentState.position + 1
    val con = DocumentState.content
    while ((con(spos) == '\n' || con(spos) == ' ' || con(spos) == '\t') && spos < con.length)
      spos += 1
    spos += off
    val epos = if (advance)
                 spos + 1
               else if (len > 0)
                 spos + len
               else
                 DocumentState.position + DocumentState.oldsendlen - 1
    val marker = file.createMarker(IMarker.PROBLEM)
    marker.setAttribute(IMarker.MESSAGE, text)
    marker.setAttribute(IMarker.LOCATION, file.getName)
    marker.setAttribute(IMarker.CHAR_START, spos)
    marker.setAttribute(IMarker.CHAR_END, epos) //for tha whitespace
    marker.setAttribute(IMarker.SEVERITY, severity)
    marker.setAttribute(IMarker.TRANSIENT, true)
  }

  def unmarkReally () : Unit = {
    DocumentState.resource.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
  }

  def unmark () : Unit = {
    val marks = DocumentState.resource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
    marks.foreach(x => if (x.getAttribute(IMarker.SEVERITY, 0) == IMarker.SEVERITY_ERROR) x.delete)
  }

  def maybeunmark (until : Int) : Unit = {
    val marks = DocumentState.resource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)
    marks.foreach(x => if (x.getAttribute(IMarker.CHAR_START, 0) < until && x.getAttribute(IMarker.SEVERITY, 0) == IMarker.SEVERITY_ERROR) x.delete)
  }
}

object DocumentState extends CoqCallback with KopitiamLogger {
  import org.eclipse.jface.text.IDocument
  import org.eclipse.swt.graphics.{Color,RGB}
  import org.eclipse.swt.widgets.Display

  var activeEditor : CoqEditor = null

  def activeDocument () : IDocument = {
    if (activeEditor != null)
      activeEditor.getDocumentProvider.getDocument(activeEditor.getEditorInput)
    else
      null
  }

  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.IFile
  def resource () : IFile = {
    if (activeEditor != null)
      activeEditor.getEditorInput.asInstanceOf[IFileEditorInput].getFile
    else
      null
  }

  def content () : String = {
    if (activeEditor != null)
      activeDocument.get
    else
      "  " //such that ctrl-n works initially...
  }

  import scala.collection.mutable.HashMap
  var positionToShell : HashMap[Int,CoqShellTokens] = new HashMap[Int,CoqShellTokens]()

  var sendlen : Int = 0
  var realundo : Boolean = false

  import org.eclipse.core.resources.IMarker
  var coqmarker : IMarker = null

  var position_ : Int = 0
  def position : Int = position_
  def position_= (x : Int) {
    //Console.println("new pos is " + x + " (old was " + position_ + ")");
    if (coqmarker == null) {
      val file = resource
      coqmarker = file.createMarker(IMarker.BOOKMARK)
      coqmarker.setAttribute(IMarker.MESSAGE, "coq position")
      coqmarker.setAttribute(IMarker.LOCATION, file.getName)
      coqmarker.setAttribute(IMarker.TRANSIENT, true)
      coqmarker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO)
    }
    coqmarker.setAttribute(IMarker.CHAR_START, x)
    coqmarker.setAttribute(IMarker.CHAR_END, x - 1) //at dot, not whitespace
    position_ = x
  }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton) {
          commit
          //Console.println("filling table (" + positionToShell.keys.toList.length + ") [" + position + "]: " + token)
          positionToShell += position -> token
        } else
          undo
      case y =>
    }
  }

  def uncolor (offset : Int) : Unit = {
    if (activeEditor != null)
      Display.getDefault.syncExec(
        new Runnable() {
          def run() = activeEditor.getSource.invalidateTextPresentation()
        });
  }

  def undoAll () : Unit = { uncolor(0) }

  var oldsendlen : Int = 0
  import org.eclipse.jface.text.{ Region, TextPresentation }
  import org.eclipse.swt.custom.StyleRange

  var reveal : Boolean = true
  var autoreveal : Boolean = false

  private def undo () : Unit = {
    //Console.println("undo (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
      if (realundo) {
        realundo = false
        val bl = new Color(Display.getDefault, new RGB(0, 0, 0))
        val start = scala.math.max(position - sendlen, 0)
        val end = scala.math.min(sendlen + position, content.length)
        //Console.println("undo (start " + start + " send length " + sendlen + " content length " + content.length + " submitting length " + (end - start) + ")")
        val txtp = new TextPresentation(new Region(0, start), 20)
        txtp.setDefaultStyleRange(new StyleRange(0, start, null, sentColor))
        val rev = reveal
        if (activeEditor != null)
          Display.getDefault.syncExec(
            new Runnable() {
              def run() = {
                activeEditor.getSource.invalidateTextPresentation()
                activeEditor.getSource.changeTextPresentation(txtp, true)
                if (rev)
                  activeEditor.selectAndReveal(start, 0)
              }
            })
        if (autoreveal) {
          reveal = true
          autoreveal = false
        }
        position = start
        sendlen = 0
      } else { //just an error
        //Console.println("undo: barf")
        oldsendlen = sendlen
        sendlen = 0
      }
    }
  }

  def sentColor : Color = {
    //log.warning("Getting sent color")
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    new Color(Display.getDefault, PreferenceConverter.getColor(store, "coqSentBg"))
  }

  def sentProcessColor : Color = {
    //log.warning("Getting sent color")
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    new Color(Display.getDefault, PreferenceConverter.getColor(store, "coqSentProcessBg"))
  }

  def process (until : Int) : Unit = {
    val off = if (until == -1)
                sendlen
              else
                until - position
    val txtp = new TextPresentation(new Region(position, off), 20) //wtf 20?
    txtp.setDefaultStyleRange(new StyleRange(position, off, null, sentProcessColor))
    if (activeEditor != null)
      Display.getDefault.syncExec(
        new Runnable () {
          def run () =
            activeEditor.getSource.changeTextPresentation(txtp, true)
        })
  }

  def processUndo () : Unit = {
    val txtp = new TextPresentation(new Region(0, position), 20)
    txtp.setDefaultStyleRange(new StyleRange(0, position, null, sentColor))
    if (activeEditor != null)
      Display.getDefault.syncExec(
        new Runnable () {
          def run () = {
            activeEditor.getSource.invalidateTextPresentation()
            activeEditor.getSource.changeTextPresentation(txtp, true)
          }
        })
  }

  private def commit () : Unit = {
    //Console.println("commited (@" + position + ", " + sendlen + ")")
    if (sendlen != 0) {
      //Console.println("commited - and doing some work")
      val end = scala.math.min(sendlen, content.length - position)
      position += end
      sendlen = 0
      val len = scala.math.min(position, content.length)
      //Console.println("commiting, end is " + end + " (pos + len: " + (position + sendlen) + ")" + ", pos:" + position + ", submitted length " + (len - position))
      val txtp = new TextPresentation(new Region(0, position), 20)
      txtp.setDefaultStyleRange(new StyleRange(0, position, null, sentColor))
      val rev = reveal
      if (activeEditor != null)
        Display.getDefault.syncExec(
          new Runnable() {
            def run() = {
              activeEditor.getSource.changeTextPresentation(txtp, true)
              if (rev)
                activeEditor.selectAndReveal(position, 0)
            }
          })
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
//  import akka.actor.ActorSystem
  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    ActionDisabler.disableAll
    DocumentMonitor.init
    //val system = ActorSystem("Kopitiam")
    //CoqProgressMonitor.actor = system.actorOf(Props[CoqProgressMonitorImplementation], name = "ProgressMonitor")
    //CoqProgressMonitor.timer = system.actorOf(Props[MyTimer], name = "MyTimer")
    CoqTop.init
    PrintActor.register(DocumentState)
    CoqTop.coqpath = Activator.getDefault.getPreferenceStore.getString("coqpath") + System.getProperty("file.separator")
  }
}
