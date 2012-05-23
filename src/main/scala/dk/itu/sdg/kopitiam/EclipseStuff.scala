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
    val commentoff = CoqTop.computeCommentOffset(con.drop(DocumentState.position), spos - DocumentState.position)
    marker.setAttribute(IMarker.CHAR_START, spos + commentoff)
    marker.setAttribute(IMarker.CHAR_END, epos + commentoff) //for tha whitespace
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
  var until : Int = -1
  var realundo : Boolean = false

  import org.eclipse.core.resources.IMarker
  import org.eclipse.core.runtime.CoreException
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
    try {
      coqmarker.setAttribute(IMarker.CHAR_START, x)
      coqmarker.setAttribute(IMarker.CHAR_END, x - 1) //at dot, not whitespace
      position_ = x
    } catch {
      case e : CoreException =>
        Console.println("caught CoreException")
        coqmarker.delete
        coqmarker = null
        position = x
    }
    //Console.println("position updated to " + x)
  }

  def readyForInput () : Boolean = { _readyForInput }
  def setBusy () : Unit = { _readyForInput = false }
  private var _readyForInput : Boolean = false
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton) {
          commit
          //Console.println("filling table (" + positionToShell.keys.toList.length + ") [" + position + "]: " + token)
          positionToShell += position -> token
        } else
          undo
        _readyForInput = true
      case y =>
    }
  }

  def undoAll () : Unit = {
    if (activeEditor != null)
      activeEditor.damager.addColors(0, 0, false)
  }

  var oldsendlen : Int = 0
  import org.eclipse.jface.text.{ Region, TextPresentation }
  import org.eclipse.swt.custom.StyleRange

  var reveal : Boolean = true
  var autoreveal : Boolean = false

  private def undo () : Unit = {
    //Console.println("undo (@" + position + ", sendlen: " + sendlen + ") real: " + realundo)
    if (sendlen != 0) {
      if (realundo) {
        val start = scala.math.max(position - sendlen, 0)
        realundo = false
        val rev = reveal
        activeEditor.damager.addColors(start, 0, rev)
        if (autoreveal) {
          reveal = true
          autoreveal = false
        }
        position = start
        sendlen = 0
      } else { //just an error
        //Console.println("undo: barf")
        //kill off process colored thingies
        activeEditor.damager.addColors(position, 0, false)
        oldsendlen = sendlen
        sendlen = 0
      }
    }
  }

  def process () : Unit = {
    activeEditor.damager.addColors(position, scala.math.max(until - position, sendlen), false)
  }

  def processUndo () : Unit = {
    activeEditor.damager.addColors(position, 0, false)
  }

  private def commit () : Unit = {
    //Console.println("commited (@" + position + ", " + sendlen + "): " + positionToShell.contains(position))
    if (sendlen != 0) {
      //Console.println("commited - and doing some work")
      val end = scala.math.min(sendlen, content.length - position)
      position += end
      sendlen = 0
      val len = scala.math.min(position, content.length)
      val rev = reveal
      activeEditor.damager.addColors(position, scala.math.max(until - position, sendlen), rev)
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
  import org.eclipse.swt.widgets.{Composite,Text}
  import org.eclipse.swt.SWT
  import org.eclipse.swt.layout.{FormData,FormLayout,FormAttachment}
  import org.eclipse.swt.graphics.{Color,RGB,Rectangle}
  import org.eclipse.swt.widgets.{Display,Sash,Listener,Event,TabFolder,TabItem}
//  import org.eclipse.swt.custom.{CTabFolder,CTabItem}


  var context : Text = null
  var goals : TabFolder = null
  var subgoals : List[TabItem] = List[TabItem]()
  var subgoalTexts : List[Text] = List[Text]()
  var comp : Composite = null

  /*
   * |---------------------|       -
   * | context             |       | comp (FormLayout)
   * |---------------------| sash  |
   * | goals (TabFolder)   |       |
   * |---------------------|       -
   */

  override def createPartControl (parent : Composite) : Unit = {
    comp = new Composite(parent, SWT.NONE)
    comp.setLayout(new FormLayout())

    context = new Text(comp, SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)

    goals = new TabFolder(comp, SWT.NONE)

    val sash = new Sash(comp, SWT.HORIZONTAL)

    //sash
    val contextData = new FormData()
    contextData.left = new FormAttachment(0, 0)
    contextData.right = new FormAttachment(100, 0)
    contextData.top = new FormAttachment(0, 0)
    contextData.bottom = new FormAttachment(sash, 0)
    context.setLayoutData(contextData)

    val goalData = new FormData()
    goalData.left = new FormAttachment(0, 0)
    goalData.right = new FormAttachment(100, 0)
    goalData.top = new FormAttachment(sash, 0)
    goalData.bottom = new FormAttachment(100, 0)
    goals.setLayoutData(goalData)

    val limit = 20
    val percent = 50
    val sashData = new FormData()
    sashData.left = new FormAttachment(0, 0)
    sashData.right = new FormAttachment(100, 0)
    sashData.top = new FormAttachment(percent, 0)
    sash.setLayoutData(sashData)
    sash.addListener(SWT.Selection, new Listener () {
      def handleEvent (e : Event) = {
        val sashRect : Rectangle = sash.getBounds()
        val shellRect : Rectangle = comp.getClientArea()
        val top = shellRect.height - sashRect.height - limit
        e.y = scala.math.max(scala.math.min(e.y, top), limit)
        if (e.y != sashRect.y)  {
          sashData.top = new FormAttachment (0, e.y)
          comp.layout()
        }
      }
    });

    CoqOutputDispatcher.goalviewer = this
  }

  def clear () : Unit = {
    writeGoal("", List[String]())
  }

  def writeGoal (assumptions : String, sgoals : List[String]) : Unit = {
    Display.getDefault.syncExec(
      new Runnable() {
        def run() =
          if (! comp.isDisposed) {
            context.setText(assumptions)
            Console.println("sgoals: " + sgoals.length + " subgoals: " + subgoals.length)
            if (sgoals.length < subgoals.length) {
              //drop some, but keep one!
              val (stay, leave) = subgoals.splitAt(sgoals.length)
              val (stayT, leaveT) = subgoalTexts.splitAt(sgoals.length)
              subgoals = stay
              subgoalTexts = stayT
              leaveT.foreach(_.setText(""))
              leaveT.foreach(_.dispose)
              leave.foreach(_.dispose)
              goals.reskin(SWT.ALL)
            } else if (sgoals.length > subgoals.length) {
              //add some
              val tomake = sgoals.length - subgoals.length
              var index : Int = 0
              while (index < tomake) {
                val ti = new TabItem(goals, SWT.NONE)
                subgoals = subgoals ++ List(ti)
                ti.setText((subgoals.length + index - 1).toString)
                val te = new Text(goals, SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL)
                subgoalTexts = subgoalTexts ++ List(te)
                ti.setControl(te)
                index = index + 1
              }
            }
            var index : Int = 0
            while (index < sgoals.length) {
              subgoalTexts(index).setText(sgoals(index))
              index = index + 1
            }
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
