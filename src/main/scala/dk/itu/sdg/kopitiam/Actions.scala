/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.IWorkbenchWindowActionDelegate
import org.eclipse.core.commands.IHandler

abstract class KAction extends IWorkbenchWindowActionDelegate with IHandler {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  import org.eclipse.core.commands.{IHandlerListener,ExecutionEvent}

  override def init (w : IWorkbenchWindow) : Unit = ()
  override def dispose () : Unit = ()

  override def selectionChanged (a : IAction, s : ISelection) : Unit = ()

  private var handlers : List[IHandlerListener] = List[IHandlerListener]()
  override def addHandlerListener (h : IHandlerListener) : Unit = { handlers ::= h }
  override def removeHandlerListener (h : IHandlerListener) : Unit =
    { handlers = handlers.filterNot(_ == h) }

  override def run (a : IAction) : Unit = { doit }
  override def execute (ev : ExecutionEvent) : Object = { doit; null }

  override def isEnabled () : Boolean = true
  override def isHandled () : Boolean = true
  def doit () : Unit
}

import org.eclipse.ui.IEditorActionDelegate
abstract class KEditorAction extends KAction with IEditorActionDelegate {
  import org.eclipse.ui.IEditorPart
  import org.eclipse.jface.action.IAction
  var editor : IEditorPart = null

  override def setActiveEditor (a : IAction, t : IEditorPart) : Unit = {
    editor = t
  }
}

abstract class KCoqAction extends KAction {
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  import org.eclipse.core.commands.{IHandlerListener,ExecutionEvent}

  private var first : Boolean = true
  override def selectionChanged (a : IAction, s : ISelection) : Unit =
    { if (first) { first = false; ActionDisabler.registeraction(a, start(), end()) } }

  override def isEnabled () : Boolean = {
    if (DocumentState.position == 0 && start)
      false
    else if (DocumentState.position + 1 >= DocumentState.content.length && end)
      false
    else
      true
  }
  override def run (a : IAction) : Unit = { doitH }
  override def execute (ev : ExecutionEvent) : Object = { doitH; null }

  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  import org.eclipse.core.resources.{IResource, IMarker}
  def doitH () : Unit = {
    if (! ActionDisabler.ready)
      EclipseBoilerPlate.warnUser("Not ready", "Eclipse preference is not ready yet, please wait a moment")
    else {
      ActionDisabler.disableAll
      JavaPosition.unmark
      val coqstarted = CoqTop.isStarted
      val acted1 = PlatformUI.getWorkbench
      val acted2 = if (acted1 != null) acted1.getActiveWorkbenchWindow else null
      val acted3 = if (acted2 != null) acted2.getActivePage else null
      val acted = if (acted3 != null) acted3.getActiveEditor else null
      if (DocumentState.activeEditor != acted && acted.isInstanceOf[CoqEditor]) {
        if (DocumentState.resource != null)
          EclipseBoilerPlate.unmarkReally
        DocumentState.resetState
        JavaPosition.retract
        if (DocumentState.activeEditor != null)
          DocumentState.processUndo
        DocumentState.activeEditor = acted.asInstanceOf[CoqEditor]

        if (CoqOutputDispatcher.goalviewer != null)
          CoqOutputDispatcher.goalviewer.clear

        if (! coqstarted)
          CoqStartUp.start
        if (coqstarted) {
          DocumentState.setBusy
          PrintActor.deregister(CoqOutputDispatcher)
          PrintActor.deregister(CoqStepNotifier)
          PrintActor.deregister(CoqCommands)
          val shell = CoqState.getShell
          PrintActor.register(CoqStartUp)
          CoqStartUp.synchronized {
	    val initial = DocumentState.positionToShell(0).globalStep
	    CoqTop.writeToCoq("Backtrack " + initial + " 0 " + shell.context.length + ".")
	    CoqStartUp.wait
          }
        }
      } else if (! coqstarted) {
        Console.println("starting my own coqtop!")
        CoqStartUp.start
      }
      doit
    }
  }

  def start () : Boolean
  def end () : Boolean
}

object ActionDisabler {
  import org.eclipse.jface.action.IAction
  var actions : List[IAction] = List()
  var starts : List[Boolean] = List()
  var ends : List[Boolean] = List()
  def registeraction (act : IAction, start : Boolean, end : Boolean) : Unit = {
    actions ::= act
    starts ::= start
    ends ::= end
  }

  def ready () : Boolean = { initialized }
  private var initialized : Boolean = false
  def initializationFinished () : Unit = {
    initialized = true
    enableMaybe
  }

  def disableAll () = {
    actions.foreach(_.setEnabled(false))
  }

  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  def enableMaybe () = {
    if (initialized)
      //Console.println("maybe enable " + DocumentState.position + " len " + DocumentState.content.length)
      if (DocumentState.activated.isInstanceOf[CoqEditor])
        if (DocumentState.activated.asInstanceOf[CoqEditor] == DocumentState.activeEditor) {
          if (DocumentState.position == 0)
            enableStart
          else if (DocumentState.position + 1 >= DocumentState.content.length)
            actions.zip(ends).filterNot(_._2).map(_._1).foreach(_.setEnabled(true))
          else
            actions.foreach(_.setEnabled(true))
        } else
          enableStart
          else if (DocumentState.activated.isInstanceOf[JavaEditor])
            if (DocumentState.activated.asInstanceOf[JavaEditor] == JavaPosition.editor) {
              if (JavaPosition.getProj == null)
                disableAll
              else if (JavaPosition.getProj.proofShell != None)
                //we might want to enable only some
                actions.foreach(_.setEnabled(true))
            } else
              disableAll
  }

  def enableStart () = {
    if (initialized)
      actions.zip(starts).filterNot(_._2).map(_._1).foreach(_.setEnabled(true))
  }
}

class CoqUndoAction extends KCoqAction {
  override def doit () : Unit = {
    doitReally(DocumentState.position)
  }

  def doitReally (pos : Int) : Unit = {
    if (DocumentState.readyForInput) {
      DocumentState.setBusy
      //invariant: we're at position p, all previous commands are already sent to coq
      // this implies that the table (positionToShell) is fully populated until p
      // this also implies that the previous content is not messed up!
      val curshell = CoqState.getShell

      val prevshell : CoqShellTokens =
        if (JavaPosition.cur == None) {
          //curshell is head of prevs, so we better have one more thing
          val prevs = DocumentState.positionToShell.keys.toList.sortWith(_ > _)
          assert(prevs.length > 0)
          Console.println("working (pos: " + pos + ") on the following keys " + prevs)
          //now we have the current state (curshell): g cs l
          //we look into lastmost shell, either:
          //g-- cs l or g-- cs l-- <- we're fine
          //g-- cs+c l++ <- we need to find something with cs (just jumped into a proof)
          var i : Int = prevs.filter(_ >= pos).length
          if (i == prevs.length) //special case, go to lastmost
            i = i - 1
          var prevshell : CoqShellTokens = DocumentState.positionToShell(prevs(i))
          //Console.println("prevshell: " + prevshell + "\ncurshell: " + curshell)
          assert(prevshell.globalStep < curshell.globalStep) //we're decreasing!
          while (! prevshell.context.toSet.subsetOf(curshell.context.toSet) && prevs.length > (i + 1)) {
            i += 1
            prevshell = DocumentState.positionToShell(prevs(i))
          }
          DocumentState.sendlen = DocumentState.position - prevs(i)
          prevs.take(i).foreach(DocumentState.positionToShell.remove(_))
          assert(DocumentState.positionToShell.contains(0) == true)
          prevshell
        } else
          //we're java mode here...
          //find most recent statement which was send over the wire
          JavaPosition.getLastCoqStatement match {
            case Some(x) => x
            case None => Console.println("how did that happen???"); null
          }
      EclipseBoilerPlate.unmarkReally
      JavaPosition.unmark
      val ctxdrop = curshell.context.length - prevshell.context.length
      CoqTop.writeToCoq("Backtrack " + prevshell.globalStep + " " + prevshell.localStep + " " + ctxdrop + ".")
    }
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqUndoAction extends CoqUndoAction { }

class CoqRetractAction extends KCoqAction {
  override def doit () : Unit = {
    DocumentState.setBusy
    PrintActor.deregister(CoqOutputDispatcher)
    DocumentState.resetState
    PrintActor.register(CoqStartUp)
    val shell = CoqState.getShell
    DocumentState.processUndo
    EclipseBoilerPlate.unmarkReally
    JavaPosition.unmark
    JavaPosition.retract
    val initial =
      if (DocumentState.positionToShell.contains(0))
        DocumentState.positionToShell(0).globalStep
      else {
        Console.println("CoqRetractAction: retracting without position information, using 2")
        2
      }
    CoqTop.writeToCoq("Backtrack " + initial + " 0 " + shell.context.length + ".")
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqRetractAction extends CoqRetractAction { }

class CoqStepAction extends KCoqAction {
//should be elsewhere
import scala.annotation.tailrec
def countSubstring (str1 : String, str2 : String) : Int={
   @tailrec def count (pos : Int, c : Int) : Int={
      val idx = str1.indexOf(str2, pos)
      if (idx == -1) c else count(idx + str2.size, c + 1)
   }
   count(0,0)
}

  import java.io.File
  override def doit () : Unit = {
    Console.println("CoqStepAction.doit called, ready? " + DocumentState.readyForInput)
    if (DocumentState.readyForInput) {
      //why is this here?
      //log says "changing model while proving java code works now"
      CoqCommands.step
      //input protocol is too complex -- or rather too ad-hoc...
      val cmd = DocumentState.nextCommand
      val rcmd =
        cmd match {
          case Some(cmd) =>
            val cmd2 = CoqTop.filterComments(cmd)
            if (countSubstring(cmd2, "\"") % 2 == 1) {
              DocumentState.undo
              Console.println("not sending anything here!")
              EclipseBoilerPlate.mark("unterminated string!")
              PrintActor.distribute(CoqShellReady(false, CoqState.getShell))
              ""
            } else if (cmd2.contains("Add LoadPath") && ! new File(cmd2.substring(cmd2.indexOf("\"") + 1, cmd2.lastIndexOf("\""))).exists) {
              DocumentState.undo
              EclipseBoilerPlate.mark("LoadPath does not exist!")
              PrintActor.distribute(CoqShellReady(false, CoqState.getShell))
              ""
            } else
              cmd
          case None =>
            Console.println("trying my luck with JP")
            if (DocumentState.position + 1 >= DocumentState.content.size && JavaPosition.editor != null)
              JavaPosition.getCoqCommand match {
                case None =>
                  Console.println("no luck")
                  ""
                case Some(x) =>
                  Console.println("luck! " + x)
                  DocumentState.sendlen = x.size
                  x
              }
            else ""
        }
      if (rcmd.size > 0) {
        Console.println("command is: " + rcmd)
        DocumentState.setBusy
        DocumentState.process
        CoqTop.writeToCoq(rcmd) //sends comments over the line
      }
    }
  }

  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAction extends CoqStepAction { }

class CoqStepAllAction extends KCoqAction {
  override def doit () : Unit = {
    val nc = CoqTop.findNextCommand(DocumentState.content.drop(DocumentState.position))
    Console.println("step step step " + nc)
    if (nc != -1) {
      DocumentState.reveal = false
      CoqStepNotifier.active = true
      //we need to provoke first message to start callback loops
      CoqStepAction.doit()
    } else CoqCommands.step
  }
  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAllAction extends CoqStepAllAction { }

class CoqStepUntilAction extends KCoqAction {
  override def doit () : Unit = {
    val cursor = EclipseBoilerPlate.getCaretPosition
    reallydoit(cursor)
  }

  def reallydoit (cursor : Int) : Unit = {
    val togo = CoqTop.findNextCommand(DocumentState.content.drop(cursor)) + cursor
    Console.println("reallydoit: togo is " + togo + ", cursor is " + cursor + ", docpos is " + DocumentState.position)
    //doesn't work reliable when inside a comment
    if (DocumentState.position == togo)
      CoqCommands.step
    else if (DocumentState.position < togo) {
      val unt = CoqTop.findPreviousCommand(DocumentState.content, togo - 2)
      //I can't life with non-deterministic behaviour:
      // Step Until [x] ~> ~> ~> [x] ~> Step Until [x] <- runs one step further
      //Step Until should be idempotent if x doesn't change!
      if (unt > DocumentState.position) {
        DocumentState.until = unt
        DocumentState.process
        DocumentState.reveal = false
        CoqStepNotifier.test = Some((x : Int, y : Int) => y >= togo)
        CoqStepNotifier.active = true
        CoqStepAction.doit()
      } else
        CoqCommands.step
    } else { //Backtrack
      //go forward till cursor afterwards
      DocumentState.reveal = false
      CoqCommands.doLater(() => CoqStepUntilAction.reallydoit(cursor))
      CoqUndoAction.doitReally(cursor)
    }
  }
  override def start () : Boolean = false
  override def end () : Boolean = false
}
object CoqStepUntilAction extends CoqStepUntilAction { }

class CompileCoqAction extends KAction {
  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  override def doit () : Unit = {
    val acted = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.getActiveEditor
    val ip = acted.getEditorInput
    assert(ip.isInstanceOf[IFileEditorInput])
    val r = ip.asInstanceOf[IFileEditorInput].getFile
    new CoqCompileJob(r.getProject.getLocation.toFile, r.getName, false).schedule
  }
}
object CompileCoqAction extends CompileCoqAction { }

class RestartCoqAction extends KAction {
  override def doit () : Unit = {
    CoqTop.killCoq
    DocumentState.resetState
    DocumentState.processUndo
    EclipseBoilerPlate.unmarkReally
    JavaPosition.unmark
    JavaPosition.retract
    CoqCommands.empty
    PrintActor.deregister(CoqOutputDispatcher)
    PrintActor.deregister(JavaPosition)
    CoqStartUp.start
  }
}
object RestartCoqAction extends RestartCoqAction { }

class InterruptCoqAction extends KAction {
  override def doit () : Unit = {
    Console.println("interrupt called")
    DocumentState.processUndo
    CoqTop.interruptCoq
  }
}
object InterruptCoqAction extends InterruptCoqAction { }

//only enable in proof edit mode!
class CoqRefreshAction extends KCoqAction {
  override def doit () : Unit = {
    CoqTop.writeToCoq("Show. ")
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqRefreshAction extends CoqRefreshAction { }

class ProveMethodAction extends KEditorAction with EclipseJavaHelper {
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.text.ITextSelection
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.IMarker
  override def run (a : IAction) : Unit = {
    if (! ActionDisabler.ready)
      EclipseBoilerPlate.warnUser("Not ready", "Sorry, the Eclipse preference store is not yet ready. Wait a few seconds")
    else {
      //plan:
      // a: get project
      val edi : JavaEditor = editor.asInstanceOf[JavaEditor]
      val prov = edi.getDocumentProvider
      val doc = prov.getDocument(edi.getEditorInput)
      val bla = getRoot(edi.getEditorInput)
      val cu = getCompilationUnit(bla)
      //assign JavaPosition.editor - for error reporting in walkAST
      if (JavaPosition.editor != edi) {
        if (JavaPosition.editor != null) {
          if (CoqTop.isStarted)
            CoqRetractAction.doitH
          JavaPosition.retract
        }
        JavaPosition.editor = edi
      }
      JavaPosition.unmark
      // b: if outdated coqString: translate -- need to verify outdated...
      walkAST(cu, doc) //side effects: properties: coq output, spec ptr to method
      // c: find method and statement we want to prove
      if (JavaPosition.markers.length == 0) { //no errors!
        val selection = edi.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
        val off = selection.getOffset
        val node = findASTNode(cu, off, 0)
        val md = findMethod(node)
        val proj = EclipseTables.DocToProject(doc)
        proj.program = Some(cu)
        proj.proveMethod(md)
        CoqCommands.step
      }
    }
  }
  override def doit () : Unit = { }
}
object ProveMethodAction extends ProveMethodAction { }

class TranslateToSimpleJavaAction extends KAction {
  import org.eclipse.ui.handlers.HandlerUtil
  import org.eclipse.jface.viewers.IStructuredSelection
  import org.eclipse.core.resources.{IResource,IFile}
  import org.eclipse.core.commands.ExecutionEvent
  import org.eclipse.jdt.core.ICompilationUnit
  import java.io.{InputStreamReader,ByteArrayInputStream}
  import scala.util.parsing.input.StreamReader

  override def execute (ev : ExecutionEvent) : Object = {
    val sel = HandlerUtil.getActiveMenuSelection(ev).asInstanceOf[IStructuredSelection]
    val fe = sel.getFirstElement
    if (fe.isInstanceOf[IFile])
      translate(fe.asInstanceOf[IFile])
    else if (fe.isInstanceOf[ICompilationUnit])
      translate(fe.asInstanceOf[ICompilationUnit].getResource.asInstanceOf[IFile])
    null
  }

  def translate (file : IFile) : Unit = {
    val con : Option[String]=
      try
        Some(new java.util.Scanner(file.getContents, "UTF-8").useDelimiter("\\A").next())
      catch
      { case (e : Throwable) => None }
    con match {
      case Some(x) =>
        //XXX: need to rework translation
        /*
        val newcon = JavaOutput.parseandoutput(x)
        val fn = file.getName
        val nffm = file.getProjectRelativePath.removeLastSegments(1).toString + "/" + fn.substring(0, fn.length - 5) + "Simple.java"
        Console.println("new file name is " + nffm)
        val translatedfile = file.getProject.getFile(nffm)
        if (! translatedfile.exists)
          translatedfile.create(null, IResource.NONE, null)
        translatedfile.setCharset("UTF-8", null)

        val ncon = newcon.getBytes("UTF-8")

        translatedfile.setContents(new ByteArrayInputStream(ncon), IResource.NONE, null)
        */
      case None =>
    }
  }

  override def doit () : Unit = ()
}
object TranslateToSimpleJavaAction extends TranslateToSimpleJavaAction { }


object CoqStartUp extends CoqCallback {
  private var initialize : Int = 0
  
  def start () : Unit = synchronized {
    if (! CoqTop.isStarted) {
      PrintActor.deregister(CoqCommands)
      PrintActor.register(CoqStartUp)
      if (EclipseConsole.out == null)
        EclipseConsole.initConsole
      PrintActor.stream = EclipseConsole.out
      if (! CoqTop.startCoq)
        EclipseBoilerPlate.warnUser("No Coq", "No Coq binary found, please specify one in the Kopitiam preferences page")
      else {
        wait
      }
      PrintActor.deregister(CoqStartUp)
    }
  }

  import java.io.File
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(m, t) =>
      	//Console.println("CoqStartUp dispatch, initialize is " + initialize + " fini is " + fini)
        val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
        val lp = new File(loadp).exists
        if (initialize == 0) {
          DocumentState.setBusy
          if (lp) {
            CoqTop.writeToCoq("Add LoadPath \"" + loadp + "\".")
            initialize = 1
          } else {
            Console.println("loadpath from preferences does not exist")
            CoqTop.writeToCoq("Add LoadPath \"" + EclipseBoilerPlate.getProjectDir + "\".")
            initialize = 2
          }
        } else if (initialize == 1) {
            DocumentState.setBusy
            CoqTop.writeToCoq("Add LoadPath \"" + EclipseBoilerPlate.getProjectDir + "\".")
            initialize = 2
        } else {
          PrintActor.deregister(CoqStartUp)
          PrintActor.register(CoqOutputDispatcher)
          PrintActor.register(CoqStepNotifier)
          PrintActor.register(CoqCommands)
          if (CoqCommands.nonempty)
            PrintActor.register(CoqCommands)
          initialize = 0
          synchronized {
            notifyAll
          }
          ActionDisabler.enableMaybe
        }
      case y =>
    }
  }
}

object CoqStepNotifier extends CoqCallback {
  var err : Boolean = false
  var test : Option[(Int, Int) => Boolean] = None
  var active : Boolean = false

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqError(m, n, s, l) => if (active) err = true
      case CoqUserInterrupt() => if (active) err = true
      case CoqShellReady(monoton, tokens) =>
        if (active)
          if (err)
            fini
          else {
            val nc = CoqTop.findNextCommand(DocumentState.content.drop(DocumentState.position))
            if ((test.isDefined && test.get(DocumentState.position, DocumentState.position + nc)) || nc == -1)
              fini
            else if (monoton)
              CoqStepAction.doit
            else {
              //we got into an error state
              fini
              if (CoqState.proofMode)
                CoqRefreshAction.doit
            }
          }
      case x => //Console.println("got something, try again player 1 " + x)
    }
  }

  def fini () : Unit = {
    err = false
    active = false
    test = None
    DocumentState.reveal = true
    DocumentState.until = -1
    if (CoqState.proofMode)
      CoqRefreshAction.doit
  }
}

object CoqCommands extends CoqCallback {
  private var commands : List[() => Unit] = List[() => Unit]()

  def nonempty () : Boolean = { commands.size > 0 }

  def empty () : Unit = {
    commands = List[() => Unit]()
    PrintActor.deregister(CoqCommands)
  }

  def doLater (f : () => Unit) : Unit = {
    if (commands.size == 0)
      PrintActor.register(CoqCommands)
    commands = (commands :+ f)
  }

  def step () : Unit = {
    if (finished)
      if (commands.size != 0) {
        val c = commands.head
        commands = commands.tail
        //should we execute in a certain thread? UI?
        c()
      } else {
        PrintActor.deregister(CoqCommands)
        Console.println("no command to run, deregistered")
      }
  }

  private def finished () : Boolean = {
    //might also be used instead of reveal stuff in DocumentState!
    if (CoqStepNotifier.active)
      return false
    //if (CoqStartUp.fini == false)
      //unfortunately this is reset (to false) in CoqStartUp.start
    //  return false
    return true
  }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton)
          step
        else if (CoqState.lastCommand.startsWith("Backtrack"))
          step
        else
          empty
      case _ =>
    }
  }
}

object CoqOutputDispatcher extends CoqCallback {
  import org.eclipse.core.resources.IMarker

  var goalviewer : GoalViewer = null

  def stripAndReduce (x : List[String]) : String = {
    if (x.length > 0) x.map(_.filterNot(_ == '\r')).reduceLeft(_ + "\n" + _) else ""
  }

  override def dispatch (x : CoqResponse) : Unit = {
    //Console.println("received in dispatch " + x)
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton) {
          EclipseBoilerPlate.unmark
          JavaPosition.unmark
        }
        if (! CoqStepNotifier.active)
          ActionDisabler.enableMaybe
        if (!monoton && token.context.length == 0 && goalviewer != null)
          goalviewer.clear
      case CoqGoal(n, goals) =>
        if (!CoqStepNotifier.active && goalviewer != null) {
          //Console.println("outputdispatcher, n is " + n + ", goals:\n" + goals)
          val goal =
            if (goals.last.startsWith("(dependent evars:"))
              goals.dropRight(1)
            else
              goals
          val (hy, res) = goal.splitAt(goal.indexWhere(_.contains("======")))
          val ht = stripAndReduce(hy)
          var subgoals : List[String] = List[String]()
          var index : Int = 0
          while (index < res.length) {
            val rr = res.drop(index)
            //Console.println("index: " + index + " res.length: " + res.length + " list stuff: " + rr)
            val subd = rr.drop(1).indexWhere(_.contains("subgoal ")) + 1
            //Console.println("subd is " + subd)
            val (g, r) = if (subd > 0) rr.splitAt(subd) else (rr, List[String]())
            //Console.println("g is " + g + " r is " + r)
            val gt = if (g.length > 1) stripAndReduce(g.drop(1)).trim else ""
            //Console.println("gt " + gt)
            subgoals = gt :: subgoals
            if (subd > 0)
              index = index + subd
            else
              index = res.length
          }
          goalviewer.writeGoal(ht, subgoals.reverse)
        }
      case CoqProofCompleted() =>
        if (!CoqStepNotifier.active && goalviewer != null)
          goalviewer.writeGoal("Proof completed", List[String]())
      case CoqError(msg, msgnonl, start, len) =>
        if (len != 0)
          EclipseBoilerPlate.mark(msgnonl, IMarker.SEVERITY_ERROR, false, start, len)
        else
          EclipseBoilerPlate.mark(msgnonl)
        EclipseConsole.out.println("Error: " + msg)
      case CoqWarning(msg) =>
        EclipseBoilerPlate.mark(msg, IMarker.SEVERITY_WARNING, true)
        EclipseConsole.out.println("Warning: " + msg)
      case CoqUserInterrupt() =>
        EclipseConsole.out.println("Interrupted. Most likely in a sane state.")
      case CoqVariablesAssumed(v) =>
        EclipseConsole.out.println("Variable " + v + " assumed")
      case CoqTheoremDefined(t) =>
        EclipseConsole.out.println("Theorem " + t + " defined")
      case CoqUnknown(x) =>
        EclipseConsole.out.println(x)
      case CoqSearchResult(n, v) =>
        EclipseConsole.out.println(n + ": " + v)
    }
  }
}

