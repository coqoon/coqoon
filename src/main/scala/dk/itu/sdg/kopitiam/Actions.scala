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
    ActionDisabler.disableAll
    val coqstarted = CoqTop.isStarted
    val acted = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.getActiveEditor
    if (DocumentState.activeEditor != acted) {
      if (DocumentState.resource != null)
        EclipseBoilerPlate.unmarkReally
      if (DocumentState.coqmarker != null)
        DocumentState.coqmarker.delete
      DocumentState.coqmarker = null
      DocumentState.undoAll
      DocumentState.activeEditor = acted.asInstanceOf[CoqEditor]

      val initial =
        if (DocumentState.positionToShell.contains(0))
          DocumentState.positionToShell(0).globalStep
        else
          2
      DocumentState.positionToShell.clear
      DocumentState.position = 0
      DocumentState.sendlen = 0

      if (CoqOutputDispatcher.goalviewer != null)
        CoqOutputDispatcher.goalviewer.clear

      if (! coqstarted)
        CoqStartUp.start
      if (coqstarted) {
        DocumentState.setBusy
        PrintActor.deregister(CoqOutputDispatcher)
        val shell = CoqState.getShell
        PrintActor.register(CoqStartUp)
        CoqTop.writeToCoq("Backtrack " + initial + " 0 " + shell.context.length + ".")
        while (! CoqStartUp.fini) { }
        CoqStartUp.fini = false
      }
    } else
      if (! coqstarted)
        CoqStartUp.start
    doit
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

  def disableAll () = {
    actions.foreach(_.setEnabled(false))
  }

  def enableMaybe () = {
    //Console.println("maybe enable " + DocumentState.position + " len " + DocumentState.content.length)
    if (DocumentState.position == 0)
      enableStart
    else if (DocumentState.position + 1 >= DocumentState.content.length)
      actions.zip(ends).filterNot(_._2).map(_._1).foreach(_.setEnabled(true))
    else
      actions.foreach(_.setEnabled(true))
  }

  def enableStart () = {
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
      //curshell is head of prevs, so we better have one more thing
      val prevs = DocumentState.positionToShell.keys.toList.sort(_ > _)
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
      val ctxdrop = curshell.context.length - prevshell.context.length
      DocumentState.realundo = true
      EclipseBoilerPlate.unmarkReally
      DocumentState.sendlen = DocumentState.position - prevs(i)
      prevs.take(i).foreach(DocumentState.positionToShell.remove(_))
      assert(DocumentState.positionToShell.contains(0) == true)
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
    DocumentState.position = 0
    DocumentState.sendlen = 0
    PrintActor.register(CoqStartUp)
    val shell = CoqState.getShell
    DocumentState.undoAll
    EclipseBoilerPlate.unmarkReally
    val initial =
      if (DocumentState.positionToShell.contains(0))
        DocumentState.positionToShell(0).globalStep
      else {
        Console.println("CoqRetractAction: retracting without position information, using 2")
        2
      }
    DocumentState.positionToShell.keys.toList.filterNot(_ == 0).foreach(DocumentState.positionToShell.remove(_))
    CoqTop.writeToCoq("Backtrack " + initial + " 0 " + shell.context.length + ".")
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqRetractAction extends CoqRetractAction { }

class CoqStepAction extends KCoqAction {
  override def doit () : Unit = {
    if (DocumentState.readyForInput) {
      //Console.println("run called, sending a command")
      val con = DocumentState.content
      val content = con.drop(DocumentState.position)
      if (content.length > 0) {
        val eoc = CoqTop.findNextCommand(content)
        //Console.println("eoc is " + eoc)
        if (eoc > 0) {
          DocumentState.setBusy
          DocumentState.sendlen = eoc
          DocumentState.process
          val cmd = content.take(eoc).trim
          Console.println("command is (" + eoc + "): " + cmd)
          //CoqProgressMonitor.actor.tell(("START", cmd))
          CoqTop.writeToCoq(cmd) //sends comments over the line
        }
      }
    }
  }
  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAction extends CoqStepAction { }

class CoqStepAllAction extends KCoqAction {
  override def doit () : Unit = {
    //Console.println("registering CoqStepNotifier to PrintActor, now stepping")
    //CoqProgressMonitor.multistep = true
    DocumentState.reveal = false
    PrintActor.register(CoqStepNotifier)
    //we need to provoke first message to start callback loops
    CoqStepAction.doit()
  }
  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAllAction extends CoqStepAllAction { }

class CoqStepUntilAction extends KCoqAction {
  import org.eclipse.swt.widgets.Display

  override def doit () : Unit = {
    val togo = CoqTop.findNextCommand(DocumentState.content.drop(EclipseBoilerPlate.getCaretPosition)) + EclipseBoilerPlate.getCaretPosition
    //doesn't work reliable when inside a comment
    Console.println("togo is " + togo + ", curpos is " + EclipseBoilerPlate.getCaretPosition + ", docpos is " + DocumentState.position)
    if (DocumentState.position == togo) { } else
    if (DocumentState.position < togo) {
      DocumentState.until = CoqTop.findPreviousCommand(DocumentState.content, togo - 2)
      DocumentState.process
      //CoqProgressMonitor.multistep = true
      DocumentState.reveal = false
      CoqStepNotifier.test = Some((x : Int, y : Int) => y >= togo)
      PrintActor.register(CoqStepNotifier)
      CoqStepAction.doit()
    } else { //Backtrack
      //go forward till cursor afterwards
      DocumentState.reveal = false
      PrintActor.register(CoqLater(() =>
        Display.getDefault.syncExec(
          new Runnable() {
            def run() = CoqStepUntilAction.doit
          })))
      CoqUndoAction.doitReally(EclipseBoilerPlate.getCaretPosition)
    }
  }
  override def start () : Boolean = false
  override def end () : Boolean = false
}
object CoqStepUntilAction extends CoqStepUntilAction { }

class RestartCoqAction extends KAction {
  override def doit () : Unit = {
    CoqTop.killCoq
    DocumentState.position = 0
    DocumentState.sendlen = 0
    DocumentState.undoAll
    DocumentState.positionToShell.clear
    EclipseBoilerPlate.unmarkReally
    PrintActor.deregister(CoqOutputDispatcher)
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

class TranslateAction extends KAction {
  import org.eclipse.ui.handlers.HandlerUtil
  import org.eclipse.jface.viewers.IStructuredSelection
  import org.eclipse.jdt.core.ICompilationUnit
  import org.eclipse.core.resources.{IResource,IFile}
  import org.eclipse.core.commands.ExecutionEvent
  import java.io.{InputStreamReader,ByteArrayInputStream}
  import scala.util.parsing.input.StreamReader

  import dk.itu.sdg.javaparser.JavaAST
  object JavaTC extends JavaAST { }

  import org.eclipse.jdt.core.ICompilationUnit
  override def execute (ev : ExecutionEvent) : Object = {
    Console.println("execute translation!")
    val sel = HandlerUtil.getActiveMenuSelection(ev).asInstanceOf[IStructuredSelection]
    val fe = sel.getFirstElement
    Console.println("fe is " + fe + " type " + fe.asInstanceOf[AnyRef].getClass)
    if (fe.isInstanceOf[IFile])
      translate(fe.asInstanceOf[IFile])
    else if (fe.isInstanceOf[ICompilationUnit])
      translate(fe.asInstanceOf[ICompilationUnit].getResource.asInstanceOf[IFile])
    null
  }

  def translate (file : IFile) : Unit = {
    val nam = file.getName
    if (nam.endsWith(".java")) {
      val trfi = file.getProject.getFile(nam + ".v") //TODO: find a suitable location!
      val is = StreamReader(new InputStreamReader(file.getContents))
      if (trfi.exists)
        trfi.delete(true, false, null)
      trfi.create(new ByteArrayInputStream(JavaTC.parse(is, nam.substring(0, nam.indexOf(".java"))).getBytes), IResource.NONE, null)
    } else
      Console.println("wasn't a java file")
  }

  override def doit () : Unit = ()
}
object TranslateAction extends TranslateAction { }

case class CoqLater (later : () => Unit) extends CoqCallback {
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(m, t) =>
        PrintActor.deregister(this)
        later()
      case _ =>
    }
  }
}

object CoqStartUp extends CoqCallback {
  private var initialize : Int = 0
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

  import java.io.File
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(m, t) =>
      	Console.println("dispatch, initialize is " + initialize + " fini is " + fini + " in CoqStartUp")
        val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
        val lp = new File(loadp).exists
        if (initialize == 0) {
          DocumentState.setBusy
          CoqTop.writeToCoq("Add LoadPath \"" + EclipseBoilerPlate.getProjectDir + "\".")
          initialize = 1
        } else if (initialize == 1 && lp) {
          DocumentState.setBusy
          CoqTop.writeToCoq("Add LoadPath \"" + loadp + "\".")
          initialize = 2
        } else {
          PrintActor.deregister(CoqStartUp)
          PrintActor.register(CoqOutputDispatcher)
          initialize = 0
          fini = true
          ActionDisabler.enableMaybe
        }
      case y =>
    }
  }
}

object CoqStepNotifier extends CoqCallback {
  var err : Boolean = false
  var test : Option[(Int, Int) => Boolean] = None

  import org.eclipse.swt.widgets.Display

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqError(m) => err = true
      case CoqUserInterrupt() => err = true
      case CoqShellReady(monoton, tokens) =>
        if (err)
          fini
        else
          if (test.isDefined && test.get(DocumentState.position,
                                         DocumentState.position + CoqTop.findNextCommand(DocumentState.content.drop(DocumentState.position))))
            fini
          else if (monoton) {
            CoqStepAction.doit
            //is that really needed?
            val drops = DocumentState.position + DocumentState.sendlen
            if (drops >= DocumentState.content.length || CoqTop.findNextCommand(DocumentState.content.drop(drops)) == -1)
              fini
          } else
            fini
      case x => //Console.println("got something, try again player 1 " + x)
    }
  }

  def fini () : Unit = {
    PrintActor.deregister(CoqStepNotifier)
    err = false
    test = None
    DocumentState.reveal = true
    DocumentState.until = -1
    //CoqProgressMonitor.multistep = false
    //CoqProgressMonitor.actor.tell("FINISHED")
  }
}

object CoqOutputDispatcher extends CoqCallback {
  import org.eclipse.swt.widgets.Display
  import org.eclipse.core.resources.IMarker

  var goalviewer : GoalViewer = null

  override def dispatch (x : CoqResponse) : Unit = {
    //Console.println("received in dispatch " + x)
    x match {
      case CoqShellReady(monoton, token) =>
        //CoqProgressMonitor.actor.tell("FINISHED")
        if (monoton)
          EclipseBoilerPlate.unmark
        ActionDisabler.enableMaybe
        if (!monoton && token.context.length == 0 && goalviewer != null)
          goalviewer.clear
      case CoqGoal(n, goals) =>
        //Console.println("outputdispatcher, n is " + n + ", goals:\n" + goals)
        val (hy, res) = goals.splitAt(goals.findIndexOf(_.contains("======")))
        val ht = if (hy.length > 0) hy.map(_.filterNot(_ == '\r')).reduceLeft(_ + "\n" + _) else ""
        val subd = res.findIndexOf(_.contains("subgoal "))
        val (g, r) = if (subd > 0) res.splitAt(subd) else (res, List[String]())
        val gt = if (g.length > 1) g.drop(1).map(_.filterNot(_ == '\r')).reduceLeft(_ + "\n" + _) else ""
        val ot = if (r.length > 0) {
          val r2 = r.map(x => { if (x.contains("subgoal ")) x.drop(1) else x })
          r2.map(_.filterNot(_ == '\r')).reduceLeft(_ + "\n" + _)
        } else ""
        if (goalviewer != null)
          goalviewer.writeGoal(ht, gt, ot)
      case CoqProofCompleted() =>
        if (goalviewer != null)
          goalviewer.writeGoal("Proof completed", "", "")
      case CoqError(msg) =>
        //TODO: what if Error not found, should come up with a sensible message anyways!
        val mess = msg.findIndexOf(_.startsWith("Error"))
        val p = if (mess == -1) {
                  //"Syntax error" for example!
                  val err = msg.findIndexOf(_.contains("error"))
                  if (err == -1)
                    msg.drop(msg.findIndexOf(_.contains("^^")))
                  else
                    msg.drop(err)
                } else
                  msg.drop(mess)
        val ps = p.reduceLeft(_ + "\n" + _)
        if (msg.length > 0 && msg(0).contains("characters")) {
          //msg(0) is: Toplevel input, characters xx-yy
          val ff = msg(0).drop(msg(0).findIndexOf(_ == 's') + 2).trim
          val split = ff.findIndexOf(_ == '-')
          val pos0 = ff.substring(0, split).toInt
          val pos1 = ff.substring(split + 1, ff.length - 1).toInt
          EclipseBoilerPlate.mark(ps, IMarker.SEVERITY_ERROR, false, pos0, pos1 - pos0)
        } else
          EclipseBoilerPlate.mark(ps)
        EclipseConsole.out.println("Error: " + msg.reduceLeft(_ + "\n" + _))
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
    }
  }
}

