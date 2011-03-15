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

  private var first : Boolean = true
  override def selectionChanged (a : IAction, s : ISelection) : Unit =
    { if (first) { first = false; ActionDisabler.registeraction(a, start(), end()) } }

  private var handlers : List[IHandlerListener] = List[IHandlerListener]()
  override def addHandlerListener (h : IHandlerListener) : Unit = { handlers ::= h }
  override def removeHandlerListener (h : IHandlerListener) : Unit =
    { handlers = handlers.filterNot(_ == h) }

  override def run (a : IAction) : Unit = { doitH }
  override def execute (ev : ExecutionEvent) : Object = { doitH; null }

  override def isEnabled () : Boolean = {
    if (DocumentState.position == 0 && start)
      false
    else if (DocumentState.position + 1 >= DocumentState.totallen && end)
      false
    else
      true
  }
  override def isHandled () : Boolean = true

  import org.eclipse.ui.{IFileEditorInput, PlatformUI}
  import org.eclipse.core.resources.{IResource, IMarker}
  def doitH () : Unit = {
    ActionDisabler.disableAll
    if (EclipseBoilerPlate.window == null)
      EclipseBoilerPlate.window = PlatformUI.getWorkbench.getActiveWorkbenchWindow
    val coqstarted = CoqTop.isStarted
    if (! coqstarted)
      CoqStartUp.start
    val acted = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getActivePage.getActiveEditor
    if (DocumentMonitor.activeeditor != acted) {
      if (DocumentMonitor.activeeditor != null)
        DocumentMonitor.activeeditor.getEditorInput.asInstanceOf[IFileEditorInput].getFile.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO)

      DocumentMonitor.activeeditor = acted.asInstanceOf[CoqEditor]

      DocumentState.position = 0
      DocumentState.sendlen = 0
      DocumentState.undoAll
      DocumentState.sourceview = DocumentMonitor.activeeditor.getSource
      DocumentState.totallen = EclipseBoilerPlate.getContent.length
      CoqOutputDispatcher.goalviewer.clear

      if (coqstarted) {
        CoqStartUp.fini = false
        PrintActor.deregister(CoqOutputDispatcher)
        val shell = CoqState.getShell
        PrintActor.register(CoqStartUp)
        CoqTop.writeToCoq("Backtrack " + DocumentState.coqstart + " 0 " + shell.context.length + ".")
        while (! CoqStartUp.fini) { }
        CoqStartUp.fini = false
      }
    }
    doit
  }

  def doit () : Unit
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
    Console.println("maybe enable " + DocumentState.position + " len " + DocumentState.totallen)
    if (DocumentState.position == 0)
      enableStart
    else if (DocumentState.position + 1 >= DocumentState.totallen)
      actions.zip(ends).filterNot(_._2).map(_._1).foreach(_.setEnabled(true))
    else
      actions.foreach(_.setEnabled(true))
  }

  def enableStart () = {
    actions.zip(starts).filterNot(_._2).map(_._1).foreach(_.setEnabled(true))
  }
}

class CoqUndoAction extends KAction {
  override def doit () : Unit = {
    val content = EclipseBoilerPlate.getContent()
    val l = CoqTop.findPreviousCommand(content, DocumentState.position)
    Console.println("prev (" + DocumentState.position + " [" + content(DocumentState.position) + "]): " + l)
    if (l > -1) {
      DocumentState.sendlen = DocumentState.position - l
      DocumentState.realundo = true
      EclipseBoilerPlate.unmark
      CoqTop.writeToCoq("Undo.") //wrong - only right in Proof mode - and even there Backtrack is more popular
    }
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqUndoAction extends CoqUndoAction { }

class CoqRetractAction extends KAction {
  override def doit () : Unit = {
    PrintActor.deregister(CoqOutputDispatcher)
    DocumentState.position = 0
    DocumentState.sendlen = 0
    PrintActor.register(CoqStartUp)
    val shell = CoqState.getShell
    DocumentState.undoAll
    EclipseBoilerPlate.unmark
    CoqTop.writeToCoq("Backtrack " + DocumentState.coqstart + " 0 " + shell.context.length + ".")
  }
  override def start () : Boolean = true
  override def end () : Boolean = false
}
object CoqRetractAction extends CoqRetractAction { }

class CoqStepAction extends KAction {
  override def doit () : Unit = {
    //Console.println("run called, sending a command")
    val con = EclipseBoilerPlate.getContent
    DocumentState.totallen = con.length
    val content = con.drop(DocumentState.position)
    if (content.length > 0) {
      val eoc = CoqTop.findNextCommand(content)
      //Console.println("eoc is " + eoc)
      if (eoc > 0) {
        DocumentState.sendlen = eoc
        val cmd = content.take(eoc).trim
        Console.println("command is (" + eoc + "): " + cmd)
        EclipseBoilerPlate.startProgress
        EclipseBoilerPlate.nameProgress(cmd)
        CoqTop.writeToCoq(cmd) //sends comments over the line
      }
    }
  }
  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAction extends CoqStepAction { }

class CoqStepAllAction extends KAction {
  override def doit () : Unit = {
    //Console.println("registering CoqStepNotifier to PrintActor, now stepping")
    EclipseBoilerPlate.multistep = true
    PrintActor.register(new CoqStepNotifier())
    //we need to provoke first message to start callback loops
    CoqStepAction.doit()
  }
  override def start () : Boolean = false
  override def end () : Boolean = true
}
object CoqStepAllAction extends CoqStepAllAction { }

class CoqStepUntilAction extends KAction {
  override def doit () : Unit = {
    //need to go back one more step
    val togo = CoqTop.findPreviousCommand(EclipseBoilerPlate.getContent, EclipseBoilerPlate.getCaretPosition + 2)
    //Console.println("togo is " + togo + ", curpos is " + EclipseBoilerPlate.getCaretPosition)
    if (DocumentState.position < togo) {
      EclipseBoilerPlate.multistep = true
      val coqs = new CoqStepNotifier()
      coqs.test = Some((x : Int) => x >= togo)
      PrintActor.register(coqs)
      CoqStepAction.doit()
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
    EclipseBoilerPlate.unmark
    PrintActor.deregister(CoqOutputDispatcher)
    CoqStartUp.start
  }
  override def start () : Boolean = false
  override def end () : Boolean = false
}
object RestartCoqAction extends RestartCoqAction { }
