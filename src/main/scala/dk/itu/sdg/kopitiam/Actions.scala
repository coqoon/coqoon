/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.core.commands.{IHandler,ExecutionEvent}

abstract class KAction extends IHandler {
  import org.eclipse.ui.IWorkbenchWindow
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  import org.eclipse.core.commands.IHandlerListener

  override def dispose () : Unit = ()

  //I've no clue why this code is around and whether it is useful at all
  private var handlers : List[IHandlerListener] = List[IHandlerListener]()
  override def addHandlerListener (h : IHandlerListener) : Unit = { handlers ::= h }
  override def removeHandlerListener (h : IHandlerListener) : Unit =
    { handlers = handlers.filterNot(_ == h) }

  override def execute (ev : ExecutionEvent) : Object = { doit; null }

  override def isEnabled () : Boolean = true
  override def isHandled () : Boolean = true
  def doit () : Unit
}

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

class ProveMethodAction extends KAction with EclipseJavaHelper with CoreJavaChecker {
  val editor : org.eclipse.ui.IEditorPart = null
  
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.text.ITextSelection
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.IMarker
  override def execute (ev : ExecutionEvent) : Object = {
    if (! DocumentState.readyForInput)
      EclipseBoilerPlate.warnUser("Not ready yet", "Sorry, Coq interaction is active. Maybe it is doing a Qed.")
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
        val need = (JavaPosition.editor != null)
        if (need) {
          JavaPosition.retract
          JavaPosition.unmarkProofs
        }
        JavaPosition.editor = edi
        if (need)
          ()//CoqRetractAction.doitH
      }
      JavaPosition.unmark
      // a': CoreJava checking!
      checkAST(cu, doc)
      if (JavaPosition.markers.length == 0) { //no errors!
        // b: if outdated coqString: translate -- need to verify outdated...
        walkAST(cu, doc) //side effects: properties: coq output, spec ptr to method
        // c: find method and statement we want to prove
        if (JavaPosition.markers.length == 0) { //no errors!
          val selection = edi.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
          val off = selection.getOffset
          val node = findASTNode(cu, off, 0)
          val md = findMethod(node)
          md match {
            case None => EclipseBoilerPlate.warnUser("Cursor not inside of method", "Please put the cursor inside of the method to verify")
            case Some(x) =>
              if (JavaPosition.proofmarkers.contains(x.getName.getIdentifier))
                EclipseBoilerPlate.warnUser("Already proven", "Sorry, this method was already proven")
              else {
                val proj = EclipseTables.DocToProject(doc)
                proj.program = Some(cu)
                proj.proveMethod(x)
                CoqCommands.step
              }
          }
        }
      }
    }
    null
  }
  override def doit () : Unit = { }
}
object ProveMethodAction extends ProveMethodAction { }

object CoqCommands extends CoqCallback {
  private var commands : List[() => Unit] = List[() => Unit]()

  def nonempty () : Boolean = { commands.size > 0 }

  def empty () : Unit = {
    commands = List[() => Unit]()
  }

  def doLater (f : () => Unit) : Unit = {
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
        Console.println("no command to run, deregistered")
      }
  }

  private def finished () : Boolean = {
    return true
  }

  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqShellReady(monoton, token) =>
        if (monoton)
          step
        else
          empty
      case _ =>
    }
  }
}
