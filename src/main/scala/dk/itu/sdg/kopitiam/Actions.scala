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

class JavaEditorState {
  import org.eclipse.jdt.core.dom._
  
  var editor : org.eclipse.ui.IEditorPart = null
  
  var coqtop : CoqTopIdeSlave_v20120710 = null
  
  def method : Option[MethodDeclaration] = None
  def setMethod(a : Option[MethodDeclaration]) = ()
  def complete : Option[ASTNode] = None
  def setComplete(a : Option[ASTNode]) = ()
  def underway : Option[ASTNode] = None
  def setUnderway(a : Option[ASTNode]) = ()
  
  var completedMethods : List[MethodDeclaration] = null
}

class ProveMethodAction extends KAction
    with EclipseJavaHelper
    with CoreJavaChecker with org.eclipse.ui.IEditorActionDelegate {
  import org.eclipse.ui.IEditorPart
  var editor : IEditorPart = null
  
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  override def run(a : IAction) = execute(null)
  override def setActiveEditor(a : IAction, b : IEditorPart) = (editor = b)
  override def selectionChanged(a : IAction, b : ISelection) = ()
  
  import org.eclipse.jface.text.ITextSelection
  import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  import org.eclipse.ui.texteditor.ITextEditor
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.IMarker
  override def execute (ev : ExecutionEvent) : Object = {
    if (false) //! DocumentState.readyForInput)
      EclipseBoilerPlate.warnUser("Not ready yet", "Sorry, Coq interaction is active. Maybe it is doing a Qed.")
    else {
      //plan:
      // a: get project
      val edi : ITextEditor = editor.asInstanceOf[ITextEditor]
      val jes : JavaEditorState = null /* for now */
      val prov = edi.getDocumentProvider
      val doc = prov.getDocument(edi.getEditorInput)
      val bla = getRoot(edi.getEditorInput)
      val cu = getCompilationUnit(bla)
      jes.method.map(_ => { jes.setUnderway(None); jes.setMethod(None) })
      // a': CoreJava checking!
      if (checkAST(jes, cu, doc)) { //no errors!
        // b: if outdated coqString: translate -- need to verify outdated...
        // c: find method and statement we want to prove
        if (walkAST(jes, cu, doc)) { //no errors!
          val selection = edi.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
          val off = selection.getOffset
          val node = findASTNode(cu, off, 0)
          val md = findMethod(node)
          md match {
            case None => EclipseBoilerPlate.warnUser("Cursor not inside of method", "Please put the cursor inside of the method to verify")
            case Some(x) =>
              if (jes.completedMethods.contains(x))
                EclipseBoilerPlate.warnUser("Already proven", "Sorry, this method was already proven")
              else {
                jes.setMethod(Some(x))
                new JavaProofInitialisationJob(jes).schedule
                //proj.proveMethod(x)
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

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends CoqJob("Initialise Java proof mode", null) {
  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status}
  import org.eclipse.core.runtime.Path
  override def run(monitor : IProgressMonitor) : IStatus = {
    val fei = jes.editor.getEditorInput().asInstanceOf[IFileEditorInput]
    val proj = fei.getFile.getParent
    val basename = fei.getFile.getName().dropRight(5)
    val model = proj.getFile(new Path(basename + "_model.v"))
    if (!model.exists) {
      EclipseBoilerPlate.warnUser("Please write a model file for this Java file named '" + basename + "_model'.", "")
      return Status.OK_STATUS
    } else
      new CoqCompileJob(model.getProject.getLocation.toFile, model.getName, true).schedule
    //send over definition and spec
    //send over beginning of proof
    //register handlers!
    Status.OK_STATUS
  }
}