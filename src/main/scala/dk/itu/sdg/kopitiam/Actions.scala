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

import org.eclipse.ui.IEditorPart

class JavaEditorState(val editor : IEditorPart) extends CoqTopContainer {
  import org.eclipse.jdt.core.dom._
  
  private var coqTopV : CoqTopIdeSlave_v20120710 = null
  def coqTop = {
    if (coqTopV == null) {
      coqTopV = CoqTopIdeSlave.forVersion("20120710") match {
        case Some(m : CoqTopIdeSlave_v20120710) => m
        case _ => null
      }
    }
    coqTopV
  }
  
  private var m : Option[MethodDeclaration] = None
  def method : Option[MethodDeclaration] = m
  def setMethod(a : Option[MethodDeclaration]) = m = a
    
  private var cu : Option[CompilationUnit] = None
  def compilationUnit : Option[CompilationUnit] = cu
  def setCompilationUnit (a : Option[CompilationUnit]) = cu = a
  
  def complete : Option[ASTNode] = None
  def setComplete(a : Option[ASTNode]) = ()
  def underway : Option[ASTNode] = None
  def setUnderway(a : Option[ASTNode]) = ()
  
  var completedMethods : List[MethodDeclaration] = List()
}
object JavaEditorState {
  private val states =
    scala.collection.mutable.HashMap[IEditorPart, JavaEditorState]()
  def requireStateFor(part : IEditorPart) =
    states.getOrElseUpdate(part, { new JavaEditorState(part) })
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
      val jes = JavaEditorState.requireStateFor(edi)
      val prov = edi.getDocumentProvider
      val doc = prov.getDocument(edi.getEditorInput)
      val bla = getRoot(edi.getEditorInput)
      val cu = getCompilationUnit(bla)
      jes.setCompilationUnit(Some(cu))
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

import org.eclipse.ui.IFileEditorInput
import org.eclipse.core.runtime.{IProgressMonitor, IStatus, Status, SubMonitor}
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job

class JavaProofInitialisationJob(jes : JavaEditorState)
    extends Job("Initialising Java proof mode") {
  
  override def run(monitor_ : IProgressMonitor) =
    JavaProofInitialisationJob.run(jes, monitor_)
}
object JavaProofInitialisationJob {
  def run(jes : JavaEditorState, monitor_ : IProgressMonitor) : IStatus = {
    val monitor = SubMonitor.convert(
        monitor_, "Initialising Java proof mode", 4)
    try {
      monitor.subTask("Performing custom Coq initialisation")
      val loadp = Activator.getDefault.getPreferenceStore.getString("loadpath")
      jes.coqTop.interp(false, false, "Add LoadPath \"" + loadp + "\".")

      import org.eclipse.core.resources.IResource

      val input = jes.editor.getEditorInput
      val res: Option[IResource] =
        if (input.isInstanceOf[IFileEditorInput]) {
          Some(input.asInstanceOf[IFileEditorInput].getFile)
        } else None

      res match {
        case Some(r) =>
          jes.coqTop.interp(false, false,
            "Add Rec LoadPath \"" +
              r.getProject.getFolder("src").getLocation.toOSString + "\".")
        case None =>
          Console.println("shouldn't happen - trying to get ProjectDir from " +
            input + ", which is not an IFileEditorInput")
      }
      monitor.worked(1)

      monitor.subTask("Preparing model")
      val fei = jes.editor.getEditorInput().asInstanceOf[IFileEditorInput]
      val proj = fei.getFile.getParent
      val basename = fei.getFile.getName().dropRight(5)
      val model = proj.getFile(new Path(basename + "_model.v"))
      if (!model.exists) {
        EclipseBoilerPlate.warnUser("Model file missing",
          "Please write a model file for this Java file named '" +
            basename + "_model'.")
        return Status.OK_STATUS
      } else {
        val ccj = CoqCompileJob.run(model, monitor.newChild(1))
        if (ccj != Status.OK_STATUS)
          return ccj
      }
      monitor.setWorkRemaining(2)
      
      monitor.subTask("Setting up definitions and specification")
      //send over definition and spec
      jes.compilationUnit match {
        case Some(x) =>
          val pdef = x.getProperty(EclipseJavaASTProperties.coqDefinition).
              asInstanceOf[List[String]]
          val spec = x.getProperty(EclipseJavaASTProperties.coqSpecification).
              asInstanceOf[List[String]]
          val steps = pdef ++ spec
          val loopProgress = monitor.newChild(1,
              SubMonitor.SUPPRESS_ALL_LABELS).setWorkRemaining(steps.length)
          for (s <- pdef ++ spec) {
            jes.coqTop.interp(true, false, s)
            loopProgress.worked(1)
          }
        case None =>
      }
      
      monitor.subTask("Setting up method proof environment")
      //send over beginning of proof
      jes.method match {
        case Some(meth) =>
          val prfhead = meth.getProperty(EclipseJavaASTProperties.coqProof).
              asInstanceOf[List[String]]
          val loopProgress = monitor.newChild(1,
              SubMonitor.SUPPRESS_ALL_LABELS).setWorkRemaining(prfhead.length)
          for (s <- prfhead) {
            jes.coqTop.interp(true, false, s)
            loopProgress.worked(1)
          }
        case None =>
      }
      
      val goals = jes.coqTop.goals match {
        case CoqTypes.Good(a) => a
        case _ => None
      }
      CoqJob.asyncExec {
        jes.setGoals(goals)
      }
      //register handlers!
      Status.OK_STATUS
    } finally monitor_.done
  }
}