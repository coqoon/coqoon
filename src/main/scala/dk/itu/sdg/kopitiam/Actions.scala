/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.ISources
import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.commands.{IHandler,AbstractHandler,ExecutionEvent}
import org.eclipse.core.expressions.IEvaluationContext

abstract class KAction extends AbstractHandler {
  protected var editor : ITextEditor = null
  
  protected def getState : JavaEditorState =
    JavaEditorState.requireStateFor(editor)
  
  override def setEnabled(evaluationContext : Object) = {
    val activeEditor = if (evaluationContext != null) {
      evaluationContext.asInstanceOf[IEvaluationContext].getVariable(
          ISources.ACTIVE_EDITOR_NAME)
    } else org.eclipse.ui.PlatformUI.getWorkbench().
        getActiveWorkbenchWindow().getActivePage().getActiveEditor()
    if (activeEditor != null && activeEditor.isInstanceOf[ITextEditor]) {
      editor = activeEditor.asInstanceOf[ITextEditor]
      setBaseEnabled(calculateEnabled)
    } else setBaseEnabled(false)
  }
  
  def calculateEnabled : Boolean = true
  
  override def isHandled () : Boolean = true
}

import org.eclipse.ui.IEditorPart

class JavaEditorState(val editor : ITextEditor) extends CoqTopContainer {
  import org.eclipse.jdt.core.dom._
  
  def getIDocument =
    editor.getDocumentProvider.getDocument(editor.getEditorInput)
  
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
  
  private var completeV : Option[Statement] = None
  def complete : Option[Statement] = completeV
  def setComplete(a : Option[Statement]) = (completeV = a)
  
  private var underwayV : Option[Statement] = None
  def underway : Option[Statement] = underwayV
  def setUnderway(a : Option[Statement]) = (underwayV = a)
  
  var completedMethods : List[MethodDeclaration] = List()
}
object JavaEditorState {
  private val states =
    scala.collection.mutable.HashMap[ITextEditor, JavaEditorState]()
  def requireStateFor(part : ITextEditor) =
    states.getOrElseUpdate(part, { new JavaEditorState(part) })
}

import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.runtime.IAdapterFactory
class JavaEditorStateFactory extends IAdapterFactory {
  override def getAdapterList = Array(classOf[CoqTopContainer])
  override def getAdapter(a : Any, klass : Class[_]) = {
    if (a.isInstanceOf[ITextEditor] && klass == classOf[CoqTopContainer]) {
      JavaEditorState.requireStateFor(a.asInstanceOf[ITextEditor])
    } else null
  }
}

class ProveMethodAction extends KAction
    with EclipseJavaHelper
    with CoreJavaChecker with org.eclipse.ui.IEditorActionDelegate {
  import org.eclipse.ui.IEditorPart
  
  import org.eclipse.jface.action.IAction
  import org.eclipse.jface.viewers.ISelection
  override def run(a : IAction) = execute(null)
  override def setActiveEditor(a : IAction, b : IEditorPart) = {
    editor = b.asInstanceOf[ITextEditor]
  }
  override def selectionChanged(a : IAction, b : ISelection) = ()
  
  import org.eclipse.jface.text.ITextSelection
  import org.eclipse.ui.part.FileEditorInput
  import org.eclipse.core.resources.IMarker
  override def execute (ev : ExecutionEvent) : Object = {
    if (isEnabled()) {
      //plan:
      // a: get project
      val jes = getState
      val prov = editor.getDocumentProvider
      val doc = prov.getDocument(editor.getEditorInput)
      val bla = getRoot(editor.getEditorInput)
      val cu = getCompilationUnit(bla)
      jes.setCompilationUnit(Some(cu))
      jes.method.map(_ => { jes.setUnderway(None); jes.setMethod(None) })
      // a': CoreJava checking!
      if (checkAST(jes, cu, doc)) { //no errors!
        // b: if outdated coqString: translate -- need to verify outdated...
        // c: find method and statement we want to prove
        if (walkAST(jes, cu, doc)) { //no errors!
          val selection = editor.getSelectionProvider.getSelection.asInstanceOf[ITextSelection]
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
}

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
          val pdef = EclipseJavaASTProperties.getDefinition(x).get
          val spec = EclipseJavaASTProperties.getSpecification(x).get
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
      import org.eclipse.ui.handlers.IHandlerService
      val ihs_ = jes.editor.getSite.getService(classOf[IHandlerService])
      val ihs = ihs_.asInstanceOf[IHandlerService]
      ihs.activateHandler("Kopitiam.step_forward", new JavaStepForwardHandler)
      Status.OK_STATUS
    } finally monitor_.done
  }
}

class JavaStepForwardHandler
    extends KAction with EclipseJavaHelper with JavaASTUtils {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val jes = getState

      var captureNext: Boolean = (jes.complete == None)

      import org.eclipse.jdt.core.dom.Statement
      
      val print: Statement => Option[String] = x =>
        if (captureNext) {
          val ps = printProofScript(jes.getIDocument, x)
          ps match {
            case None => None
            case Some(ps) =>
              jes.setUnderway(Some(x))
              Some(ps)
          }
        } else {
          if (jes.complete.get == x)
            captureNext = true
          None
        }

      traverseAST(jes.method.get, true, true, print) match {
        case a : List[String] if a.size == 1 =>
          jes.coqTop.interp(false, false, a.head) match {
            case CoqTypes.Good(msg) =>
              jes.setComplete(jes.underway)
            case CoqTypes.Fail((position, msg)) =>
              jes.setUnderway(jes.complete)
            case CoqTypes.Unsafe(msg) =>
              println("I have no idea " + msg)
          }
        case _ => None
      }
      
      jes.coqTop.goals match {
        case CoqTypes.Good(goals) =>
          jes.setGoals(goals)
        case _ => jes.setGoals(None)
      }
    }
    null
  }
}