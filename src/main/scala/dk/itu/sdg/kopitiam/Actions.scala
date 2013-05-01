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
  
  import org.eclipse.ui.handlers.IHandlerService
  def getHandlerService = editor.getSite.
      getService(classOf[IHandlerService]).asInstanceOf[IHandlerService]
    
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
  def setMethod(a : Option[MethodDeclaration]) = {
    m = a
    if (a == None) {
      setUnderway(None)
      deactivateHandlers
    }
  }
    
  private var cu : Option[CompilationUnit] = None
  def compilationUnit : Option[CompilationUnit] = cu
  def setCompilationUnit (a : Option[CompilationUnit]) = cu = a
  
  private var completeV : Option[Statement] = None
  def complete : Option[Statement] = completeV
  def setComplete(a : Option[Statement]) = {
    completeV = a
    addAnnotations(complete, underway)
  }
  
  private var underwayV : Option[Statement] = None
  def underway : Option[Statement] = underwayV
  def setUnderway(a : Option[Statement]) = {
    underwayV = a
    (underway, complete) match {
      case (Some(un), Some(co)) if co.getStartPosition > un.getStartPosition =>
        completeV = underwayV
      case (None, _) =>
        completeV = underwayV
      case _ =>
    }
    addAnnotations(complete, underway)
  }
    
  import org.eclipse.jface.text.source.IAnnotationModel
  private def doConnectedToAnnotationModel(f : IAnnotationModel => Unit) = {
    val doc = getIDocument
    val model =
      editor.getDocumentProvider.getAnnotationModel(editor.getEditorInput)
    model.connect(doc)
    try {
      f(model)
    } finally model.disconnect(doc)
  }
  
  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.Annotation
  private def addAnnotations(
      complete : Option[Statement], underway : Option[Statement]) : Unit =
    doConnectedToAnnotationModel { addAnnotations(complete, underway, _) }
  
  private var annotationPair : (Option[Annotation], Option[Annotation]) =
      (None, None)
  
  private def addAnnotations(
      complete : Option[Statement], underway : Option[Statement],
      model : IAnnotationModel) : Unit = {
    annotationPair = JavaEditorState.doSplitAnnotations(
        JavaEditorState.getSplitAnnotationRanges(
            method.map(a => a.getStartPosition),
            complete.map(a => a.getStartPosition + a.getLength),
            underway.map(a => a.getStartPosition + a.getLength)),
        annotationPair, model)
  }
  
  private var completedA =
    scala.collection.mutable.HashMap[MethodDeclaration, Annotation]()
  
  var completedMethods : List[MethodDeclaration] = List()
  
  def annotateCompletedMethods : Unit =
    doConnectedToAnnotationModel { annotateCompletedMethods(_) }
  
  def annotateCompletedMethods(model : IAnnotationModel) : Unit = {
    completedMethods.map(a => {
      completedA.get(a) match {
        case Some(ann) =>
          /* do nothing */
        case None =>
          val ann = new Annotation(
              "dk.itu.sdg.kopitiam.provenannotation", false, "Proven Method")
          completedA.put(a, ann)
          model.addAnnotation(ann, new Position(
              a.getStartPosition, a.getLength))
      }
    })
  }
  
  import org.eclipse.ui.handlers.IHandlerActivation
  private var handlerActivations : List[IHandlerActivation] = List()
  
  def activateHandler(id : String, handler : IHandler) = {
    val activation = getHandlerService.activateHandler(id, handler)
    handlerActivations :+= activation
    activation
  }
  
  def deactivateHandlers = {
    import scala.collection.JavaConversions._
    getHandlerService.deactivateHandlers(handlerActivations)
    handlerActivations = List()
  }
}
object JavaEditorState {
  private val states =
    scala.collection.mutable.HashMap[ITextEditor, JavaEditorState]()
  def requireStateFor(part : ITextEditor) =
    states.getOrElseUpdate(part, { new JavaEditorState(part) })
  
  import org.eclipse.jdt.core.dom.Statement
  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.{
    Annotation, IAnnotationModel, IAnnotationModelExtension}
  def getSplitAnnotationRanges(
      start_ : Option[Int], first_ : Option[Int], second_ : Option[Int]) = {
    val firstRange = start_.flatMap(start => first_.flatMap(first =>
        Some(new Position(start, first - start))))
    val secondRange = start_.flatMap(start => second_.flatMap(second =>
      first_ match {
        case None =>
          Some(new Position(start, second - start))
        case Some(first) if first != second =>
          Some(new Position(first, second - first))
        case _ => None
      }))
    (firstRange, secondRange)
  }
  
  def doSplitAnnotations(
      r : (Option[Position], Option[Position]),
      e : (Option[Annotation], Option[Annotation]),
      model : IAnnotationModel) :
      (Option[Annotation], Option[Annotation]) = {
    val modelEx = model.asInstanceOf[IAnnotationModelExtension]
    def _do(
        p : Option[Position], a : Option[Annotation],
        aType : String, aText : String) : Option[Annotation] = p match {
      case Some(r) => a match {
        case None =>
          val an = new Annotation(aType, false, aText)
          model.addAnnotation(an, r)
          Some(an)
        case Some(an) =>
          modelEx.modifyAnnotationPosition(an, r)
          Some(an)
      }
      case None =>
        a.map(b => model.removeAnnotation(b))
        None
    }
    (_do(r._1, e._1, "dk.itu.sdg.kopitiam.processed", "Processed Proof"),
     _do(r._2, e._2, "dk.itu.sdg.kopitiam.processing", "Processing Proof"))
  }
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