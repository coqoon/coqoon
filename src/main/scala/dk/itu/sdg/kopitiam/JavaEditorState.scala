package dk.itu.sdg.kopitiam

import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.commands.{IHandler, ExecutionEvent}

class JavaEditorState(val editor : ITextEditor) extends CoqTopContainer {
  @deprecated
  type ForbiddenJavaEditor = org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  
  import org.eclipse.jdt.core.dom._
  
  import scala.collection.mutable.Stack
  private val stepsV : Stack[JavaStep] = Stack[JavaStep]()
  def steps = stepsV
  
  def getIDocument =
    editor.getDocumentProvider.getDocument(editor.getEditorInput)
  def cursorPosition : Int = {
    import org.eclipse.jface.text.ITextSelection
    val selection = editor.getSelectionProvider.getSelection
    if (selection != null && selection.isInstanceOf[ITextSelection]) {
      selection.asInstanceOf[ITextSelection].getOffset
    } else 0
  }
  
  import org.eclipse.ui.handlers.IHandlerService
  def getHandlerService = editor.getSite.
      getService(classOf[IHandlerService]).asInstanceOf[IHandlerService]
    
  private var coqTopV : CoqTopIdeSlave_v20120710 = null
  def coqTop = {
    if (coqTopV == null)
      coqTopV = CoqTopIdeSlave_v20120710().orNull
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
  
  private var completeV : Option[ASTNode] = None
  def complete : Option[ASTNode] = completeV
  def setComplete(a : Option[ASTNode]) = {
    completeV = a
    addAnnotations(complete, underway)
  }
  
  private var underwayV : Option[ASTNode] = None
  def underway : Option[ASTNode] = underwayV
  def setUnderway(a : Option[ASTNode]) = {
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
    editor.asInstanceOf[ForbiddenJavaEditor].
        getViewer.invalidateTextPresentation /* XXX */
  }
  
  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.Annotation
  private def addAnnotations(
      complete : Option[ASTNode], underway : Option[ASTNode]) : Unit =
    doConnectedToAnnotationModel { addAnnotations(complete, underway, _) }
  
  private var annotationPair : (Option[Annotation], Option[Annotation]) =
      (None, None)
  
  private def addAnnotations(
      complete : Option[ASTNode], underway : Option[ASTNode],
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
    import org.eclipse.jface.text.source.IAnnotationModelExtension
    val modelEx = model.asInstanceOf[IAnnotationModelExtension]
    var remainingA = completedA.clone
    completedMethods.map(a => {
      val pos = new Position(a.getStartPosition, a.getLength)
      completedA.get(a) match {
        case Some(ann) =>
          modelEx.modifyAnnotationPosition(ann, pos)
        case None =>
          val ann = new Annotation(
              "dk.itu.sdg.kopitiam.provenannotation", false, "Proven Method")
          completedA.put(a, ann)
          model.addAnnotation(ann, pos)
      }
      remainingA -= a
    })
    remainingA.map(a => {
      completedA.remove(a._1)
      model.removeAnnotation(a._2)
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
  
  def updateASTifValid (off : Int) = {
    val prov = editor.getDocumentProvider
    val doc = prov.getDocument(editor.getEditorInput)
    val bla = EclipseJavaHelper.getRoot(editor.getEditorInput)
    val cu = EclipseJavaHelper.getCompilationUnit(bla)
    if (CoreJavaChecker.checkAST(this, cu, doc)) {
      if (EclipseJavaHelper.walkAST(this, cu, doc)) {
    	setCompilationUnit(Some(cu))
        val node = EclipseJavaHelper.findASTNode(cu, off, 0)
        val oldm = method
        val md = EclipseJavaHelper.findMethod(node)
        md match {
          case None =>
          case Some(x) => setMethod(Some(x))
        }
    	
    	val newSteps = JavaStepForwardHandler.collectProofScript(
    	    this, true, Some(node.getStartPosition))
    	steps.clear
    	steps.pushAll(newSteps)
    	
    	//adjustprovenmethods(cu)
    	setUnderway(Some(steps.top.node))
    	setComplete(Some(steps.top.node))
      }
    }
  }  
  
  import org.eclipse.jface.text.reconciler.MonoReconciler
  private val reconciler =
    new MonoReconciler(new JavaEditorReconcilingStrategy(this), true)
  reconciler.setDelay(1)
  reconciler.install(editor.asInstanceOf[ForbiddenJavaEditor].getViewer)
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

import org.eclipse.jface.text.reconciler.IReconcilingStrategy
private class JavaEditorReconcilingStrategy(
    jes : JavaEditorState) extends IReconcilingStrategy {
  import org.eclipse.jface.text.{IRegion, Region, IDocument}
  import org.eclipse.jface.text.reconciler.DirtyRegion
  
  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.core.resources.{IMarker,IResource}
  
  override def reconcile(r : IRegion) : Unit = {
    if (jes.method == None)
      return
    
    val input = jes.editor.getEditorInput
    
    if (input != null && input.isInstanceOf[IFileEditorInput]) {
      val file = input.asInstanceOf[IFileEditorInput].getFile()
      if (file.findMarkers(
          IMarker.PROBLEM, true, IResource.DEPTH_ZERO).length > 0)
        new DeleteErrorMarkersJob(
            file, IMarker.PROBLEM, true, IResource.DEPTH_ZERO).schedule
    }

    val off = r.getOffset
    val node = EclipseJavaHelper.findASTNode(jes.method.orNull, off, 0)
    println("Println debugging is great, " + node)

    node match {
      case e: org.eclipse.jdt.core.dom.EmptyStatement =>

        val underwayOffset =
          jes.underway.map(a => a.getStartPosition + a.getLength).getOrElse { Int.MinValue }

        if (off < underwayOffset) {
          if (jes.busy)
            jes.coqTop.interrupt
          val completeOffset =
            jes.complete.map(a => a.getStartPosition + a.getLength).getOrElse { Int.MinValue }
          if (off < completeOffset)
            UIUtils.asyncExec {
              JavaEditorHandler.doStepBack(jes, _.prefixLength(
                a => (off < (a.node.getStartPosition + a.node.getLength))))
            }
        }

        jes.updateASTifValid(off)
        
      case _ =>
        UIUtils.asyncExec {
          JavaEditorHandler.doStepBack(jes, _.length)
          jes.setMethod(None)
          jes.completedMethods = List()
          jes.annotateCompletedMethods
        }
    }
  }
  
  override def reconcile(dr : DirtyRegion, r : IRegion) = reconcile(r)
  
  override def setDocument(newDocument : IDocument) = ()
}