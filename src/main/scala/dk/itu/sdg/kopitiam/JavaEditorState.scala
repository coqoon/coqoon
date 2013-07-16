package dk.itu.sdg.kopitiam

import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.commands.{IHandler, ExecutionEvent}

class JavaEditorState(val editor : ITextEditor) extends CoqTopEditorContainer {
  @deprecated
  type ForbiddenJavaEditor = org.eclipse.jdt.internal.ui.javaeditor.JavaEditor
  
  import org.eclipse.jdt.core.dom._
  
  import scala.collection.mutable.Stack
  private val stepsV : Stack[JavaStep] = Stack[JavaStep]()
  def steps = stepsV
  
  import org.eclipse.ui.handlers.IHandlerService
  def getHandlerService = UIUtils.getWorkbench.
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
      setGoals(None)
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
    val doc = document
    val model =
      editor.getDocumentProvider.getAnnotationModel(editor.getEditorInput)
    model.connect(doc)
    try {
      f(model)
    } finally model.disconnect(doc)
    editor.asInstanceOf[ForbiddenJavaEditor].
        getViewer.invalidateTextPresentation /* XXX */
  }
  
  private def addAnnotations(
      complete : Option[ASTNode], underway : Option[ASTNode]) : Unit =
    doConnectedToAnnotationModel { addAnnotations(complete, underway, _) }
  
  private def addAnnotations(
      complete : Option[ASTNode], underway : Option[ASTNode],
      model : IAnnotationModel) : Unit =
    doSplitAnnotations(
        JavaEditorState.getSplitAnnotationRanges(
            method.map(a => a.getStartPosition),
            complete.map(a => a.getStartPosition + a.getLength),
            underway.map(a => a.getStartPosition + a.getLength)), model)
  
  import org.eclipse.core.resources.IMarker
  
  var completedMethods : List[MethodDeclaration] = List()
  
  def markCompletedMethods : Unit = {
    import org.eclipse.ui.IFileEditorInput
    import org.eclipse.core.resources.IResource
    val input = editor.getEditorInput.asInstanceOf[IFileEditorInput].getFile
    new DeleteMarkersJob(input, "dk.itu.sdg.kopitiam.provenmarker",
        true, IResource.DEPTH_ZERO).schedule
    completedMethods.map(a =>
      new CreateMarkerJob(input,
          (a.getStartPosition, a.getStartPosition + a.getLength),
          "Proven:\n\n" + JavaEditorState.getProofScript(a).mkString("\n"),
          "dk.itu.sdg.kopitiam.provenmarker", IMarker.SEVERITY_ERROR).schedule)
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
        setMethod(EclipseJavaHelper.findMethod(node))
    	
    	val newSteps = JavaStepForwardHandler.collectProofScript(
    	    method.get, true, None,
    	    complete.map(a => a.getStartPosition + a.getLength).
    	        orElse(Some(Int.MinValue)))
    	steps.clear
    	steps.pushAll(newSteps)
    	
    	val newCompletedMethods =
    	  for (i <- completedMethods;
    	       j <- Seq(cu.findDeclaringNode(i.resolveBinding.getKey))
    	       if j != null && j.isInstanceOf[MethodDeclaration])
    	    yield j.asInstanceOf[MethodDeclaration]
    	val update = (completedMethods.size != newCompletedMethods.size)
    	
    	UIUtils.asyncExec {
    	  setUnderway(Some(steps.top.node))
    	  completedMethods = newCompletedMethods
    	  if (update) /* XXX: is this test good enough? */
    	    markCompletedMethods
    	}
      }
    }
  }  
  
  import org.eclipse.jface.text.reconciler.MonoReconciler
  private val reconciler =
    new MonoReconciler(new JavaEditorReconcilingStrategy(this), true)
  reconciler.setDelay(1)
  reconciler.install(editor.asInstanceOf[ForbiddenJavaEditor].getViewer)
  
  def createCertificate =
    JavaEditorState.createCertificate(compilationUnit.get)
}
object JavaEditorState {
  private val states =
    scala.collection.mutable.HashMap[ITextEditor, JavaEditorState]()
  def requireStateFor(part : ITextEditor) =
    states.getOrElseUpdate(part, { new JavaEditorState(part) })
  
  import org.eclipse.jface.text.Position
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
  
  import org.eclipse.jdt.core.dom.CompilationUnit
  
  def createCertificate(cu : CompilationUnit) = {
    import EclipseJavaASTProperties._
    (getDefinition(cu).get ++ getSpecification(cu).get ++
        (JavaASTUtils.traverseCU(cu, getProofScript).flatten) :+
        getEnd(cu).get).mkString("\n")
  }
  
  import org.eclipse.jdt.core.dom.MethodDeclaration
  
  def getProofScript(m : MethodDeclaration) =
    EclipseJavaASTProperties.getProof(m).get ++ JavaASTUtils.traverseAST(
        m, true, false, JavaASTUtils.printProofScript) :+ "Qed."
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
          KopitiamMarkers.Problem.ID, true, IResource.DEPTH_ZERO).length > 0)
        new DeleteMarkersJob(file,
            KopitiamMarkers.Problem.ID, true, IResource.DEPTH_ZERO).schedule
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
          jes.coqTop.kill /* XXX: can't use a step back job (goal update) */
          jes.setMethod(None)
          jes.completedMethods = List()
          jes.markCompletedMethods
        }
    }
  }
  
  override def reconcile(dr : DirtyRegion, r : IRegion) = reconcile(r)
  
  override def setDocument(newDocument : IDocument) = ()
}