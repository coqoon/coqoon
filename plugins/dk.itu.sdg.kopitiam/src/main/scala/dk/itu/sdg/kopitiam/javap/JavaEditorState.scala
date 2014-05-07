package dk.itu.sdg.kopitiam.javap

import dk.itu.coqoon.ui.{
  CreateMarkerJob, DeleteMarkersJob, CoqTopContainer, CoqTopEditorContainer}
import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core
import dk.itu.coqoon.core.coqtop.CoqTopIdeSlave_v20120710
import dk.itu.coqoon.core.utilities.{TryCast, TryService}

import dk.itu.sdg.kopitiam._

import org.eclipse.ui.texteditor.ITextEditor
import org.eclipse.core.commands.{IHandler, ExecutionEvent}

class JavaEditorState(val editor : ITextEditor) extends CoqTopEditorContainer {
  type ForbiddenJavaEditor = org.eclipse.jdt.internal.ui.javaeditor.JavaEditor

  import org.eclipse.jdt.core.dom._

  import scala.collection.mutable.Stack
  private val stepsV : Stack[JavaStep] = Stack()
  override def steps = stepsV

  import org.eclipse.ui.handlers.IHandlerService
  def getHandlerService = TryService[IHandlerService](UIUtils.getWorkbench).get

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
  def complete : Option[Int] =
    completeV.map(n => n.getStartPosition + n.getLength)
  def setComplete(a : Option[ASTNode]) = {
    completeV = a
    addAnnotations(complete, underway)
  }

  @deprecated("Dummy transitional function.", since = "always")
  def __temp_getComplete() : Option[ASTNode] = completeV

  private var underwayV : Option[ASTNode] = None
  def underway : Option[Int] =
    underwayV.map(n => n.getStartPosition + n.getLength)
  def setUnderway(a : Option[ASTNode]) = {
    underwayV = a
    (underway, complete) match {
      case (Some(un), Some(co)) if co > un =>
        completeV = underwayV
      case (None, _) =>
        completeV = underwayV
      case _ =>
    }
    addAnnotations(complete, underway)
  }

  override protected def invalidate() =
    TryCast[ForbiddenJavaEditor](editor).foreach(
        _.getViewer.invalidateTextPresentation) /* XXX */

  private def addAnnotations(
      complete : Option[Int], underway : Option[Int]) : Unit =
    doConnectedToAnnotationModel { addAnnotations(complete, underway, _) }

  import org.eclipse.jface.text.source.IAnnotationModel
  private def addAnnotations(
      complete : Option[Int], underway : Option[Int],
      model : IAnnotationModel) : Unit = {
    var startNode : Option[ASTNode] = None
    method.flatMap(EclipseJavaASTProperties.getQuantification) match {
      case n @ Some(q) => startNode = n
      case _ => method.flatMap(EclipseJavaASTProperties.getPrecondition) match {
        case n @ Some(p) => startNode = n
        case _ => method.flatMap(EclipseJavaASTProperties.getPostcondition) match {
          case n @ Some(p) => startNode = n
          case _ => startNode = method
        }
      }
    }
    doSplitAnnotations(CoqTopEditorContainer.getSplitAnnotationRanges(
        startNode.map(a => a.getStartPosition), complete, underway), model)
  }

  import org.eclipse.core.resources.IMarker

  var completedMethods : List[MethodDeclaration] = List()

  def markCompletedMethods : Unit = {
    import org.eclipse.ui.IFileEditorInput
    import org.eclipse.core.resources.IResource
    val input = editor.getEditorInput.asInstanceOf[IFileEditorInput].getFile
    new DeleteMarkersJob(input, ManifestIdentifiers.MARKER_PROVEN,
        true, IResource.DEPTH_ZERO).schedule
    completedMethods.map(a =>
      new CreateMarkerJob(input,
          (a.getStartPosition, a.getStartPosition + a.getLength),
          "Proven:\n\n" + JavaEditorState.getProofScript(a).mkString("\n"),
          ManifestIdentifiers.MARKER_PROVEN, IMarker.SEVERITY_ERROR).schedule)
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
    	    complete.orElse(Some(Int.MinValue)))
    	steps.clear
    	steps.pushAll(newSteps)
    	
    	val newCompletedMethods =
    	  for (i <- completedMethods;
    	       j <- TryCast[MethodDeclaration](
    	           cu.findDeclaringNode(i.resolveBinding.getKey)))
    	    yield j
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
        m, true, false,
        n => JavaASTUtils.printProofScript(n).headOption.map(
            _._1.toString)) :+ "Qed."
}

import org.eclipse.core.runtime.IAdapterFactory
class JavaEditorStateFactory extends IAdapterFactory {
  override def getAdapterList = Array(classOf[CoqTopContainer])
  override def getAdapter(a : Any, klass : Class[_]) =
      TryCast[ITextEditor](a) match {
    case Some(a) if klass == classOf[CoqTopContainer] =>
      JavaEditorState.requireStateFor(a)
    case _ => null
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

    jes.file.foreach(file => {
      if (file.findMarkers(core.ManifestIdentifiers.MARKER_PROBLEM,
          true, IResource.DEPTH_ZERO).length > 0)
        new DeleteMarkersJob(file, core.ManifestIdentifiers.MARKER_PROBLEM,
            true, IResource.DEPTH_ZERO).schedule
    })

    val off = r.getOffset
    val node = EclipseJavaHelper.findASTNode(jes.method.orNull, off, 0)
    println("Println debugging is great, " + node)

    node match {
      case e: org.eclipse.jdt.core.dom.EmptyStatement =>

        val underwayOffset = jes.underway.getOrElse(Int.MinValue)

        if (off < underwayOffset) {
          if (jes.busy)
            jes.coqTop.interrupt
          val completeOffset = jes.complete.getOrElse(Int.MinValue)
          if (off < completeOffset)
            UIUtils.exec {
              JavaEditorHandler.doStepBack(jes, _.prefixLength(
                a => (off < (a.node.getStartPosition + a.node.getLength))))
            }
        }

        jes.updateASTifValid(off)

      case _ =>
        UIUtils.exec {
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
