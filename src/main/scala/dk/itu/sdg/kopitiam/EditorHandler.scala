/* EditorHandler.scala
 * Command handler base classes and common command handlers
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.{ISources, IEditorPart}
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  setBaseEnabled(false)
  
  private var editorV : IEditorPart = null
  protected def editor = editorV
  
  override def setEnabled(evaluationContext : Object) = {
    val activeEditor = if (evaluationContext != null) {
      evaluationContext.asInstanceOf[IEvaluationContext].getVariable(
          ISources.ACTIVE_EDITOR_NAME)
    } else org.eclipse.ui.PlatformUI.getWorkbench().
        getActiveWorkbenchWindow().getActivePage().getActiveEditor()
    if (activeEditor != null && activeEditor.isInstanceOf[IEditorPart]) {
      editorV = activeEditor.asInstanceOf[IEditorPart]
      setBaseEnabled(calculateEnabled)
    } else setBaseEnabled(false)
  }
  
  protected def getCoqTopContainer = TryAdapt[CoqTopContainer](editor).orNull
  
  import org.eclipse.core.runtime.jobs.Job
  protected def scheduleJob(j : Job) = {
    getCoqTopContainer.setBusy(true)
    j.schedule
  }
  
  def calculateEnabled : Boolean = true
}

trait CoqTopContainer {
  private val lock = new Object
  
  def coqTop : CoqTopIdeSlave_v20120710
  
  private var goals_ : Option[CoqTypes.goals] = None
  def goals = lock synchronized { goals_ }
  def setGoals(g : Option[CoqTypes.goals]) = {
    lock synchronized { goals_ = g }
    fireChange(CoqTopContainer.PROPERTY_GOALS)
  }
  
  import org.eclipse.ui.IPropertyListener
  private var listeners = Set[IPropertyListener]()
  def addListener(l : IPropertyListener) = lock synchronized (listeners += l)
  def removeListener(l : IPropertyListener) = lock synchronized (listeners -= l)
  def fireChange(propertyID : Int) : Unit =
    lock synchronized { listeners }.map(_.propertyChanged(this, propertyID))
  
  private var busy_ : Boolean = false
  def busy : Boolean = lock synchronized { busy_ }
  def setBusy(b : Boolean) : Unit = {
    lock synchronized { busy_ = b }
    fireChange(CoqTopContainer.PROPERTY_BUSY)
  }
  
  private var flags = Set[String]()
  def setFlag(name : String) = lock synchronized { flags += name }
  def clearFlag(name : String) = lock synchronized { flags -= name }
  def testFlag(name : String) = lock synchronized { flags.contains(name) }
  def clearFlags = lock synchronized { flags = Set() }
}
object CoqTopContainer {
  final val PROPERTY_BUSY = 979
  final val PROPERTY_GOALS = 1979
}

trait CoqTopEditorContainer extends CoqTopContainer {
  import org.eclipse.jface.text.{Position, IDocument, ITextSelection}
  import org.eclipse.jface.text.source.{
    Annotation, IAnnotationModel, IAnnotationModelExtension}
  import org.eclipse.ui.texteditor.ITextEditor
  
  def editor : ITextEditor
  def document : IDocument =
    editor.getDocumentProvider.getDocument(editor.getEditorInput)
  def cursorPosition : Int = editor.getSelectionProvider.
      getSelection.asInstanceOf[ITextSelection].getOffset

  import org.eclipse.jface.text.source.IAnnotationModel
  protected def doConnectedToAnnotationModel(f : IAnnotationModel => Unit) = {
    val doc = document
    val model =
      editor.getDocumentProvider.getAnnotationModel(editor.getEditorInput)
    model.connect(doc)
    try {
      f(model)
    } finally model.disconnect(doc)
    invalidate()
  }
  
  protected def invalidate()
  
  var underwayA : Option[Annotation] = None
  var completeA : Option[Annotation] = None

  protected def doSplitAnnotations(r : (Option[Position], Option[Position]),
      model : IAnnotationModel) = {
    val modelEx = TryCast[IAnnotationModelExtension](model)
    def _do(p : Option[Position], a : Option[Annotation],
        aType : String, aText : String) : Option[Annotation] = (p, a) match {
      case (Some(r), None) =>
        val an = new Annotation(aType, false, aText)
        model.addAnnotation(an, r)
        Some(an)
      case (Some(r), Some(an)) =>
        modelEx.foreach(_.modifyAnnotationPosition(an, r))
        Some(an)
      case (None, _) =>
        a.map(model.removeAnnotation)
        None
    }
    completeA = _do(r._1, completeA,
        "dk.itu.sdg.kopitiam.processed", "Processed Proof")
    underwayA = _do(r._2, underwayA,
        "dk.itu.sdg.kopitiam.processing", "Processing Proof")
  }
}

import org.eclipse.core.commands.ExecutionEvent

class InterruptCoqHandler extends EditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      getCoqTopContainer.coqTop.interrupt
    null
  }
  
  override def calculateEnabled =
    (getCoqTopContainer != null && getCoqTopContainer.busy)
}