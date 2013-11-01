/* EditorHandler.scala
 * Command handler base classes and common command handlers
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import dk.itu.coqoon.ui
import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.coqtop.{CoqTypes, CoqTopIdeSlave_v20120710}
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

import org.eclipse.ui.{ISources, IEditorPart}
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  setBaseEnabled(false)

  private var editorV : IEditorPart = null
  protected def editor = editorV

  override protected def setEnabled(evaluationContext : Object) = {
    val activeEditor = TryCast[IEvaluationContext](evaluationContext) match {
      case Some(e) => e.getVariable(ISources.ACTIVE_EDITOR_NAME)
      case _ => UIUtils.getWorkbench.getActiveWorkbenchWindow.
          getActivePage.getActiveEditor
    }
    val oldEditor = editorV
    editorV = TryCast[IEditorPart](activeEditor).orNull
    if (oldEditor != editorV)
      editorChanged(oldEditor, editorV)
    setBaseEnabled(editorV != null && calculateEnabled)
  }

  protected def editorChanged(o : IEditorPart, n : IEditorPart) = ()

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
  import org.eclipse.ui.IFileEditorInput
  import org.eclipse.ui.texteditor.ITextEditor

  def file = TryCast[IFileEditorInput](editor.getEditorInput).map(_.getFile)
  def editor : ITextEditor
  def document : IDocument =
    editor.getDocumentProvider.getDocument(editor.getEditorInput)
  def cursorPosition : Int = editor.getSelectionProvider.
      getSelection.asInstanceOf[ITextSelection].getOffset

  import org.eclipse.jface.text.source.IAnnotationModel
  protected def doConnectedToAnnotationModel(f : IAnnotationModel => Unit) = {
    val doc = document
    Option(editor.getDocumentProvider.getAnnotationModel(
        editor.getEditorInput)).foreach(model => {
      model.connect(doc)
      try {
        f(model)
      } finally model.disconnect(doc)
      invalidate()
    })
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
        ui.ManifestIdentifiers.ANNOTATION_PROCESSED, "Processed Proof")
    underwayA = _do(r._2, underwayA,
        ui.ManifestIdentifiers.ANNOTATION_PROCESSING, "Processing Proof")
  }
}
object CoqTopEditorContainer {
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

import org.eclipse.ui.menus.UIElement
import org.eclipse.ui.commands.IElementUpdater

class ToggleCoqFlagHandler extends EditorHandler with IElementUpdater {
  import CoqTypes._

  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val name = ev.getParameter(
          ui.ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME).
              split(" ").toList
      getCoqTopContainer.coqTop.getOptionValue(name) match {
        case Some(BoolValue(v)) => scheduleJob(
            new SetCoqOptionJob(name, BoolValue(!v), getCoqTopContainer))
        case _ =>
      }
    }
    null
  }

  /* The states of the UI elements associated with this handler must be updated
   * whenever the active editor changes */
  override protected def editorChanged(o : IEditorPart, n : IEditorPart) =
    TryAdapt[CoqTopContainer](n).foreach(_ => UIUtils.refreshElements(
        ui.ManifestIdentifiers.COMMAND_TOGGLE_COQ_FLAG))

  override def updateElement(
      element : UIElement, map : java.util.Map[_, _]) = {
    TryCast[String](map.get(
        ui.ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME)) match {
      case Some(name_) =>
        val name = name_.split(" ").toList
        getCoqTopContainer.coqTop.getOptionValue(name) match {
          case Some(BoolValue(v)) => element.setChecked(v)
          case _ =>
        }
      case _ =>
    }
  }
}