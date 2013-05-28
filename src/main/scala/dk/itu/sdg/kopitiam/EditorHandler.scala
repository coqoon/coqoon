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
  
  protected def getCoqTopContainer = {
    if (editor != null) {
      val ad = editor.getAdapter(classOf[CoqTopContainer])
      if (ad != null && ad.isInstanceOf[CoqTopContainer]) {
        ad.asInstanceOf[CoqTopContainer]
      } else null
    } else null
  }
  
  import org.eclipse.core.runtime.jobs.Job
  protected def scheduleJob(j : Job) = {
    getCoqTopContainer.setBusy(true)
    j.schedule
  }
  
  def calculateEnabled : Boolean = true
}

trait CoqTopContainer {
  def coqTop : CoqTopIdeSlave_v20120710
  
  private var goals_ : Option[CoqTypes.goals] = None
  def goals = goals_
  def setGoals(g : Option[CoqTypes.goals]) = {
    goals_ = g
    fireChange(CoqTopContainer.PROPERTY_GOALS)
  }
  
  import org.eclipse.ui.IPropertyListener
  private var listeners = Set[IPropertyListener]()
  def addListener(l : IPropertyListener) = (listeners += l)
  def removeListener(l : IPropertyListener) = (listeners -= l)
  def fireChange(propertyID : Int) : Unit =
    listeners.map(_.propertyChanged(this, propertyID))
  
  private var busy_ : Boolean = false
  def busy : Boolean = busy_
  def setBusy(b : Boolean) : Unit = {
    busy_ = b
    fireChange(CoqTopContainer.PROPERTY_BUSY)
  }
}
object CoqTopContainer {
  final val PROPERTY_BUSY = 979
  final val PROPERTY_GOALS = 1979
}

trait CoqTopEditorContainer extends CoqTopContainer {
  import org.eclipse.jface.text.{IDocument, ITextSelection}
  import org.eclipse.ui.texteditor.ITextEditor
  
  def editor : ITextEditor
  def document : IDocument =
    editor.getDocumentProvider.getDocument(editor.getEditorInput)
  def cursorPosition : Int = editor.getSelectionProvider.
      getSelection.asInstanceOf[ITextSelection].getOffset
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