/* EditorHandler.scala
 * Command handler base classes and common command handlers
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.coqtop.{CoqTypes, CoqTopIdeSlave_v20120710}
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

import org.eclipse.ui.{ISources, IEditorPart}
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  setBaseEnabled(false)

  private var editorV : Option[IEditorPart] = None
  protected def getEditor() = editorV

  override protected def setEnabled(evaluationContext : Object) = {
    val activeEditor = TryCast[IEvaluationContext](evaluationContext) match {
      case Some(e) =>
        TryCast[IEditorPart](e.getVariable(ISources.ACTIVE_EDITOR_NAME))
      case _ =>
        Option(UIUtils.getWorkbench.
            getActiveWorkbenchWindow.getActivePage.getActiveEditor)
    }
    val oldEditor = editorV
    editorV = activeEditor
    if (oldEditor != editorV)
      editorChanged(oldEditor, editorV)
    setBaseEnabled(editorV != null && calculateEnabled)
  }

  protected def editorChanged(
      o : Option[IEditorPart], n : Option[IEditorPart]) = ()

  def calculateEnabled() : Boolean = true
}

abstract class CoqTopEditorHandler extends EditorHandler {
  protected def getCoqTopContainer =
    getEditor.flatMap(TryAdapt[CoqTopContainer])

  import org.eclipse.core.runtime.jobs.Job
  protected def scheduleJob(j : Job) = {
    getCoqTopContainer.foreach(_.setBusy(true))
    j.schedule
  }
}

import org.eclipse.core.commands.ExecutionEvent

class InterruptCoqHandler extends CoqTopEditorHandler {
  override def execute(ev : ExecutionEvent) = {
    if (isEnabled())
      getCoqTopContainer.foreach(_.coqTop.interrupt)
    null
  }

  override def calculateEnabled = getCoqTopContainer.exists(_.busy)
}

import org.eclipse.ui.menus.UIElement
import org.eclipse.ui.commands.IElementUpdater

class ToggleCoqFlagHandler extends CoqTopEditorHandler with IElementUpdater {
  import CoqTypes._

  override def execute(ev : ExecutionEvent) = {
    if (isEnabled()) {
      val name = ev.getParameter(
          ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME).
              split(" ").toList
      getCoqTopContainer.flatMap(_.coqTop.getOptionValue(name)) match {
        case Some(BoolValue(v)) => scheduleJob(
            new SetCoqOptionJob(name, BoolValue(!v), getCoqTopContainer.get))
        case _ =>
      }
    }
    null
  }

  /* The states of the UI elements associated with this handler must be updated
   * whenever the active editor changes */
  override protected def editorChanged(
      o : Option[IEditorPart], n : Option[IEditorPart]) =
    if (n != None)
      UIUtils.refreshElements(ManifestIdentifiers.COMMAND_TOGGLE_COQ_FLAG)

  override def updateElement(
      element : UIElement, map : java.util.Map[_, _]) = {
    TryCast[String](map.get(
        ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME)) match {
      case Some(name_) =>
        val name = name_.split(" ").toList
        getCoqTopContainer.flatMap(_.coqTop.getOptionValue(name)) match {
          case Some(BoolValue(v)) => element.setChecked(v)
          case _ =>
        }
      case _ =>
    }
  }
}
