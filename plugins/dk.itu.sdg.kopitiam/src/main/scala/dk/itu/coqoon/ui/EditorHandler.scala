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
          ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME).
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
        ManifestIdentifiers.COMMAND_TOGGLE_COQ_FLAG))

  override def updateElement(
      element : UIElement, map : java.util.Map[_, _]) = {
    TryCast[String](map.get(
        ManifestIdentifiers.COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME)) match {
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
