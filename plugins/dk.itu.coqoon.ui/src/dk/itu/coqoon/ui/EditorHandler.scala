/* EditorHandler.scala
 * Command handler base classes and common command handlers
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

import org.eclipse.ui.{ISources, IEditorPart}
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.expressions.IEvaluationContext

abstract class EditorHandler extends AbstractHandler {
  setBaseEnabled(false)

  private var editorV : Option[IEditorPart] = None
  protected def getEditor() = editorV

  protected def adaptEditor[A](implicit arg0 : Manifest[A]) =
    getEditor.flatMap(TryAdapt[A])

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

import org.eclipse.core.commands.ExecutionEvent

class ReformatCoqHandler extends EditorHandler {
  import org.eclipse.jface.text.source.ISourceViewer
  private def getViewer() = adaptEditor[ISourceViewer]

  override def calculateEnabled = (getViewer != None)

  override def execute(ev : ExecutionEvent) = {
    getViewer.foreach(
        _.getTextOperationTarget.doOperation(ISourceViewer.FORMAT))
    null
  }
}