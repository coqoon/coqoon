/* Navigation.scala
 * Coq editor handlers for finding and opening Coq model objects
 * Copyright © 2013, 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.jface.text.source.ISourceViewer

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}

class OpenDeclarationHandler extends EditorHandler {
  import CoqWordDetector._
  import OpenDeclarationHandler._

  private def getViewer() = adaptEditor[ISourceViewer]

  override def calculateEnabled = (getViewer != None)

  override def execute(ev : ExecutionEvent) : AnyRef = {
    getViewer.foreach(viewer => {
      adaptEditor[BaseCoqEditor] match {
        case Some(ed) =>
          ed.getWorkingCopy.get.foreach(wc => {
            val position_ = positionFromViewer(viewer)
            wc.getSentenceAt(position_).foreach {
              case sentence =>
                val position = position_ - sentence.getOffset
                for (((offset, length), i) <- sentence.getEntities
                    if position >= offset &&
                       position <= (offset + length)) {
                  i.open
                  return null
                }
            }
          })
          return null
        case None =>
      }
    })
    null
  }
}
object OpenDeclarationHandler {
  import org.eclipse.ui.IEditorPart
  import org.eclipse.ui.part.FileEditorInput
  private def fileFromEditor(e : IEditorPart) =
    TryCast[FileEditorInput](e.getEditorInput).map(_.getFile)

  import org.eclipse.jface.text.ITextSelection
  private def positionFromViewer(v : ISourceViewer) =
    v.getSelectionProvider.getSelection.asInstanceOf[ITextSelection].getOffset

  import org.eclipse.core.resources.IFile
  def openEditorOn(e : ICoqElement) =
      e.getContainingResource.flatMap(TryCast[IFile]).flatMap(resource => {
    val page = UIUtils.getWorkbench.getActiveWorkbenchWindow.getActivePage
    Option(org.eclipse.ui.ide.IDE.openEditor(page, resource, false))
  })

  def highlightElement(e : ICoqScriptElement) =
      openEditorOn(e).flatMap(TryAdapt[ISourceViewer]).foreach(viewer => {
    val padding = e.getText.takeWhile(_.isWhitespace).length
    val (start, length) =
      (e.getOffset + padding, e.getLength - padding)
    viewer.revealRange(start, length)
    viewer.setSelectedRange(start, length)
  })
}