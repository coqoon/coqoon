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
      adaptEditor[pide.PIDENavigationHost] match {
        case Some(an) =>
          val position_ = positionFromViewer(viewer)
          an.findCommand(position_) match {
            case Some((offset, command)) =>
              val position = position_ - offset
              for (r @ (range, _) <- an.getEntities(command)
                   if position >= range.start && position <= range.stop) {
                an.selectEntity(r)
                return null
              }
            case None =>
          }
          return null
        case None =>
      }
      val file = getEditor.flatMap(fileFromEditor)
      file.flatMap(ICoqModel.getInstance.toCoqElement).foreach(element => {
        val document = viewer.getDocument
        val cursorPosition = positionFromViewer(viewer)
        var (start, end) = (cursorPosition, cursorPosition)
        while (isWordStart(document.getChar(start - 1)) ||
               isWordPart(document.getChar(start - 1)))
          start -= 1
        while (isWordPart(document.getChar(end)) ||
               isWordEnd(document.getChar(end)))
          end += 1
        if (start != end) {
          val identifier = document.get(start, end - start)
          var result : Option[ICoqScriptElement] = None
          element.getAncestor[ICoqProject].foreach(_.accept(_ match {
            case e : ICoqLtacSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqFixpointSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqInductiveSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqDefinitionSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqSectionStartSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqModuleStartSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case e : ICoqAssertionSentence
                if e.getIdentifier() == identifier =>
              result = Some(e); false
            case p : IParent if result == None => true
            case f => false
          }))
          result.foreach(highlightElement)
        }
      })
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