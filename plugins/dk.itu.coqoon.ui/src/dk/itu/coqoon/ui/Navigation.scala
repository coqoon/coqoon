/* Navigation.scala
 * Coq editor handlers for finding and opening Coq model objects
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.ui.part.FileEditorInput
import org.eclipse.core.commands.ExecutionEvent

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}
import dk.itu.coqoon.core.coqtop.CoqSentence

class OpenDeclarationHandler extends EditorHandler {
  import OpenDeclarationHandler._
  override def execute(ev : ExecutionEvent) : AnyRef = {
    val editor = TryCast[CoqEditor](UIUtils.getWorkbench.
        getActiveWorkbenchWindow.getActivePage.getActiveEditor)
    editor match {
      case Some(editor) =>
        editor.file.flatMap(ICoqModel.getInstance.toCoqElement) match {
          case Some(f : ICoqVernacFile) =>
            var (start, end) = (editor.cursorPosition, editor.cursorPosition)
            while (isCoqIdentifierCharacter(editor.document.getChar(start - 1)))
              start -= 1
            while (isCoqIdentifierCharacter(editor.document.getChar(end)))
              end += 1
            if (start != end) {
              val identifier = editor.document.get(start, end - start)
              var result : Option[ICoqScriptElement] = None
              f.getAncestor[ICoqProject].foreach(_.accept(_ match {
                case e : ICoqLtacSentence
                    if e.getIdentifier() == identifier =>
                  result = Some(e); false
                case e : ICoqScriptGroup if result == None =>
                  e.getDisposition match {
                    case NamedCoqGroup(id) if id == identifier =>
                      result = e.getChildren.headOption; false
                    case _ => true
                  }
                case p : IParent if result == None => true
                case _ => false
              }))
              result.foreach(highlightElement)
            }
          case _ =>
        }
      case _ =>
    }
    null
  }
}
object OpenDeclarationHandler {
  def isCoqIdentifierCharacter(c : Char) =
    c.isLetterOrDigit || c == '_' || c == '\''

  import org.eclipse.core.resources.IFile
  def openEditorOn(e : ICoqElement) =
      e.getContainingResource.flatMap(TryCast[IFile]).flatMap(resource => {
    val page = UIUtils.getWorkbench.getActiveWorkbenchWindow.getActivePage
    Option(org.eclipse.ui.ide.IDE.openEditor(page, resource, false))
  })

  def highlightElement(e : ICoqScriptElement) =
      openEditorOn(e).flatMap(TryCast[CoqEditor]).foreach(editor => {
    val padding = e.getText.takeWhile(_.isWhitespace).length
    val (start, length) =
      (e.getOffset + padding, e.getLength - padding)
    editor.getViewer.revealRange(start, length)
    editor.getViewer.setSelectedRange(start, length)
  })
}