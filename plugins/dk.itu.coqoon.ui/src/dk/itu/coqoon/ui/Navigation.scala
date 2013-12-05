package dk.itu.coqoon.ui

import org.eclipse.ui.part.FileEditorInput
import org.eclipse.core.commands.ExecutionEvent

import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.{TryCast, TryAdapt}
import dk.itu.coqoon.core.coqtop.CoqSentence

class OpenDeclarationHandler extends EditorHandler {
  def isCoqIdentifierCharacter(c : Char) =
    c.isLetterOrDigit || c == '_' || c == '\''

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
              var result : Option[ICoqScriptGroup] = None
              f.getAncestor[ICoqProject].foreach(_.accept(_ match {
                case e : ICoqScriptGroup if result == None =>
                  e.getDisposition match {
                    case NamedCoqGroup(id) if id == identifier =>
                      result = Some(e); false
                    case _ => true
                  }
                case p : IParent if result == None => true
                case _ => false
              }))
              for (j <- result;
                   k <- j.getChildren.headOption;
                   l <- TryCast[ICoqScriptSentence](k))
                OpenDeclarationHandler.highlightSentence(l)
            }
          case _ =>
        }
      case _ =>
    }
    null
  }
}
object OpenDeclarationHandler {
  import org.eclipse.core.resources.IFile
  def openEditorOn(e : ICoqElement) =
      e.getContainingResource.flatMap(TryCast[IFile]).flatMap(resource => {
    val page = UIUtils.getWorkbench.getActiveWorkbenchWindow.getActivePage
    Option(org.eclipse.ui.ide.IDE.openEditor(page, resource, false))
  })

  def highlightSentence(s : ICoqScriptSentence) =
      openEditorOn(s).flatMap(TryCast[CoqEditor]).foreach(editor => {
    val t = s.getText
    val padding = t.takeWhile(_.isWhitespace).length
    val (start, length) =
      (t.start + padding, t.length - padding)
    editor.getViewer.revealRange(start, length)
    editor.getViewer.setSelectedRange(start, length)
  })
}