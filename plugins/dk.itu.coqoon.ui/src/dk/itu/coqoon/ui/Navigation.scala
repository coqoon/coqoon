package dk.itu.coqoon.ui

import org.eclipse.ui.part.FileEditorInput
import org.eclipse.core.commands.ExecutionEvent
import dk.itu.coqoon.ui.utilities.UIUtils

import dk.itu.coqoon.core.model.{ICoqModel, ICoqVernacFile}
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
              import dk.itu.coqoon.core.model._
              var result : Option[ICoqScriptGroup] = None
              f.accept(_ match {
                case e : ICoqVernacFile if result == None => true
                case e : ICoqScriptGroup if result == None =>
                  e.getDisposition match {
                    case NamedCoqGroup(id) if id == identifier =>
                      result = Some(e); false
                    case _ => true
                  }
                case _ => false
              })
              import dk.itu.coqoon.core.utilities.TryCast
              for (j <- result;
                   k <- j.getChildren.headOption;
                   l <- TryCast[ICoqScriptSentence](k)) {
                val t = l.getText

                val padding = t.takeWhile(_.isWhitespace).length
                val (start, length) = (t.start + padding, t.length - padding)
                editor.getViewer.revealRange(start, length)
                editor.getViewer.setSelectedRange(start, length)

                return null
              }
            }
          case _ =>
        }
      case _ =>
    }
    null
  }
}