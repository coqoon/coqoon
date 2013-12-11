package dk.itu.coqoon.ui

import org.eclipse.jface.text.{IDocument, TextUtilities => TU, DocumentCommand,
  IAutoEditStrategy, DefaultIndentLineAutoEditStrategy}

class CoqAutoEditStrategy extends IAutoEditStrategy {
  val indent = new DefaultIndentLineAutoEditStrategy

  override def customizeDocumentCommand(
      d : IDocument, c : DocumentCommand) = {
    indent.customizeDocumentCommand(d, c)
  }
}
