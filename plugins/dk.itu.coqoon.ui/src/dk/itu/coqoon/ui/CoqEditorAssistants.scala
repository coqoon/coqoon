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

import org.eclipse.jface.text.quickassist.{
  IQuickAssistProcessor, IQuickAssistInvocationContext}
import org.eclipse.jface.text.contentassist.ICompletionProposal

class CoqQuickAssistProcessor extends IQuickAssistProcessor {
  import org.eclipse.jface.text.source.Annotation

  override def canAssist(context : IQuickAssistInvocationContext) = {
    println(this + ".canAssist(" + context + ")"); false
  }
  override def canFix(i : Annotation) = {
    println(this + ".canFix(" + i + ")"); false
  }
  override def computeQuickAssistProposals(
      context : IQuickAssistInvocationContext) : Array[ICompletionProposal] = {
    Array()
  }
  override def getErrorMessage = null
}

import utilities.UIUtils
import org.eclipse.ui.{IMarkerResolution, IMarkerResolutionGenerator}
import org.eclipse.core.resources.IMarker

object ConfigureCoqPathResolution extends IMarkerResolution {
  override def getLabel = "Configure the path to Coq"

  override def run(r : IMarker) = {
    import org.eclipse.ui.dialogs.PreferencesUtil
    PreferencesUtil.createPreferenceDialogOn(UIUtils.getActiveShell,
      "Kopitiam.settings", null, null).open
  }
}

class ResolutionGenerator extends IMarkerResolutionGenerator {
  override def getResolutions(m : IMarker) = Array(ConfigureCoqPathResolution)
}
