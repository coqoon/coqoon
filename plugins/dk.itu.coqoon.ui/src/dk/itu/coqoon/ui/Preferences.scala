/* (c) 2011 Hannes Mehnert */
/* Copyright © 2013, 2014 Alexander Faithfull */

package dk.itu.coqoon.ui

import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}
import org.eclipse.ui.IWorkbenchPreferencePage

class CoqoonColorPreferencePage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init(workbench : IWorkbench) =
    setPreferenceStore(CoqoonUIPreferences.store)

  import org.eclipse.jface.preference._
  override def createFieldEditors () : Unit = {
    import CoqoonUIPreferences._

    addField(new ColorFieldEditor(COQ_SENT_BACKGROUND,
        "Coq Sent Background", getFieldEditorParent))
    addField(new ColorFieldEditor(COQ_PROCESSING_BACKGROUND,
        "Coq Processing Background", getFieldEditorParent))
    addField(new ColorFieldEditor(KEYWORD_COLOR,
        "Keyword Foreground", getFieldEditorParent))

    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(ProcessingAnnotations.ID,
          "Enable processing annotations (PIDE)", parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Highlight commands that haven't yet been executed in the " +
          "PIDE editor.")
      ed
    })
  }
}

class CoqoonEditorPreferencePage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init(workbench : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference.ComboFieldEditor
  import org.eclipse.jface.preference.StringFieldEditor
  import org.eclipse.jface.preference.BooleanFieldEditor

  override def createFieldEditors = {
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonUIPreferences.AutomaticFormatting.ID,
          "Automatically format Coq code as you type",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Attempt to indent Coq code and close sections and proofs " +
          "automatically.")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonUIPreferences.SubstituteSequences.ID,
          "Enable automatic Unicode character substitution",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Automatically convert certain escape sequences to Unicode " +
          "characters.")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new StringFieldEditor(
          CoqoonUIPreferences.SpacesPerIndentationLevel.ID,
          "Spaces per indentation level",
          parent)
      ed.getLabelControl(parent).setToolTipText(
          "The number of spaces at each indentation level.")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonUIPreferences.UsePerspective.ID,
          "Enable perspective support (PIDE)",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Instruct Coq to only process proofs when they become visible in " +
          "the editor. (Compilation is not affected by this setting.)")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new ComboFieldEditor(
          CoqoonUIPreferences.Partitioner.ID,
          "Coq editor partitioner",
          Array[Array[String]](
              Array("Transition-based", "transition"),
              Array("Token-based", "token")),
          parent)
      ed.getLabelControl(parent).setToolTipText(
          "Override the partitioner used by the Coq editor to find " +
          "comment and string regions. (Only change " +
          "this setting if editor services like syntax highlighting seem " +
          "slow or erratic.)")
      ed
    })
  }
}

class CoqoonFoldingPreferencePage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init(workbench : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference.BooleanFieldEditor

  override def createFieldEditors = {
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonUIPreferences.Folding.ID,
          "Enable folding",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Enable folding of Coq proofs and comments.")
      ed
    })
  }
}

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

class CoqoonUIPreferences extends AbstractPreferenceInitializer {
  import CoqoonUIPreferences._

  override def initializeDefaultPreferences() = {
    import org.eclipse.swt.graphics.RGB
    import org.eclipse.core.runtime.preferences.DefaultScope
    import org.eclipse.jface.resource.StringConverter

    val node = DefaultScope.INSTANCE.getNode(ManifestIdentifiers.PLUGIN)

    node.put(COQ_SENT_BACKGROUND,
        StringConverter.asString(new RGB(118, 255, 133)))
    node.put(COQ_PROCESSING_BACKGROUND,
        StringConverter.asString(new RGB(255, 255, 0)))
    node.put(KEYWORD_COLOR,
        StringConverter.asString(new RGB(127, 6, 101)))

    node.putBoolean(MATCHING_BRACKETS, true)
    node.put(MATCHING_BRACKETS_COLOR,
        StringConverter.asString(new RGB(192, 192, 192)))

    node.putInt(SpacesPerIndentationLevel.ID, 2)
    node.putBoolean(AutomaticFormatting.ID, true)
    node.putBoolean(SubstituteSequences.ID, true)
    node.putBoolean(UsePerspective.ID, true)

    node.putBoolean(ProcessingAnnotations.ID, true)

    node.putBoolean(Folding.ID, true)

    node.put(Partitioner.ID, "transition")
  }
}
object CoqoonUIPreferences {
  val COQ_SENT_BACKGROUND = "coqSentBg"
  val COQ_PROCESSING_BACKGROUND = "dk.itu.coqoon.ui:coqProcessingBg"
  val KEYWORD_COLOR = "coqKeywordFg"
  val MATCHING_BRACKETS = "matchingBrackets"
  val MATCHING_BRACKETS_COLOR = "matchingBracketsColor"

  private[ui] def store = Activator.getDefault.getPreferenceStore

  object SpacesPerIndentationLevel {
    final val ID = "spacesPerIndentationLevel"
    def get() = store.getInt(ID)
  }

  object AutomaticFormatting {
    final val ID = "automaticFormatting"
    def get() = store.getBoolean(ID)
  }

  object SubstituteSequences {
    val ID = "substituteSequences"
    def get() = store.getBoolean(ID)
  }

  object ProcessingAnnotations {
    final val ID = "enableProcessing"
    def get() = store.getBoolean(ID)
  }

  object UsePerspective {
    val ID = "usePerspective"
    def get() = store.getBoolean(ID)
  }

  object Folding {
    val ID = "enableFolding"
    def get() = store.getBoolean(ID)
  }

  object Partitioner {
    val ID = "partitioner"
    def get() = store.getString(ID)
  }
}