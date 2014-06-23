/* (c) 2011 Hannes Mehnert */
/* Copyright © 2013, 2014 Alexander Faithfull */

package dk.itu.coqoon.ui

import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.ui.IWorkbenchPreferencePage

class CoqoonColorPreferencePage
    extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init (workbench : IWorkbench) : Unit = {
    setPreferenceStore(Activator.getDefault.getPreferenceStore)
  }

  import org.eclipse.jface.preference.ColorFieldEditor
  override def createFieldEditors () : Unit = {
    val fields = List(
      ("coqSentBg", "Coq Sent Background"),
      ("coqSentProcessBg", "Coq Processing Background"),
      ("coqKeywordFg", "Keyword Foreground")
    )
    for ((pref, label) <- fields)
      addField(new ColorFieldEditor(pref, label, getFieldEditorParent))
 }
}

class CoqoonFormattingPreferencePage
    extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init(workbench : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference.StringFieldEditor

  import CoqoonUIPreferences._
  override def createFieldEditors = {
    addField(new StringFieldEditor(SpacesPerIndentationLevel.ID,
        "Spaces per indentation level", getFieldEditorParent))
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
        StringConverter.asString(new RGB(244, 255, 200)))
    node.put(KEYWORD_COLOR,
        StringConverter.asString(new RGB(127, 6, 101)))

    node.putBoolean(MATCHING_BRACKETS, true)
    node.put(MATCHING_BRACKETS_COLOR,
        StringConverter.asString(new RGB(192, 192, 192)))

    node.putInt(SpacesPerIndentationLevel.ID, 2)
  }
}
object CoqoonUIPreferences {
  val COQ_SENT_BACKGROUND = "coqSentBg"
  val COQ_PROCESSING_BACKGROUND = "coqSentProcessBg"
  val KEYWORD_COLOR = "coqKeywordFg"
  val MATCHING_BRACKETS = "matchingBrackets"
  val MATCHING_BRACKETS_COLOR = "matchingBracketsColor"

  object SpacesPerIndentationLevel {
    val ID = "spacesPerIndentationLevel"
    def get() = Activator.getDefault.getPreferenceStore.getInt(ID)
  }
}