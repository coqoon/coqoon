/* (c) 2011 Hannes Mehnert */

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

object CoqoonUIPreferences {
  val MATCHING_BRACKETS = "matchingBrackets"
  val MATCHING_BRACKETS_COLOR = "matchingBracketsColor"
}