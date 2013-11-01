/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import dk.itu.coqoon.core.coqtop.CoqProgram

import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.ui.IWorkbenchPreferencePage

class KopitiamPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init (workbench : IWorkbench) : Unit = {
    setPreferenceStore(Activator.getDefault.getPreferenceStore)
  }

  import org.eclipse.jface.preference.{BooleanFieldEditor, DirectoryFieldEditor}
  override def createFieldEditors () : Unit = {
    addField(new DirectoryFieldEditor("loadpath", "Path to Charge!", getFieldEditorParent))
    addField(new BooleanFieldEditor("implicit", "Implicitly generate 'forward' proof script for each statement", getFieldEditorParent))
  }
}

class KopitiamColorPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
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
