/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.ui.IWorkbenchPreferencePage

class KopitiamPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init (workbench : IWorkbench) : Unit = {
    setPreferenceStore(Activator.getDefault.getPreferenceStore)
  }

  import org.eclipse.jface.preference.{BooleanFieldEditor, DirectoryFieldEditor}
  override def createFieldEditors () : Unit = {
    addField(new DirectoryFieldEditor("coqpath", "Path to Coq", getFieldEditorParent))
    addField(new DirectoryFieldEditor("loadpath", "Path to Load", getFieldEditorParent))
    addField(new BooleanFieldEditor("implicit", "Implicitly generate 'forward' proof script for each statement", getFieldEditorParent))
    addField(new BooleanFieldEditor("smartcompilation", "'Smart' compile vernacular when stepped to the end", getFieldEditorParent))
  }

  import java.io.File
  override def performOk () : Boolean = {
    super.performOk()
    if (!CoqProgram("coqtop").check)
      setErrorMessage("couldn't find coqtop in the specified path")
    true
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
