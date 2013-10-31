package dk.itu.coqoon.core

import dk.itu.sdg.kopitiam.{Activator => KActivator}
import dk.itu.coqoon.core.coqtop.CoqProgram

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}

class CoqoonPreferencesPage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  override def init(w : IWorkbench) =
    setPreferenceStore(KActivator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference.DirectoryFieldEditor
  override def createFieldEditors =
    addField(new DirectoryFieldEditor(
        "coqpath", "Path to Coq", getFieldEditorParent))

  override def performOk = {
    super.performOk()
    if (!CoqProgram("coqtop").check) {
      setErrorMessage("Can't find the coqtop program")
      false
    } else true
  }
}