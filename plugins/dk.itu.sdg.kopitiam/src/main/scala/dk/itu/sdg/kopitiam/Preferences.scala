/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}
import org.eclipse.ui.IWorkbenchPreferencePage

class KopitiamPreferencePage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
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
