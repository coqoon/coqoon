/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.IStartup
class Startup extends IStartup {
  override def earlyStartup () : Unit = {
    Console.println("earlyStartup called")
    ActionDisabler.disableAll
    DocumentMonitor.init
    CoqTop.coqpath = Activator.getDefault.getPreferenceStore.getString("coqpath") + System.getProperty("file.separator")
    CoqTop.init
  }
}

import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.ui.IWorkbenchPreferencePage

class KopitiamPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init (workbench : IWorkbench) : Unit = {
    setPreferenceStore(Activator.getDefault.getPreferenceStore)
  }
  
  import org.eclipse.jface.preference.DirectoryFieldEditor
  override def createFieldEditors () : Unit = {
    addField(new DirectoryFieldEditor("coqpath", "Path to Coq", getFieldEditorParent))
  }

  import java.io.File
  override def performOk () : Boolean = {
    super.performOk()
    val coqp = Activator.getDefault.getPreferenceStore.getString("coqpath")
    if (new File(coqp + System.getProperty("file.separator") + "coqtop").exists)
      CoqTop.coqpath = coqp + System.getProperty("file.separator")
    else
      setErrorMessage("couldn't find coqtop in the specified path")
    true
  }
}

object KopitiamPreferencePage extends KopitiamPreferencePage { }
