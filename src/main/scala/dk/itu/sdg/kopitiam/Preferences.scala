/* (c) 2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

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
    val coq =
      if (CoqTop.isWin)
        CoqTop.coqtopbinary + ".exe"
      else
        CoqTop.coqtopbinary
    if (new File(coqp + System.getProperty("file.separator") + coq).exists)
      CoqTop.coqpath = coqp + System.getProperty("file.separator")
    else
      setErrorMessage("couldn't find coqtop in the specified path")
    true
  }
}

object KopitiamPreferencePage extends KopitiamPreferencePage { }

class KopitiamColorPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {
  import org.eclipse.ui.IWorkbench

  override def init (workbench : IWorkbench) : Unit = {
    setPreferenceStore(Activator.getDefault.getPreferenceStore)
  }

  import org.eclipse.jface.preference.ColorFieldEditor
  override def createFieldEditors () : Unit = {
    addField(new ColorFieldEditor("coqSentBg", "Coq Sent Background", getFieldEditorParent))
    addField(new ColorFieldEditor("coqSentFg", "Coq Sent Foreground", getFieldEditorParent))
  }
}

object KopitiamColorPreferencePage extends KopitiamColorPreferencePage
