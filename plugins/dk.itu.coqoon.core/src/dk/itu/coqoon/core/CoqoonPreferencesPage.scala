/* CoqoonPreferencesPage.scala
 * Support for configuring Coqoon through the Eclipse preferences system
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core

import dk.itu.coqoon.core.coqtop.CoqProgram

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}

class CoqoonPreferencesPage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  override def init(w : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference._
  override def createFieldEditors = {
    addField({
      val parent = getFieldEditorParent
      val ed = new DirectoryFieldEditor(
          CoqoonPreferences.CoqPath.ID,
          "Folder containing Coq", parent)
      ed.getLabelControl(parent).setToolTipText(
          "The directory containing the coqtop program (or coqtop.exe on " +
          "Windows systems).")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new StringFieldEditor(
          CoqoonPreferences.ExtraArguments.ID,
          "Extra arguments",
          parent)
      ed.getLabelControl(parent).setToolTipText(
          "Extra arguments to be passed to the coqtop program whenever it " +
          "is invoked.")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonPreferences.RequireQualification.ID,
          "Require library names to be qualified (experimental, Coq 8.5+)",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Modify the dependency resolution behaviour to mirror that of the " +
          "-Q option introduced in Coq 8.5, and pass that option when " +
          "configuring Coq processes.")
      ed
    })
    addField({
      val parent = getFieldEditorParent
      val ed = new BooleanFieldEditor(
          CoqoonPreferences.UseQuick.ID,
          "Enable the quick compilation process (experimental, Coq 8.5+)",
          parent)
      ed.getDescriptionControl(parent).setToolTipText(
          "Pass the -quick option to Coq, causing it to defer many " +
          "computations by generating larger library files.")
      ed
    })
  }

  override def performOk = {
    super.performOk()
    if (!CoqProgram.check) {
      setErrorMessage("Can't find the coqtop program")
      false
    } else true
  }
}

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

class CoqoonPreferences extends AbstractPreferenceInitializer {
  import CoqoonPreferences._

  override def initializeDefaultPreferences() = {
    import org.eclipse.core.runtime.preferences.DefaultScope

    val node = DefaultScope.INSTANCE.getNode(ManifestIdentifiers.PLUGIN)

    node.put(CoqPath.ID, CoqPath.tryCandidates.getOrElse(""))
    node.put(ExtraArguments.ID, "")
    node.putBoolean(RequireQualification.ID, false)
    node.putBoolean(UseQuick.ID, false)
  }
}

object CoqoonPreferences {
  object CoqPath {
    final val ID = "coqpath"

    import org.eclipse.core.runtime.{Path, IPath}
    def get() : Option[IPath] =
      Option(Activator.getDefault.getPreferenceStore.getString(ID)).
          map(_.trim).filter(_.length != 0).map(p => new Path(p))

    def set(path : IPath) =
      Option(Activator.getDefault.getPreferenceStore).foreach(
          _.setValue(ID, path.toString))

    private final val candidates = Seq(
        ("/usr/local/bin", "coqtop"),
        ("/usr/bin", "coqtop"),
        ("C:\\Program Files\\Coq\\bin", "coqtop.exe"),
        ("C:\\Program Files (x86)\\Coq\\bin", "coqtop.exe"))
    private[CoqoonPreferences] def tryCandidates : Option[String] = {
      import java.io.File
      for ((directory, executable) <- candidates
               if new File(directory, executable).exists)
        return Some(directory)
      None
    }
  }

  object RequireQualification {
    final val ID = "requirequalification"
    def get() = Activator.getDefault.getPreferenceStore.getBoolean(ID)
  }

  object UseQuick {
    final val ID = "usequick"
    def get() = Activator.getDefault.getPreferenceStore.getBoolean(ID)
  }

  object ExtraArguments {
    import dk.itu.coqoon.core.project.CoqProjectFile.shellTokenise

    final val ID = "extraarguments"
    def get() =
      shellTokenise(Activator.getDefault.getPreferenceStore.getString(ID))
  }
}