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

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.preference.DirectoryFieldEditor
  private class FriendlyDirectoryFieldEditor(
      val name : String, val label : String, val parent : Composite)
      extends DirectoryFieldEditor(name, label, parent) {
    override def getChangeControl(parent : Composite) =
      super.getChangeControl(parent)
  }

  import org.eclipse.jface.preference._
  override def createFieldEditors = {
    addField({
      val parent = getFieldEditorParent
      val ed = new FriendlyDirectoryFieldEditor(CoqoonPreferences.CoqPath.ID,
          "Folder containing Coq", parent)
      ed.getLabelControl(parent).setToolTipText(
          "The directory containing the coqtop program (or coqtop.exe on " +
          "Windows systems).")
      if (CoqoonPreferences.CoqPath.isOverridden) {
        val id = CoqoonPreferences.CoqPath.getOverridingBundle.get
        ed.getTextControl(parent).setEditable(false)
        ed.getTextControl(parent).setToolTipText(
            s"The plugin '$id' is overriding the value of this preference. " +
            "It must be removed or disabled to make manual changes.")
        ed.getLabelControl(parent).setEnabled(false)
        ed.getChangeControl(parent).setEnabled(false)
      }
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

import org.eclipse.core.runtime.{Path, IPath}

trait ICoqPathOverride {
  def getCoqPath() : IPath
}

object CoqoonPreferences {
  object CoqPath {
    final val ID = "coqpath"

    private final lazy val pathOverride = {
      import dk.itu.coqoon.core.utilities.TryCast
      import org.eclipse.core.runtime.RegistryFactory
      RegistryFactory.getRegistry.getConfigurationElementsFor(
          ManifestIdentifiers.EXTENSION_POINT_OVERRIDE).filter(
              _.getName == "override").headOption.flatMap(ice => {
        import org.osgi.framework.Constants.BUNDLE_NAME
        val o = Option(ice.createExecutableExtension("override")).flatMap(
            TryCast[ICoqPathOverride])
        val bundle = ice.getDeclaringExtension.getNamespaceIdentifier
        o.map((_, bundle))
      })
    }
    def isOverridden() = pathOverride != None
    def getOverridingBundle() = pathOverride.map(_._2)

    def get() : Option[IPath] =
      pathOverride match {
      case Some((o, _)) =>
        Some(o.getCoqPath)
      case None =>
        Option(Activator.getDefault.getPreferenceStore.getString(ID)).
            map(_.trim).filter(_.length != 0).map(p => new Path(p))
    }

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