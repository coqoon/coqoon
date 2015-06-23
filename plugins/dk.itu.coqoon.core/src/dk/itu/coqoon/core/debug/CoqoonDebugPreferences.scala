/* CoqoonDebugPreferences
 * Debug channels integrated into the Eclipse preference system
 * Copyright © 2013, 2015 Alexander Faithfull
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

package dk.itu.coqoon.core.debug

import dk.itu.coqoon.core.{Activator, ManifestIdentifiers}
import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}

class CoqoonDebugPreferencesPage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  import CoqoonDebugPreferences._

  override def init(w : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference._
  override def createFieldEditors = {
    for (i <- allPrefs) {
      val parent = getFieldEditorParent
      val field = i match {
        case ChannelPreference(id, name, description) =>
          val f = new BooleanFieldEditor(id, s"${name} (${id})", parent)
          f.getDescriptionControl(parent).setToolTipText(description)
          Some(f)
        case _ =>
          None
      }
      field.foreach(addField)
    }
  }
}

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer

class CoqoonDebugPreferences extends AbstractPreferenceInitializer {
  import CoqoonDebugPreferences._

  override def initializeDefaultPreferences() = {
    import org.eclipse.core.runtime.preferences.DefaultScope
    val node = DefaultScope.INSTANCE.getNode(ManifestIdentifiers.PLUGIN)
    allPrefs.foreach(p => node.put(p.id, p.default.toString))
  }
}
object CoqoonDebugPreferences {
  abstract class Preference[A](val id : String,
      val default : A, val name : String, val description : String) {
    def get() : A
  }

  object SuppressStackTraces extends Preference("suppressStackTraces",
      false, "Suppress stack traces",
      "Don't capture a stack trace when generating debugging messages.") {
    override def get() = Activator.getDefault.getPreferenceStore.getBoolean(id)
  }

  case class ChannelPreference(override val id : String,
      override val name : String, override val description : String)
          extends Preference(id, false, name, description) {
    import org.eclipse.core.runtime.{Status, IStatus}
    override def get() = Activator.getDefault.getPreferenceStore.getBoolean(id)
    def log(text : String) =
      if (get()) {
        val dummy =
          if (!SuppressStackTraces.get) {
            new Exception("(dummy stack trace exception)").fillInStackTrace
          } else null
        Activator.getDefault.getLog.log(new Status(
            IStatus.INFO,
            ManifestIdentifiers.PLUGIN,
            IStatus.OK,
            s"${id}: ${text}", dummy))
      }
  }
  object PrintProcessInvocations extends ChannelPreference("debug.process",
      "coqtop invocations",
      "Log debugging messages whenever new instances of coqtop are started.")
  object PrintIdeslaveTraffic extends ChannelPreference("debug.ideslave",
      "-ideslave protocol messages",
      "Log debugging messages whenever ideslave protocol messages are sent " +
      "and received.")
  object PrintPIDETraffic extends ChannelPreference("debug.pide",
      "PIDE protocol messages",
      "Log debugging messages whenever PIDE protocol messages are sent and " +
      "received.")
  object PIDEMarkers extends ChannelPreference("debug.pide-markers",
      "PIDE error markers",
      "Log debugging messages whenever PIDE error markers are created or " +
      "deleted.")
  object LoadPathResolution extends ChannelPreference("debug.resolution",
      "Load path resolution",
      "Log debugging messages whenever resolution is attempted on an " +
      "incomplete load path entry.")
  object LoadPathExpansion extends ChannelPreference("debug.expansion",
      "Load path expansion",
      "Log debugging messages whenever a complete load path entry is " +
      "expanded.")
  object ProjectBuild extends ChannelPreference("debug.project",
      "Project build",
      "Log debugging messages whenever a project build operation starts or " +
      "stops.")

  val allPrefs = Seq(
      PrintProcessInvocations,
      PrintIdeslaveTraffic,
      PrintPIDETraffic,
      PIDEMarkers,
      LoadPathResolution,
      LoadPathExpansion,
      ProjectBuild)
}