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

  override def createFieldEditors =
    allPrefs.foreach(p => addField(p.createFieldEditor(getFieldEditorParent)))
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
  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.preference.FieldEditor
  abstract class Preference[A](val id : String,
      val default : A, val name : String, val description : String) {
    def get() : A
    def createFieldEditor(parent : Composite) : FieldEditor
  }

  case class BooleanPreference(override val id : String,
      override val default : Boolean, override val name : String,
      override val description : String)
          extends Preference[Boolean](id, default, name, description) {
    override final def get() =
      Activator.getDefault.getPreferenceStore.getBoolean(id)

    import org.eclipse.jface.preference.BooleanFieldEditor
    override def createFieldEditor(parent : Composite) = {
      val f = new BooleanFieldEditor(id, s"${name}", parent)
      f.getDescriptionControl(parent).setToolTipText(description)
      f
    }
  }

  object SuppressStackTraces extends BooleanPreference("suppressStackTraces",
      false, "Suppress stack traces",
      "Don't capture a stack trace when generating debugging messages.")

  object NoQueryDelegation extends BooleanPreference("noQueryDelegation",
      false, "Disable PIDE query delegation",
      "Only run PIDE queries on the main process and not on workers.")

  class ChannelPreference(id : String, name : String, description : String)
      extends BooleanPreference(id, false, name, s"$description ($id)") {
    def log(text : String) =
      if (Activator.getDefault != null && get()) {
        val dummy =
          if (!SuppressStackTraces.get) {
            new Exception("(dummy stack trace exception)").fillInStackTrace
          } else null
        DebugListener.onDebugEvent(id, text, Option(dummy))
      }
  }

  object PrintProcessInvocations extends ChannelPreference("debug.process",
      "Log coqtop invocations",
      "Log debugging messages whenever new instances of coqtop are started.")
  object PrintIdeslaveTraffic extends ChannelPreference("debug.ideslave",
      "Log -ideslave protocol messages",
      "Log debugging messages whenever ideslave protocol messages are sent " +
      "and received.")
  object PrintPIDETraffic extends ChannelPreference("debug.pide",
      "Log PIDE protocol messages",
      "Log debugging messages whenever PIDE protocol messages are sent and " +
      "received.")
  object PIDEMarkers extends ChannelPreference("debug.pide-markers",
      "Log PIDE error markers",
      "Log debugging messages whenever PIDE error markers are created or " +
      "deleted.")
  object LoadPathResolution extends ChannelPreference("debug.resolution",
      "Log load path resolution",
      "Log debugging messages whenever resolution is attempted on an " +
      "incomplete load path entry.")
  object LoadPathExpansion extends ChannelPreference("debug.expansion",
      "Log load path expansion",
      "Log debugging messages whenever a complete load path entry is " +
      "expanded.")
  object ProjectBuild extends ChannelPreference("debug.project",
      "Log project build events",
      "Log debugging messages whenever a project build operation starts or " +
      "stops.")
  object ModelBroadcasts extends ChannelPreference("debug.model",
      "Log Coq model event notifications",
      "Log debugging messages whenever the Coq model emits a change " +
      "notification.")
  object Profiling extends ChannelPreference("debug.profiling",
      "Log profiling information",
      "Log detailed timing information for some UI tasks.")

  val allPrefs = Seq[Preference[_]](
      SuppressStackTraces,
      NoQueryDelegation,
      PrintProcessInvocations,
      PrintIdeslaveTraffic,
      PrintPIDETraffic,
      PIDEMarkers,
      LoadPathResolution,
      LoadPathExpansion,
      ProjectBuild,
      ModelBroadcasts,
      Profiling)
}