/* Activator.scala
 * The plugin class for the Coqoon core
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

import org.osgi.framework.BundleContext
import org.eclipse.ui.plugin.AbstractUIPlugin

class Activator extends AbstractUIPlugin {
  override def start(context : BundleContext) = {
    super.start(context)
    Activator.instance = this
  }
  
  override def stop(context : BundleContext) = {
    Activator.instance = null
    super.stop(context)
  }
}
object Activator {
  private var instance : Activator = _
  
  def getDefault() = instance
}

object ManifestIdentifiers {
  final val PLUGIN = "dk.itu.coqoon.core"
  final val BUILDER_COQ = "dk.itu.sdg.kopitiam.CoqBuilder"
  final val EXTENSION_POINT_LOADPATH = "dk.itu.coqoon.core.loadpath"
  final val EXTENSION_POINT_OVERRIDE = "dk.itu.coqoon.core.coqtop"
  final val CONTENT_TYPE_COQFILE = "dk.itu.coqoon.core.CoqFile"
  final val CONTENT_TYPE_COQOBJECTFILE = "dk.itu.coqoon.core.CoqObjectFile"
  final val MARKER_PROBLEM = "dk.itu.sdg.kopitiam.problemmarker"
  final val NATURE_COQ = "dk.itu.sdg.kopitiam.CoqNature"
  final val PREFERENCE_PAGE_COQOON = "dk.itu.coqoon.core.pref"
}
