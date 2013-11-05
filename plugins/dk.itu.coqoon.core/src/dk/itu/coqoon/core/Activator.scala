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
  final val CONTENT_TYPE_COQFILE = "dk.itu.coqoon.core.CoqFile"
  final val CONTENT_TYPE_COQOBJECTFILE = "dk.itu.coqoon.core.CoqObjectFile"
  final val MARKER_PROBLEM = "dk.itu.sdg.kopitiam.problemmarker"
  final val NATURE_COQ = "dk.itu.sdg.kopitiam.CoqNature"
}
