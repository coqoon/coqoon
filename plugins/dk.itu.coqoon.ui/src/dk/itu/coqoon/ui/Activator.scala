package dk.itu.coqoon.ui

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
  final val PLUGIN = "dk.itu.coqoon.ui"
  final val ANNOTATION_PROCESSED = "dk.itu.sdg.kopitiam.processed"
  final val ANNOTATION_PROCESSING = "dk.itu.sdg.kopitiam.processing"
  final val VIEW_GOAL_VIEWER = "kopitiam.GoalViewer"
  final val COMMAND_TOGGLE_COQ_FLAG =
    "dk.itu.coqoon.ui.commands.toggle_coq_flag"
  final val COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME =
    "dk.itu.coqoon.ui.commands.toggle_coq_flag.name"
}
