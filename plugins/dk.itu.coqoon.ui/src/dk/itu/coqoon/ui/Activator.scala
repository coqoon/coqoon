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

  import org.eclipse.jface.preference.{IPreferenceStore, PreferenceConverter}
  override protected def initializeDefaultPreferences(
      store : IPreferenceStore) = {
    import org.eclipse.swt.graphics.RGB
    PreferenceConverter.setDefault(
        store, "coqSentBg", new RGB(118, 255, 133))
    PreferenceConverter.setDefault(
        store, "coqSentProcessBg", new RGB(244, 255, 200))
    PreferenceConverter.setDefault(
        store, "coqKeywordFg", new RGB(127, 6, 101))
  }
}
object Activator {
  private var instance : Activator = _
  
  def getDefault() = instance
}

object ManifestIdentifiers {
  final val PLUGIN = "dk.itu.coqoon.ui"
  final val ANNOTATION_PROCESSED = "dk.itu.coqoon.ui.annotationTypes.processed"
  final val ANNOTATION_PROCESSING =
    "dk.itu.coqoon.ui.annotationTypes.processing"
  final val VIEW_GOAL_VIEWER = "kopitiam.GoalViewer"
  final val COMMAND_TOGGLE_COQ_FLAG =
    "dk.itu.coqoon.ui.commands.toggle_coq_flag"
  final val COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME =
    "dk.itu.coqoon.ui.commands.toggle_coq_flag.name"
}
