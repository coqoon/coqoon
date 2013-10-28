package dk.itu.sdg.kopitiam

import org.eclipse.ui.plugin.AbstractUIPlugin

class Activator extends AbstractUIPlugin {
  System.setProperty("file.encoding", "UTF-8")
  System.setProperty("sun.jnu.encoding", "UTF-8")

  import org.osgi.framework.BundleContext
  
  override def start(context : BundleContext) = {
    super.start(context)
    Activator.single = this
  }

  override def stop(context : BundleContext) = {
    Activator.single = null
    super.stop(context)
  }

  import org.eclipse.jface.preference.{IPreferenceStore, PreferenceConverter}
  override protected def initializeDefaultPreferences(
      store : IPreferenceStore) = {
    import org.eclipse.swt.graphics.RGB
    PreferenceConverter.setDefault(store, "coqSentBg", new RGB(118, 255, 133))
    PreferenceConverter.setDefault(store, "coqSentProcessBg", new RGB(244, 255, 200))
    PreferenceConverter.setDefault(store, "coqKeywordFg", new RGB(127, 6, 101))
    store.setDefault("implicit", true)
  }
}
object Activator {
  private var single : Activator = null
  def getDefault() = single
  
  import org.eclipse.core.runtime.{Status, IStatus}
  def makeStatus(severity : Int,
      message : String, exception : Throwable = null) : IStatus =
    new Status(severity, ManifestIdentifiers.PLUGIN, message, exception)
}

object ManifestIdentifiers {
  final val ANNOTATION_PROCESSED = "dk.itu.sdg.kopitiam.processed"
  final val ANNOTATION_PROCESSING = "dk.itu.sdg.kopitiam.processing"
  final val MARKER_PROVEN = "dk.itu.sdg.kopitiam.provenmarker"
  final val MARKER_PROBLEM = "dk.itu.sdg.kopitiam.problemmarker"
  final val PLUGIN = "dk.itu.sdg.kopitiam"
  final val VIEW_GOAL_VIEWER = "kopitiam.GoalViewer"
  final val COMMAND_TOGGLE_COQ_FLAG = "Kopitiam.toggle_coq_flag"
  final val COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME =
    "Kopitiam.toggle_coq_flag.name"
}