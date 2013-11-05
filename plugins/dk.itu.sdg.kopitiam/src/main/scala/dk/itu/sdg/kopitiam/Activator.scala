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
  final val MARKER_PROVEN = "dk.itu.sdg.kopitiam.provenmarker"
  final val PLUGIN = "dk.itu.sdg.kopitiam"
}
