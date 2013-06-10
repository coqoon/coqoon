package dk.itu.sdg.kopitiam

import org.eclipse.ui.plugin.AbstractUIPlugin

class Activator extends AbstractUIPlugin {
  System.setProperty("file.encoding", "UTF-8")
  System.setProperty("sun.jnu.encoding", "UTF-8")

  import org.osgi.framework.BundleContext
  
  override def start(context : BundleContext) = {
    super.start(context)
    println("starting Kopitiam!")
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
  final val PLUGIN_ID = "Kopitiam"
  
  private var single : Activator = null
  def getDefault() = single
}