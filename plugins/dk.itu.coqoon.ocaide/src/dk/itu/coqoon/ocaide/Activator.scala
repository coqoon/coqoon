package dk.itu.coqoon.ocaide

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
