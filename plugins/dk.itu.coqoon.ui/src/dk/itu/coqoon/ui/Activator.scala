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

  import ManifestIdentifiers.Images._
  import dk.itu.coqoon.ui.utilities.UIUtils
  import org.eclipse.swt.graphics.{Image, ImageData}
  import org.eclipse.core.runtime.{Path, FileLocator}
  import org.eclipse.jface.resource.ImageRegistry
  override protected def initializeImageRegistry(r : ImageRegistry) =
    for (i <- Seq(
        PACKAGE_FRAGMENT -> "icons/jdt/package_obj.gif",
        PACKAGE_FRAGMENT_ROOT -> "icons/jdt/packagefolder_obj.gif"))
      r.put(i._1, new Image(UIUtils.getDisplay, new ImageData(
          FileLocator.find(getBundle, new Path(i._2), null).openStream)))
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

  object Images {
    final val PACKAGE_FRAGMENT =
      "dk.itu.coqoon.ui.images.PackageFragment"
    final val PACKAGE_FRAGMENT_ROOT =
      "dk.itu.coqoon.ui.images.PackageFragmentRoot"
  }
}
