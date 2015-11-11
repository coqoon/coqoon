package dk.itu.coqoon.ui

import org.osgi.framework.BundleContext
import org.eclipse.ui.plugin.AbstractUIPlugin

class Activator extends AbstractUIPlugin {
  override def start(context : BundleContext) = {
    super.start(context)
    Activator.instance = this
  }
  
  override def stop(context : BundleContext) = {
    try {
      Activator.instance = null
    } finally {
      super.stop(context)
    }
  }

  import ManifestIdentifiers.Images._
  import dk.itu.coqoon.ui.utilities.UIUtils
  import org.eclipse.swt.graphics.{Image, ImageData}
  import org.eclipse.core.runtime.{Path, FileLocator}
  import org.eclipse.jface.resource.ImageRegistry
  override protected def initializeImageRegistry(r : ImageRegistry) =
    for (i <- Seq(
        PACKAGE_FRAGMENT -> "icons/jdt/package_obj.gif",
        PACKAGE_FRAGMENT_ROOT -> "icons/jdt/packagefolder_obj.gif",
        EMPTY_PACKAGE_FRAGMENT -> "icons/jdt/empty_pack_obj.gif"))
      r.put(i._1, new Image(UIUtils.getDisplay, new ImageData(
          FileLocator.find(getBundle, new Path(i._2), null).openStream)))
}
object Activator {
  private var instance : Activator = _
  
  def getDefault() = instance
}

object ManifestIdentifiers {
  final val PLUGIN = "dk.itu.coqoon.ui"

  object Annotations {
    final val FAILED = "dk.itu.coqoon.ui.annotationTypes.failed"
    final val PROCESSED = "dk.itu.coqoon.ui.annotationTypes.processed"
    final val PROCESSING = "dk.itu.coqoon.ui.annotationTypes.processing"
  }

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
    final val EMPTY_PACKAGE_FRAGMENT =
      "dk.itu.coqoon.ui.images.EmptyPackageFragment"
  }
}
