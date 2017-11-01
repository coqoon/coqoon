package dk.itu.coqoon.ui.utilities.jface

import dk.itu.coqoon.core.utilities.TryCast

object Selection {
  import org.eclipse.jface.viewers
  import scala.collection.JavaConversions.asScalaBuffer
  object Structured {
    def unapply(a : viewers.ISelection) : Option[Seq[Any]] =
      TryCast[viewers.IStructuredSelection](a).map(
          s => asScalaBuffer(s.toList).toList)
  }
}