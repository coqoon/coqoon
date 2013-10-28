package dk.itu.ecloq.core.utilities

object TryCast {
  def apply[A](a : Any)(implicit a0 : Manifest[A]) = a0.unapply(a)
}

object TryAdapt {
  import org.eclipse.core.runtime.IAdaptable
  def apply[A](ad : IAdaptable)(implicit a0 : Manifest[A]) : Option[A] =
    Option(ad).map(_.getAdapter(a0.runtimeClass)).flatMap(TryCast[A])
}

object TryService {
  import org.eclipse.ui.services.IServiceLocator
  def apply[A](sl : IServiceLocator)(implicit a0 : Manifest[A]) : Option[A] =
    Option(sl).map(_.getService(a0.runtimeClass)).flatMap(TryCast[A])
}