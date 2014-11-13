package dk.itu.sdg.kopitiam.javap

import dk.itu.sdg.kopitiam.Activator
import dk.itu.coqoon.core.model.{CoqLoadPath, AbstractLoadPathManager,
  AbstractLoadPathProvider, AbstractLoadPathImplementation}

import org.eclipse.core.runtime.Path

class ChargeLibrary extends AbstractLoadPathProvider {
  override def getName = "Charge! for Java"

  override def getImplementation(id : String) =
    if (ChargeLibrary.ID == id) {
      Some(new ChargeLibrary.Implementation(this, id))
    } else None

  override def getImplementations : Seq[AbstractLoadPathImplementation] =
    Seq(new ChargeLibrary.Implementation(this))
}
object ChargeLibrary {
  final val ID = "dk.itu.sdg.kopitiam/lp/charge/0.1"

  import dk.itu.coqoon.core.model.AbstractLoadPathImplementation
  private class Implementation(provider : AbstractLoadPathProvider,
      id : String = ID) extends AbstractLoadPathImplementation {
    override def getProvider = provider
    override def getIdentifier = id

    override def getName = "Charge! for Java"
    override def getAuthor = "Jesper Bengtson <jebe@itu.dk>"
    override def getDescription =
      "The Charge! separation logic framework, for verifying Java programs."

    import AbstractLoadPathImplementation._
    override def getLoadPath =
      if (id == ID) {
        Activator.getDefault.getPreferenceStore.getString("loadpath") match {
          case p if p.length > 0 =>
            Right(Seq(CoqLoadPath(new Path(p), None)))
          case _ =>
            Left(Broken)
        }
      } else Left(VersionMismatch)
  }
}