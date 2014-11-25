package dk.itu.sdg.kopitiam.javap

import dk.itu.sdg.kopitiam.Activator
import dk.itu.coqoon.core.model.{LoadPathEntry, AbstractLoadPathManager,
  LoadPathImplementation, LoadPathImplementationFactory}

import org.eclipse.core.runtime.Path

class ChargeLibrary extends LoadPathImplementationFactory {
  override def getName = "Charge! for Java"

  override def getImplementation(id : String) =
    if (ChargeLibrary.ID == id) {
      Some(new ChargeLibrary.Implementation(this, id))
    } else None

  override def getImplementations : Seq[LoadPathImplementation] =
    Seq(new ChargeLibrary.Implementation(this))
}
object ChargeLibrary {
  final val ID = "dk.itu.sdg.kopitiam/lp/charge/0.1"

  private class Implementation(provider : LoadPathImplementationFactory,
      id : String = ID) extends LoadPathImplementation {
    override def getProvider = provider
    override def getIdentifier = id

    override def getName = "Charge! for Java"
    override def getAuthor = "Jesper Bengtson <jebe@itu.dk>"
    override def getDescription =
      "The Charge! separation logic framework, for verifying Java programs."

    import LoadPathImplementation._
    override def getLoadPath =
      if (id == ID) {
        Activator.getDefault.getPreferenceStore.getString("loadpath") match {
          case p if p.length > 0 =>
            Right(Seq(LoadPathEntry(new Path(p), Nil)))
          case _ =>
            Left(Broken)
        }
      } else Left(VersionMismatch)
  }
}