package dk.itu.sdg.kopitiam.javap

import dk.itu.sdg.kopitiam.Activator
import dk.itu.coqoon.core.model.{LoadPathEntry, AbstractLoadPathManager,
  LoadPathImplementation, LoadPathImplementationProvider}

import org.eclipse.core.runtime.Path

class ChargeLibrary extends LoadPathImplementationProvider {
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

  import dk.itu.coqoon.core.model.IncompleteLoadPathEntry
  private class Implementation(provider : LoadPathImplementationProvider,
      id : String = ID) extends LoadPathImplementation {
    override def getProvider = provider
    override def getIdentifier = id

    override def getName = "Charge! for Java"
    override def getAuthor = "Jesper Bengtson <jebe@itu.dk>"
    override def getDescription =
      "The Charge! separation logic framework, for verifying Java programs."

    import LoadPathImplementation._
    override def getIncompleteLoadPath =
      if (id == ID) {
        Right(Seq(
            IncompleteLoadPathEntry(Seq(
                Left(ChargeLocation)), Nil)))
      } else Left(VersionMismatch)

    override def getValue(v : IncompleteLoadPathEntry.Variable) =
      if (v == ChargeLocation) {
        Activator.getDefault.getPreferenceStore.getString("loadpath") match {
          case p if p.length > 0 =>
            Some(p)
          case _ =>
            None
        }
      } else None
  }

  final val ChargeLocation = IncompleteLoadPathEntry.Variable(
      "CHARGE_LOCATION", "Path to the Charge! library")
}