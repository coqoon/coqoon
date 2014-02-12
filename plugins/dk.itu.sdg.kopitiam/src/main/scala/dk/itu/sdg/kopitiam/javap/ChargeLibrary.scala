package dk.itu.sdg.kopitiam.javap

import dk.itu.sdg.kopitiam.Activator
import dk.itu.coqoon.core.model.{
  CoqLoadPath, AbstractLoadPathManager, AbstractLoadPathProvider}

import org.eclipse.core.runtime.Path

class ChargeLibrary extends AbstractLoadPathProvider {
  override def getName = "Charge! for Java"

  override def getLoadPath(id : String) =
      Activator.getDefault.getPreferenceStore.getString("loadpath") match {
    case p if p.length > 0 => Seq(CoqLoadPath(new Path(p), None))
    case p => Nil
  }
}
object ChargeLibrary {
  final val ID = "dk.itu.sdg.kopitiam/lp/charge/0.1"
}