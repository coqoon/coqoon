package dk.itu.coqoon.ui.editors

import dk.itu.coqoon.core.coqtop.ideslave.CoqTypes

trait CoqContainer {
  private val lock = new Object

  import org.eclipse.ui.IPropertyListener
  private var listeners = Set[IPropertyListener]()
  def addListener(l : IPropertyListener) =
    lock synchronized (listeners += l)
  def removeListener(l : IPropertyListener) =
    lock synchronized (listeners -= l)
  def fireChange(propertyID : Int) : Unit =
    lock synchronized { listeners }.map(_.propertyChanged(this, propertyID))
}

trait CoqGoalsContainer extends CoqContainer {
  private val lock = new Object

  private var goals_ : Option[CoqTypes.goals] = None
  def goals = lock synchronized { goals_ }
  def setGoals(g : Option[CoqTypes.goals]) = {
    lock synchronized { goals_ = g }
    fireChange(CoqGoalsContainer.PROPERTY_GOALS)
  }
}
object CoqGoalsContainer {
  final val PROPERTY_GOALS = 1979
}
