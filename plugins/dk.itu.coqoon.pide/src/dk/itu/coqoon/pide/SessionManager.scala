package dk.itu.coqoon.pide

abstract class SessionManager {
  import isabelle.Session
  import dk.itu.coqoon.core.utilities.CacheSlot

  private final val SessionLock = new Object
  private lazy val session = CacheSlot {
    import isabelle.{Coq_Syntax, Coq_Resources}
    val syntax = new Coq_Syntax
    val resources = new Coq_Resources(syntax)
    val session = new Session
    session.register_resources("coq", resources)

    initialisers.foreach(f => f(session))

    session
  }

  private var initialisers : Seq[Session => Unit] = Seq()
  /* Adds the function @f to the list of functions to be called whenever a
   * session is created or recreated. (If a session is already running, then @f
   * will be called with it, but this method will not create a new session.) */
  def addInitialiser(f : Session => Unit) =
    executeWithSessionLockSlot(slot => {
      initialisers :+= f
      if (slot.test)
        f(slot.get)
    })

  def start() : Session.Phase
  def stop() = executeWithSessionLockSlot(_.asOption.foreach(_.stop))

  def getPhase() = executeWithSessionLock(_.phase)
    
  def executeWithSessionLock[A](f : Session => A) =
    executeWithSessionLockSlot(s => f(s.get))

  def executeWithSessionLockSlot[A](f : CacheSlot[Session] => A) =
    SessionLock synchronized {
      f(session)
    }
}
