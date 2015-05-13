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
    
    session
  }

  def start() : Session.Phase
  def stop() =
    SessionLock synchronized {
      session.asOption match {
        case Some(s) =>
          s.stop
          session.clear
        case None =>
      }
    }

  def getPhase() = executeWithSessionLock(_.phase)
    
  def executeWithSessionLock[A](f : Session => A) =
    SessionLock synchronized {
      f(session.get)
    }
}
