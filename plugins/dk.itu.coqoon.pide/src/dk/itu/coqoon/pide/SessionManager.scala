package dk.itu.coqoon.pide

abstract class SessionManager {
  import isabelle.Session

  private final val SessionLock = new Object
  private lazy val session = {
    import isabelle.{Coq_Syntax, Coq_Resources}
    val syntax = new Coq_Syntax
    val resources = new Coq_Resources(syntax)
    val session = new Session
    session.register_resources("coq", resources)
    
    session
  }

  def start() : Session.Phase
  def stop() =
    executeWithSessionLock(session => {
      session.stop
    })

  def getPhase() = executeWithSessionLock(_.phase)
    
  def executeWithSessionLock[A](f : Session => A) =
    SessionLock synchronized {
      f(session)
    }
}