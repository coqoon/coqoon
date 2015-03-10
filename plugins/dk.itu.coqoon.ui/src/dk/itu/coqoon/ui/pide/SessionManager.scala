package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.core.CoqoonPreferences
import dk.itu.coqoon.core.debug.CoqoonDebugPreferences

object SessionManager extends dk.itu.coqoon.pide.SessionManager {
  import isabelle.Session
  override def start =
    executeWithSessionLock(session => {
      session.start("coq",
          CoqoonPreferences.CoqPath.get match {
            case Some(path) => path + java.io.File.separator + "coqtop"
            case _ => "coqtop"
          }, Nil)
      while (!session.is_ready && session.phase != Session.Failed)
        Thread.sleep(500)
      session.phase
    })

  start()
}