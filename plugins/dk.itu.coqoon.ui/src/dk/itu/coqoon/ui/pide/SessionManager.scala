/* SessionManager.scala
 * Coqoon-specific subclasses and wrappers for the PIDE SessionManager class
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.core.CoqoonPreferences
import dk.itu.coqoon.core.debug.CoqoonDebugPreferences
import dk.itu.coqoon.core.coqtop.CoqProgram
import isabelle.Session

class SessionManager extends dk.itu.coqoon.pide.SessionManager {
  override def start =
    executeWithSessionLock(session => {
      session.start("coq", CoqProgram.path,
          Seq("-async-queries-always-delegate") ++
              CoqoonPreferences.ExtraArguments.get)
      while (!session.is_ready && session.phase != Session.Failed)
        Thread.sleep(500)
      session.phase
    })

  addInitialiser(session => {
    session.commands_changed += Session.Consumer[Any]("Coqoon") {
      case changed : Session.Commands_Changed =>
        CoqoonDebugPreferences.PrintPIDETraffic.log(s"! ${changed}")
      case q =>
        CoqoonDebugPreferences.PrintPIDETraffic.log(s"! ${q}")
    }
    session.all_messages += Session.Consumer("Coqoon")(q =>
      CoqoonDebugPreferences.PrintPIDETraffic.log(s"? ${q}"))
  })
}

class SessionPool(count : Int = 3) {
  private class PooledSession extends SessionManager {
    addInitialiser(session => {
      session.phase_changed +=
        Session.Consumer[Session.Phase]("Phase listener")(
            p => onPhaseChange(this, p))
    })
  }

  def makePooledSession() : SessionManager = new PooledSession

  private object PoolLock {
    var active : List[PooledSession] = List()
  }

  /* Session.Failed doesn't actually mean "failed"; it means "inactive and
   * can't be started". (The normal shutdown sequence, for example, is
   * Session.Ready -> Session.Shutdown -> Session.Failed.) */
  private def onPhaseChange(
      sacrifice : PooledSession, newPhase : Session.Phase) = {
    val toKill =
      PoolLock synchronized {
        newPhase match {
          case Session.Ready =>
            PoolLock.active :+= sacrifice
            if (PoolLock.active.size > count) {
              try {
                PoolLock.active.headOption
              } finally PoolLock.active = PoolLock.active.tail
            } else None
          case Session.Failed
              if PoolLock.active.contains(sacrifice) =>
            PoolLock.active = PoolLock.active.filter(_ != sacrifice)
            None
          case _ =>
            None
        }
      }
    toKill.foreach(_.stop)
  }
}
object SessionPool extends SessionPool(count = 3)
