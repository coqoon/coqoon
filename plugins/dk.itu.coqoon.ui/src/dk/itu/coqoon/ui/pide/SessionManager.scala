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

class SessionManager {
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

  def stop() = executeWithSessionLockSlot(_.asOption.foreach(_.stop))

  def getPhase() = executeWithSessionLock(_.phase)

  def executeWithSessionLock[A](f : Session => A) =
    executeWithSessionLockSlot(s => f(s.get))

  def executeWithSessionLockSlot[A](f : CacheSlot[Session] => A) =
    SessionLock synchronized {
      f(session)
    }

  import CoqoonDebugPreferences.PrintPIDETraffic

  def start() : Session.Phase =
    executeWithSessionLock(session => {
      session.start("coq", CoqProgram.path,
          Seq("-async-queries-always-delegate") ++
              CoqoonPreferences.ExtraArguments.get)
      while (!session.is_ready && session.phase != Session.Failed)
        Thread.sleep(500)
      session.phase
    })

  addInitialiser(session => {
    session.commands_changed +=
      Session.Consumer[Session.Commands_Changed]("Coqoon")(
          changed => PrintPIDETraffic.log(s"! ${changed}"))
    session.all_messages +=
      Session.Consumer("Coqoon")(
          q => PrintPIDETraffic.log(s"? ${q}"))
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
