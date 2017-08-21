package dk.itu.coqoon.core.debug

trait DebugListener {
  def onDebugEvent(id : String, message : String, trace : Option[Throwable])
}
object DebugListener {
  private object DebugLock {
    var listeners : Seq[DebugListener] = Seq()
  }

  def register(l : DebugListener) = {
    DebugLock synchronized (DebugLock.listeners :+= l)
  }
  def unregister(l : DebugListener) = {
    DebugLock synchronized (DebugLock.listeners.filterNot(_ == l))
  }

  private[debug] def onDebugEvent(
      id : String, message : String, trace : Option[Throwable]) =
    DebugLock synchronized (DebugLock.listeners.foreach(
        _.onDebugEvent(id, message, trace)))

  register(DebugLogger)
}

import dk.itu.coqoon.core.{Activator, ManifestIdentifiers}
import org.eclipse.core.runtime.{Status, IStatus}

object DebugLogger extends DebugListener {
  override def onDebugEvent(
      id : String, text : String, trace : Option[Throwable]) =
    Activator.getDefault.getLog.log(new Status(
            IStatus.INFO,
            ManifestIdentifiers.PLUGIN,
            DEBUG,
            s"${id}: ${text}", trace.orNull))
  final val DEBUG = 0x1000007
}