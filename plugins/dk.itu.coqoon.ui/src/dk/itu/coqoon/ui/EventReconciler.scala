package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.SupersedableTask
import org.eclipse.jface.text.{
  IDocument, ITextViewer, DocumentEvent, IDocumentListener, ITextInputListener}

abstract class EventReconciler {
  import EventReconciler._

  private val reconcileTask = new SupersedableTask(400)
  private object ReconciliationLock {
    var events : List[DecoratedEvent] = List()
  }

  private class Listener extends IDocumentListener with ITextInputListener {
    override def documentAboutToBeChanged(ev : DocumentEvent) =
      ReconciliationLock synchronized {
        ReconciliationLock.events = ReconciliationLock.events :+
          DecoratedEvent(ev, ev.fDocument.get(ev.fOffset, ev.fLength))
        reconcileTask.schedule {
          val events =
            ReconciliationLock synchronized {
              try {
                ReconciliationLock.events
              } finally ReconciliationLock.events = List()
            }
          reconcile(events)
        }
      }
    override def documentChanged(ev : DocumentEvent) = ()

    override def inputDocumentAboutToBeChanged(
        outgoing : IDocument, incoming : IDocument) = ()
    override def inputDocumentChanged(
        outgoing : IDocument, incoming : IDocument) = {
      Option(outgoing).foreach(_.removeDocumentListener(this))
      Option(incoming).foreach(_.addDocumentListener(this))
    }
  }

  private var viewer : Option[ITextViewer] = None
  private var listener : Option[Listener] = None

  def install(v : ITextViewer) = {
    viewer = Option(v)
    listener = listener.orElse(Some(new Listener))
    (viewer, listener) match {
      case (Some(viewer), Some(listener)) =>
        viewer.addTextInputListener(listener)
      case _ =>
    }
  }
  def uninstall() = {
    (viewer, listener) match {
      case (Some(viewer), Some(listener)) =>
        viewer.removeTextInputListener(listener)
      case _ =>
        
    }
    viewer = None
    listener = None
  }

  def reconcile(events : List[DecoratedEvent])
}
object EventReconciler {
  case class DecoratedEvent(ev : DocumentEvent, old : String)
}