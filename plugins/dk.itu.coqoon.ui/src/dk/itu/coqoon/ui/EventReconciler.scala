package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.SupersedableTask
import org.eclipse.jface.text.{
  IDocument, ITextViewer, DocumentEvent, IDocumentListener, ITextInputListener}

abstract class BatchCollector[A](delay : Int = 400) {
  private val collectTask = new SupersedableTask(delay)
  private object CollectionLock {
    var items : List[A] = List()
  }
  def add(item : A) =
    CollectionLock synchronized {
      CollectionLock.items :+= item
      collectTask schedule {
        val items =
          CollectionLock synchronized {
            try {
              CollectionLock.items
            } finally CollectionLock.items = List()
          }
        process(items)
      }
    }

  protected def process(items : List[A])
}

import EventReconciler.DecoratedEvent
abstract class EventReconciler extends BatchCollector[DecoratedEvent]{
  private object ReconciliationLock {
    var events : List[DecoratedEvent] = List()
  }

  private class Listener extends IDocumentListener with ITextInputListener {
    override def documentAboutToBeChanged(ev : DocumentEvent) =
      add(DecoratedEvent(ev, ev.fDocument.get(ev.fOffset, ev.fLength)))

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
}
object EventReconciler {
  case class DecoratedEvent(ev : DocumentEvent, old : String)
}