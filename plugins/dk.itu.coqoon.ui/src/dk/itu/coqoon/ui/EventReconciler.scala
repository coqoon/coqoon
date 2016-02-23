package dk.itu.coqoon.ui

import dk.itu.coqoon.ui.utilities.SupersedableTask
import org.eclipse.jface.text.{
  IDocument, ITextViewer, DocumentEvent, IDocumentListener, ITextInputListener}

abstract class BatchCollector[A](delay : Int = BatchCollector.DEFAULT_DELAY) {
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
object BatchCollector {
  final val DEFAULT_DELAY = 400
}

import EventReconciler.DecoratedEvent
abstract class EventReconciler(delay : Int = BatchCollector.DEFAULT_DELAY)
    extends BatchCollector[DecoratedEvent](delay) {
  private object ReconciliationLock {
    var events : List[DecoratedEvent] = List()
  }

  private object Listener extends IDocumentListener with ITextInputListener {
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

  def install(v : ITextViewer) = Option(v).foreach(viewer => {
    viewer.addTextInputListener(Listener)
    this.viewer = Some(viewer)
  })
  def uninstall() = viewer.foreach(viewer => {
    viewer.removeTextInputListener(Listener)
    this.viewer = None
  })
}
object EventReconciler {
  case class DecoratedEvent(ev : DocumentEvent, old : String)
}