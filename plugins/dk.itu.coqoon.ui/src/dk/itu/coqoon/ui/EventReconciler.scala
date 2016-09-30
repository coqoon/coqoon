package dk.itu.coqoon.ui

import dk.itu.coqoon.core.utilities.BatchCollector
import org.eclipse.jface.text.{
  IDocument, ITextViewer, DocumentEvent, IDocumentListener, ITextInputListener}

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

class MultiReconciler(delay : Int = BatchCollector.DEFAULT_DELAY)
    extends EventReconciler(delay) {
  private var handlers : List[List[DecoratedEvent] => Unit] = List()
  def addHandler(f : List[DecoratedEvent] => Unit) = (handlers :+= f)

  override def process(items : List[DecoratedEvent]) =
    handlers.foreach(handler => handler(items))
}