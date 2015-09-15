package dk.itu.coqoon.ui.utilities

import org.eclipse.swt.{SWT, events, widgets}

object Event {
  object Selection {
    def unapply(ev : widgets.Event) =
      if (ev.`type` == SWT.Selection) {
        Some(new events.SelectionEvent(ev))
      } else None
  }
  object Modify {
    def unapply(ev : widgets.Event) =
      if (ev.`type` == SWT.Modify) {
        Some(new events.ModifyEvent(ev))
      } else None
  }
}
object Listener {
  def apply(f : PartialFunction[widgets.Event, Unit]) =
    new widgets.Listener {
      override def handleEvent(ev : widgets.Event) = f(ev)
    }
  object Selection {
    def apply(w : widgets.Widget, l : widgets.Listener) =
      w.addListener(SWT.Selection, l)
  }
  object Modify {
    def apply(w : widgets.Widget, l : widgets.Listener) =
      w.addListener(SWT.Modify, l)
  }
}