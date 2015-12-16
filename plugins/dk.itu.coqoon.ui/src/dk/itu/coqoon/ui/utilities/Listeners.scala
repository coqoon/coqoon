/* Listeners.scala
 * Scala wrappers around SWT events and event listeners
 * Copyright © 2015 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.ui.utilities

import org.eclipse.swt.{SWT, events, widgets}

object Event {
  object Selection {
    def unapply(ev : widgets.Event) =
      if (ev.`type` == SWT.Selection) {
        Some(new events.SelectionEvent(ev))
      } else None
  }
  object DefaultSelection {
    def unapply(ev : widgets.Event) =
      if (ev.`type` == SWT.DefaultSelection) {
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
  object DefaultSelection {
    def apply(w : widgets.Widget, l : widgets.Listener) =
      w.addListener(SWT.DefaultSelection, l)
  }
  object Modify {
    def apply(w : widgets.Widget, l : widgets.Listener) =
      w.addListener(SWT.Modify, l)
  }
}