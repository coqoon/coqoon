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

import org.eclipse.swt.{SWT, widgets}

object Event {
  class _EventImpl[A] private[Event](kind : Int, f : widgets.Event => A) {
    def unapply(ev : widgets.Event) =
      if (ev.`type` == kind) {
        Some(f(ev))
      } else None
  }
  import org.eclipse.swt.events._
  object Selection extends _EventImpl(SWT.Selection, new SelectionEvent(_))
  object DefaultSelection extends _EventImpl(
      SWT.DefaultSelection, new SelectionEvent(_))
  object Modify extends _EventImpl(SWT.Modify, new ModifyEvent(_))
  object MouseUp extends _EventImpl(SWT.MouseUp, new MouseEvent(_))
}
object Listener {
  def apply(f : PartialFunction[widgets.Event, Unit]) =
    new widgets.Listener {
      override def handleEvent(ev : widgets.Event) = f(ev)
    }
  class _ListenerImpl private[Listener](kind : Int) {
    def apply(w : widgets.Widget, l : widgets.Listener) =
      w.addListener(kind, l)
  }
  object Selection extends _ListenerImpl(SWT.Selection)
  object DefaultSelection extends _ListenerImpl(SWT.DefaultSelection)
  object Modify extends _ListenerImpl(SWT.Modify)
  object MouseUp extends _ListenerImpl(SWT.MouseUp)
}