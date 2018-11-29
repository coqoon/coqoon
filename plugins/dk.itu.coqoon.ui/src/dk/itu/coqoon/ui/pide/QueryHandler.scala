/* QueryHandler.scala
 * A command handler for creating PIDE query popups
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.EditorHandler
import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Shell, Caret, Widget, Control, Composite}
import org.eclipse.swt.graphics.Point
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.jface.layout.{GridDataFactory => GDF}
import org.eclipse.jface.window.Window
import org.eclipse.jface.dialogs.{Dialog, PopupDialog}
import org.eclipse.jface.viewers.StyledString

class QueryHandler extends EditorHandler {
  private val settings =
    new org.eclipse.jface.dialogs.DialogSettings(
        "Transient query dialog settings")

  import dk.itu.coqoon.ui.utilities.UIUtils
  override def execute(ev : ExecutionEvent) = {
    getEditor.flatMap(
        TryCast[PIDECoqEditor with QueryHost[isabelle.Command]]).foreach(
        editor => {
      val text = editor.getViewer.getTextWidget
      var (rx, ry) = {
        val cl = text.getCaret.getLocation
        /* I have no idea why we're 80 pixels short, but we are (at least on my
         * computer!) */
        (cl.x, cl.y + 80)
      }

      var control : Option[Control] = Some(text)
      while (control != None) {
        control.foreach(control => {
          val cl = control.getLocation
          rx += cl.x
          ry += cl.y
        })
        control = control.flatMap(c => Option(c.getParent))
      }

      val command = editor.findActiveCommand()
      command.foreach(c =>
          new QueryPopup(
              editor, c, text.getShell, new Point(rx, ry), settings).open)
    })
    null
  }
}
