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
  import dk.itu.coqoon.ui.utilities.UIUtils
  override def execute(ev : ExecutionEvent) = {
    getEditor.flatMap(TryCast[PIDECoqEditor]).foreach(editor => {
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

      /* We want to evaluate the query on the last non-ignored command before
       * the caret. (Ideally we'd just use the command under the caret, but
       * that doesn't work in any recent version of the PIDE library...) */
      val caret = editor.getViewer.getTextWidget.getCaretOffset
      editor.commands.reverse.find {
        case (o, c)
            if o <= caret && !c.is_ignored =>
          val d = new QueryPopup(editor, c, text.getShell, new Point(rx, ry))
          d.open
          true
        case _ => false
      }
    })
    null
  }
}
