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
    TryCast[PIDECoqEditor](editor).foreach(editor => {
      val text = editor.getViewer.getTextWidget
      var (rx, ry) = {
        val cl = text.getCaret.getLocation
        println(text.getCaret.getClass.getSimpleName, text.getCaret, cl)
        /* I have no idea why we're 80 pixels short, but we are (at least on my
         * computer!) */
        (cl.x, cl.y + 80)
      }
      var control : Option[Control] = Some(text)
      while (control != None) {
        control.foreach(control => {
          val cl = control.getLocation
          println(control.getClass.getSimpleName, control, cl)
          rx += cl.x
          ry += cl.y
        })
        control = control.flatMap(c => Option(c.getParent))
      }

      val caret = editor.getViewer.getTextWidget.getCaretOffset
      editor.commands.find {
        case (o, c)
            if caret >= o && caret < (o + c.length) =>
          val d = new QueryPopup(editor, c, text.getShell, new Point(rx, ry))
          d.open
          true
        case _ => false
      }
      System.out.println("It is println")
    })
    null
  }
}

import org.eclipse.jface.viewers.StyledString
private object QueryStyler extends StyledString.Styler {
  lazy val blue = dk.itu.coqoon.ui.utilities.UIUtils.Color(64, 64, 200)

  import org.eclipse.swt.graphics.TextStyle
  override def applyStyles(style : TextStyle) = style.foreground = blue
}