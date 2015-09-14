package dk.itu.coqoon.ui.utilities

import scala.xml
import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.swt.{SWT, layout, widgets}
import org.eclipse.jface.layout.{
  GridDataFactory, GridLayoutFactory, RowLayoutFactory}

class UIXML {
  import UIXML._
  import scala.collection.mutable.{Map => MMap}
  def apply(x : xml.Node, context : widgets.Widget,
      names : Option[MMap[String, widgets.Widget]]) : Unit = {
    val widget : Option[widgets.Widget] = (context, x) match {
      case (parent : widgets.Composite, xml.Elem(_, "label", _, _, _*)) =>
        var flags = getScrollableFlags(x)

        if (x \@ "shadow" == "in") {
          flags |= SWT.SHADOW_IN
        } else if (x \@ "shadow" == "out") {
          flags |= SWT.SHADOW_OUT
        } else if (x \@ "shadow" == "none") {
          flags |= SWT.SHADOW_NONE
        }

        if (x \@ "separator" == "horizontal") {
          flags |= SWT.SEPARATOR | SWT.HORIZONTAL
        } else if (x \@ "separator" == "vertical") {
          flags |= SWT.SEPARATOR | SWT.VERTICAL
        }

        if (x \@ "align" == "left") {
          flags |= SWT.LEFT
        } else if (x \@ "align" == "center") {
          flags |= SWT.CENTER
        } else if (x \@ "align" == "right") {
          flags |= SWT.RIGHT
        }

        if (x \@ "wrap" == "true")
          flags |= SWT.WRAP

        val l = new widgets.Label(parent, flags)
        l.setText(juice(x))
        Some(l)
      case (parent : widgets.Composite, xml.Elem(_, "button", _, _, _*)) =>
        var flags = getControlFlags(x)

        if (x \@ "style" == "arrow") {
          flags |= SWT.ARROW
          if (x \@ "direction" == "up") {
            flags |= SWT.UP
          } else if (x \@ "direction" == "right") {
            flags |= SWT.RIGHT
          } else if (x \@ "direction" == "down") {
            flags |= SWT.DOWN
          } else if (x \@ "direction" == "left") {
            flags |= SWT.LEFT
          }
        } else if (x \@ "style" == "check") {
          flags |= SWT.CHECK
        } else if (x \@ "style" == "push") {
          flags |= SWT.PUSH
        } else if (x \@ "style" == "radio") {
          flags |= SWT.RADIO
        } else if (x \@ "style" == "toggle") {
          flags |= SWT.TOGGLE
        }

        val l = new widgets.Button(parent, flags)
        l.setText(juice(x))
        Some(l)
      case (parent : widgets.Composite, xml.Elem(_, "text", a, _, _*)) =>
        var flags = getScrollableFlags(x)

        /* XXX: should this flag handling go somewhere more generic? */
        /* ICON_CANCEL, ICON_SEARCH, PASSWORD, SEARCH */
        if (x \@ "align" == "left") {
          flags |= SWT.LEFT
        } else if (x \@ "align" == "center") {
          flags |= SWT.CENTER
        } else if (x \@ "align" == "right") {
          flags |= SWT.RIGHT
        }

        if (x \@ "lines" == "single") {
          flags |= SWT.SINGLE
        } else if (x \@ "lines" == "multi") {
          flags |= SWT.MULTI
        }

        if (x \@ "read-only" == "true")
          flags |= SWT.READ_ONLY

        if (x \@ "wrap" == "true")
          flags |= SWT.WRAP

        val t = new widgets.Text(parent, flags)
        t.setText(juice(x))
        Some(t)
      case (parent : widgets.Composite, xml.Elem(_, "composite", _, _, _*)) =>
        val flags = getScrollableFlags(x)
        Some(new widgets.Composite(parent, flags))
      case (parent : widgets.Table, xml.Elem(_, "column", _, _, _*)) =>
        var flags = 0

        if (x \@ "align" == "left") {
          flags |= SWT.LEFT
        } else if (x \@ "align" == "center") {
          flags |= SWT.CENTER
        } else if (x \@ "align" == "right") {
          flags |= SWT.RIGHT
        }

        val tc = new widgets.TableColumn(parent, flags)
        tc.setText(juice(x))
        Some(tc)
      case (parent : widgets.Composite,
          xml.Elem(_, "grid-layout", _, _, _*)) =>
        val gl = GridLayoutFactory.fillDefaults

        val columns = x \@ "columns"
        if (columns != "")
          gl.numColumns(Integer.parseInt(columns))
        gl.equalWidth(x \@ "equal-width" == "true")

        parent.setLayout(gl.create)
        None
      case (context : widgets.Control, xml.Elem(_, "grid-data", _, _, _*)) =>
        val gd = GridDataFactory.fillDefaults().create

        gd.horizontalAlignment =
          if (x \@ "h-align" == "beginning") {
            SWT.BEGINNING
          } else if (x \@ "h-align" == "center") {
            SWT.CENTER
          } else if (x \@ "h-align" == "end") {
            SWT.END
          } else if (x \@ "h-align" == "fill") {
            SWT.FILL
          } else gd.horizontalAlignment

        gd.verticalAlignment =
          if (x \@ "v-align" == "beginning") {
            SWT.BEGINNING
          } else if (x \@ "v-align" == "center") {
            SWT.CENTER
          } else if (x \@ "v-align" == "end") {
            SWT.END
          } else if (x \@ "v-align" == "fill") {
            SWT.FILL
          } else gd.verticalAlignment

        val width = (x \@ "width-hint")
        if (width != "")
          gd.widthHint = Integer.parseInt(width)
        val height = (x \@ "height-hint")
        if (height != "")
          gd.heightHint = Integer.parseInt(height)

        val hSpan = (x \@ "h-span")
        if (hSpan != "")
          gd.horizontalSpan = Integer.parseInt(hSpan)
        val vSpan = (x \@ "v-span")
        if (vSpan != "")
          gd.verticalSpan = Integer.parseInt(vSpan)

        gd.grabExcessHorizontalSpace = (x \@ "h-grab" == "true")
        gd.grabExcessVerticalSpace = (x \@ "v-grab" == "true")

        println(s"$context.setLayoutData($gd)")
        context.setLayoutData(gd)

        None
      case (parent : widgets.Composite,
          xml.Elem(_, "fill-layout", _, _, _*)) =>
        val fl = new org.eclipse.swt.layout.FillLayout

        if (x \@ "type" == "horizontal") {
          fl.`type` = SWT.HORIZONTAL
        } else if (x \@ "type" == "vertical") {
          fl.`type` = SWT.VERTICAL
        }

        parent.setLayout(fl)
        None
      case (parent : widgets.Composite,
          xml.Elem(_, "row-layout", _, _, _*)) =>
        val rl = RowLayoutFactory.fillDefaults

        if (x \@ "type" == "horizontal") {
          rl.`type`(SWT.HORIZONTAL)
        } else if (x \@ "type" == "vertical") {
          rl.`type`(SWT.VERTICAL)
        }

        rl.wrap(x \@ "wrap" == "true")
        rl.pack(x \@ "pack" == "true")
        rl.fill(x \@ "fill" == "true")
        rl.justify(x \@ "justify" == "true")

        parent.setLayout(rl.create)
        None
      case _ =>
        None
    }
    widget.foreach(widget => {
      val name = x \@ "name"
      if (!name.isEmpty)
        names.foreach(_ += (name -> widget))
    })
    widget.flatMap(TryCast[widgets.Control]).foreach(widget => {
      widget.setEnabled(x \@ "enabled" != "false")
      x.child.foreach(apply(_, widget, names))
    })
  }
}
object UIXML extends UIXML {
  def makeNameMap() =
    scala.collection.mutable.Map[String, widgets.Widget]()

  private def getControlFlags(x : xml.Node) =
    if (x \@ "border" == "true") {
      SWT.BORDER
    } else SWT.NONE
  private def getScrollableFlags(x : xml.Node) = {
    val flags =
      (x \@ "scroll") match {
        case "horizontal" => SWT.H_SCROLL
        case "vertical" => SWT.V_SCROLL
        case "both" => SWT.H_SCROLL | SWT.V_SCROLL
        case _ => SWT.NONE
      }
    flags | getControlFlags(x)
  }
  private def juice(x : xml.Node) = {
    val textBlocks =
      (for (t <- x.child if t.isInstanceOf[xml.Text])
        yield t.text)
    textBlocks.mkString(" ").trim.replaceAll(raw"\s+", " ")
  }
}