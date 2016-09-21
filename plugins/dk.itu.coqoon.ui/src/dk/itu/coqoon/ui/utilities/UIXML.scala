/* UIXML.scala
 * A user interface construction system based on Scala XML documents
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

import scala.xml
import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.swt.{SWT, custom, layout, widgets}
import org.eclipse.jface.viewers
import org.eclipse.jface.layout.{
  GridDataFactory, GridLayoutFactory, RowLayoutFactory}

class UIXML {
  import UIXML._
  def apply(x : xml.Node, context : AnyRef) = {
    lazy val names = new NameMap
    go(x, context, names)
    names
  }
  private def go(
      x : xml.Node, context : AnyRef, names : NameMap) : Option[AnyRef] = {
    val result : Option[AnyRef] = (context, x) match {
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

        first(x, "align").map(unpackTextAlign).foreach(flags |= _)

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
      case (parent : widgets.Composite, xml.Elem(_, "combo", _, _, _*)) =>
        var flags = getControlFlags(x)

        if (x \@ "read-only" == "true") {
          flags |= SWT.READ_ONLY
        }

        val l = new widgets.Combo(parent, flags)
        Some(l)
      case (parent : widgets.Composite, xml.Elem(_, "text", a, _, _*)) =>
        var flags = getScrollableFlags(x)

        /* ICON_CANCEL, ICON_SEARCH, PASSWORD, SEARCH? */
        first(x, "align").map(unpackTextAlign).foreach(flags |= _)
        first(x, "lines").map(unpackTextLines).foreach(flags |= _)

        if (x \@ "read-only" == "true")
          flags |= SWT.READ_ONLY

        if (x \@ "wrap" == "true")
          flags |= SWT.WRAP

        val t = new widgets.Text(parent, flags)
        t.setText(juice(x))
        Some(t)
      case (parent : widgets.Composite,
          xml.Elem(_, "styled-text", a, _, _*)) =>
        var flags = getScrollableFlags(x)

        first(x, "lines").map(unpackTextLines).foreach(flags |= _)

        if (x \@ "selection" == "full")
          flags |= SWT.FULL_SELECTION

        if (x \@ "read-only" == "true")
          flags |= SWT.READ_ONLY

        if (x \@ "wrap" == "true")
          flags |= SWT.WRAP

        val t = new custom.StyledText(parent, flags)
        t.setText(juice(x))
        Some(t)
      case (parent : widgets.Composite, xml.Elem(_, "composite", _, _, _*)) =>
        val flags = getScrollableFlags(x)
        Some(new widgets.Composite(parent, flags))
      case (parent : widgets.Composite,
          xml.Elem(_, "tree-viewer", _, _, _*)) =>
        Some(new viewers.TreeViewer(parent,
            SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.BORDER))
      case (parent : widgets.Composite,
          xml.Elem(_, "combo-viewer", _, _, _*)) =>
        Some(new viewers.ComboViewer(parent, SWT.READ_ONLY | SWT.BORDER))
      case (parent : widgets.Composite, xml.Elem(_, "tab-folder", _, _, _*)) =>
        val flags = getScrollableFlags(x)
        Some(new widgets.TabFolder(parent, flags))
      case (parent : widgets.TabFolder, xml.Elem(_, "tab", _, _, _*)) =>
        val t = new widgets.TabItem(parent, SWT.NONE)
        t.setText(x \@ "label")
        Some(t)

      /* Configuration elements that don't create new widgets */
      case (parent : widgets.Composite,
          xml.Elem(_, "grid-layout", _, _, _*)) =>
        val gl = GridLayoutFactory.fillDefaults.create

        first(x, "columns").map(Integer.parseInt).foreach(gl.numColumns = _)
        gl.makeColumnsEqualWidth = (x \@ "equal-width" == "true")

        first(x, "h-spacing", "spacing").map(
            Integer.parseInt).foreach(gl.horizontalSpacing = _)
        first(x, "v-spacing", "spacing").map(
            Integer.parseInt).foreach(gl.verticalSpacing = _)

        parent.setLayout(gl)
        None
      case (context : widgets.Control, xml.Elem(_, "grid-data", _, _, _*)) =>
        val gd = GridDataFactory.fillDefaults().create

        first(x, "h-align", "align").map(unpackGridAlign).foreach(
            gd.horizontalAlignment = _)
        first(x, "v-align", "align").map(unpackGridAlign).foreach(
            gd.verticalAlignment = _)

        first(x, "width-hint").map(Integer.parseInt).foreach(gd.widthHint = _)
        first(x, "height-hint").map(
            Integer.parseInt).foreach(gd.heightHint = _)

        first(x, "h-span", "span").map(Integer.parseInt).foreach(
            gd.horizontalSpan = _)
        first(x, "v-span", "span").map(Integer.parseInt).foreach(
            gd.verticalSpan = _)

        import java.lang.Boolean.parseBoolean
        first(x, "h-grab", "grab").map(parseBoolean).foreach(
            gd.grabExcessHorizontalSpace = _)
        first(x, "v-grab", "grab").map(parseBoolean).foreach(
            gd.grabExcessVerticalSpace = _)

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
      case (context : widgets.Control,
          xml.Elem(_, "tool-tip", _, _, _*)) =>
        context.setToolTipText(juice(x))
        None

      /* If the context is a viewer, and this element has no particular
       * significance for viewers, try to interpret it again using the viewer's
       * control as the context. */
      case (context : viewers.Viewer, c) =>
        go(c, context.getControl, names)
        None

      /* If the context is a TabItem, then interpret this element using the
       * parent TabFolder; if that produces a control, then install it into the
       * TabItem */
      case (context : widgets.TabItem, c) =>
        go(c, context.getParent, names) match {
          case Some(r : widgets.Control) =>
            context.setControl(r)
          case Some(r : viewers.Viewer) =>
            context.setControl(r.getControl)
          case _ =>
        }
        None

      case _ =>
        None
    }

    result.foreach(
        result => first(x, "name").foreach(names.names += _ -> result))
    result.flatMap(TryCast[widgets.Control]).foreach(
        widget => widget.setEnabled(x \@ "enabled" != "false"))
    result.foreach(
        result => x.child.foreach(go(_, result, names)))
    result
  }
}
object UIXML extends UIXML {
  class NameMap {
    private[UIXML] var names = Map[String, AnyRef]()
    def get[A <: AnyRef](name : String)(implicit a0 : Manifest[A]) =
      names.get(name).flatMap(TryCast[A])
    def getMany[A <: AnyRef](
        names : String*)
        (implicit a0 : Manifest[A]) =
      names.map(n => this.names.get(n).flatMap(TryCast[A]))
  }

  private def first(x : xml.Node, attributes : String*) : Option[String] = {
    for (n <- attributes;
         v <- Option(x \@ n) if !v.isEmpty)
      return Some(v)
    None
  }

  def unpackGridAlign(t : String) = t match {
    case "beginning" => SWT.BEGINNING
    case "center" => SWT.CENTER
    case "end" => SWT.END
    case "fill" => SWT.FILL
    case _ => SWT.NONE
  }
  def unpackTextAlign(t : String) = t match {
    case "left" => SWT.LEFT
    case "center" => SWT.CENTER
    case "right" => SWT.RIGHT
    case _ => SWT.NONE
  }
  def unpackTextLines(t : String) = t match {
    case "single" => SWT.SINGLE
    case "multi" => SWT.MULTI
    case _ => SWT.NONE
  }

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