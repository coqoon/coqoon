/* QueryPopup.scala
 * A transient dialog for submitting and displaying PIDE queries
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.pide

import dk.itu.coqoon.ui.utilities.UIXML
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Shell, Composite}
import org.eclipse.swt.graphics.Point
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.jface.resource.JFaceResources

class QueryPopup(
    editor : PIDECoqEditor,
    command : isabelle.Command,
    shell: Shell, position: Point)
    extends PopupDialog(shell, SWT.RESIZE | SWT.ON_TOP, true, false, false,
        false, false, null, null) with OverlayListener {
  import org.eclipse.swt.custom.{StyledText, StyleRange}

  import org.eclipse.jface.viewers.StyledString
  private val resultsText = new StyledString
  private var queryResults: Option[StyledText] = None

  def runQuery(query: String) = {
    editor.setOverlay(Some((Queries.coq_query(command, query), this)))
    appendResult(query + "\n", Stylers.Query)
    (queryButton.toSeq ++ queryText.toSeq).filter(
        !_.isDisposed).foreach(_.setEnabled(false))
  }

  import dk.itu.coqoon.ui.utilities.UIUtils
  override def onResult(result : Either[String, String]) =
    UIUtils.asyncExec {
      val (message, styler) =
        result match {
          case Left(error) =>
            (error, Stylers.Error)
          case Right(result) =>
            (result, Stylers.Result)
        }
      appendResult(message + "\n", styler)
      (queryButton.toSeq ++ queryText.toSeq).filter(
          !_.isDisposed).foreach(_.setEnabled(true))
      queryText.foreach(_.setFocus)
      editor.setOverlay(None)
    }

  def appendResult(result: String, styler: StyledString.Styler) = {
    queryResults.foreach(qr => {
      val scroll = (qr.getCaretOffset == qr.getText.length)
      val oldEnd = qr.getText.length
      qr.append(result)
      qr.setStyleRange({
        val range = new StyleRange
        range.start = oldEnd
        range.length = result.length
        styler.applyStyles(range)
        range
      })
      if (scroll) {
        qr.setTopPixel(Int.MaxValue)
        qr.setCaretOffset(Int.MaxValue)
      }
    })
  }

  import org.eclipse.swt.widgets.{Text, Button}
  private var queryText : Option[Text] = None
  private var queryButton : Option[Button] = None

  override def getDefaultLocation(initialSize: Point) = position
  override def getDefaultSize() = new Point(400, 250)
  override def createDialogArea(parent_ : Composite) = {
    val parent = super.createDialogArea(parent_)
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="2" spacing="0" />
          <!-- We can safely assume that the parent uses a GridLayout -->
          <grid-data grab="true" />
          <text name="queryText" border="true">
            <grid-data h-grab="true" />
          </text>
          <button name="queryButton" style="arrow" direction="right">
            <grid-data />
          </button>
          <styled-text name="queryResults" border="true" wrap="true"
                       read-only="true" scroll="vertical" >
            <grid-data grab="true" h-span="2" />
          </styled-text>
        </composite>, parent)
    val queryText = names.get[Text]("queryText").get
    queryText.setFont(JFaceResources.getTextFont)
    this.queryText = Some(queryText)

    val queryButton = names.get[Button]("queryButton").get
    this.queryButton = Some(queryButton)

    val queryResults = names.get[StyledText]("queryResults").get
    queryResults.setFont(JFaceResources.getTextFont)
    this.queryResults = Some(queryResults)

    def runQuery() = {
      val query = queryText.getText.trim
      queryText.setText("")
      if (query.length > 0)
        QueryPopup.this.runQuery(query)
    }

    import dk.itu.coqoon.ui.utilities.{Event, Listener}
    val l = Listener {
      case Event.Selection(_) => runQuery
      case Event.DefaultSelection(_) => runQuery
    }
    Listener.Selection(queryButton, l)
    Listener.DefaultSelection(queryText, l)

    parent
  }

  override def close() = {
    editor.setOverlay(None)
    super.close()
  }
}

private object Stylers {
  import org.eclipse.jface.viewers.StyledString
  object Query extends StyledString.Styler {
    lazy val blue = dk.itu.coqoon.ui.utilities.UIUtils.Color(64, 64, 200)

    import org.eclipse.swt.graphics.TextStyle
    override def applyStyles(style : TextStyle) = style.foreground = blue
  }
  object Error extends StyledString.Styler {
    lazy val red = dk.itu.coqoon.ui.utilities.UIUtils.Color(200, 0, 0)

    import org.eclipse.swt.graphics.TextStyle
    override def applyStyles(style : TextStyle) = style.foreground = red
  }
  object Result extends StyledString.Styler {
    import org.eclipse.swt.graphics.TextStyle
    override def applyStyles(style : TextStyle) = ()
  }
}
