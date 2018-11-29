/* QueryPopup.scala
 * A transient dialog for submitting and displaying PIDE queries
 * Copyright © 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.editors.pide

import dk.itu.coqoon.ui.utilities.UIXML
import dk.itu.coqoon.core.utilities.CacheSlot
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Shell, Composite}
import org.eclipse.swt.graphics.Point
import org.eclipse.jface.dialogs.{PopupDialog, IDialogSettings}
import org.eclipse.jface.resource.JFaceResources

class QueryPopup[C](
    queryHost : QueryHost[C],
    command : C,
    shell: Shell, position: Point,
    settings : IDialogSettings)
    extends PopupDialog(shell, SWT.RESIZE | SWT.ON_TOP, true, true, false,
        false, false, null, null) with QueryListener {
  override protected def getDialogSettings = settings

  import org.eclipse.swt.graphics.Cursor
  private lazy val busyCursor = CacheSlot {
    new Cursor(getShell.getDisplay, SWT.CURSOR_WAIT)
  }
  import org.eclipse.swt.custom.{StyledText, StyleRange}

  import org.eclipse.jface.viewers.StyledString
  private val resultsText = new StyledString
  private var queryResults: Option[StyledText] = None

  def runQuery(query: String) = {
    queryHost.runQuery(command, query, this)
    (getShell +: queryResults.toSeq).filterNot(
          _.isDisposed).foreach(_.setCursor(busyCursor.get))
    appendResult(query, Stylers.Query, true)
    (queryButton.toSeq ++ queryText.toSeq).filterNot(
        _.isDisposed).foreach(_.setEnabled(false))
  }

  import dk.itu.coqoon.ui.utilities.UIUtils
  override def onQueryResult(result : Either[String, Seq[String]]) =
    UIUtils.asyncExec {
      result match {
        case Left(error) =>
          appendResult(error, Stylers.Error, true)
        case Right(results) =>
          results.foreach(appendResult(_, Stylers.Result, true))
      }
      (queryButton.toSeq ++ queryText.toSeq).filterNot(
          _.isDisposed).foreach(_.setEnabled(true))
      (getShell +: queryResults.toSeq).filterNot(
          _.isDisposed).foreach(_.setCursor(null))
      queryText.foreach(_.setFocus)
    }

  def appendResult(result_ : String, styler : StyledString.Styler,
      addToHistory : Boolean = false) = {
    val result = result_ + "\n"
    if (addToHistory)
      queryHost.queryHistory :+= (result_, styler)
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
  override def getDefaultSize() = new Point(500, 350)
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
    queryText = names.get[Text]("queryText")
    queryButton = names.get[Button]("queryButton")
    queryResults = names.get[StyledText]("queryResults")

    queryText.foreach(_.setFont(JFaceResources.getTextFont))
    queryResults.foreach(_.setFont(JFaceResources.getTextFont))

    def runQuery() = queryText.foreach(queryText => {
      val query = queryText.getText.trim
      queryText.setText("")
      if (query.length > 0)
        QueryPopup.this.runQuery(query)
    })

    import dk.itu.coqoon.ui.utilities.{Event, Listener}
    val l = Listener {
      case Event.Selection(_) => runQuery
      case Event.DefaultSelection(_) => runQuery
    }
    queryButton.foreach(Listener.Selection(_, l))
    queryText.foreach(Listener.DefaultSelection(_, l))

    queryHost.queryHistory.foreach {
      case (result, styler) =>
        appendResult(result, styler)
    }

    parent
  }

  override def close() = {
    queryHost.clearQuery
    (getShell +: queryResults.toSeq).filterNot(
          _.isDisposed).foreach(_.setCursor(null))
    busyCursor.asOption.foreach(cursor => {
      cursor.dispose
      busyCursor.clear
    })
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
