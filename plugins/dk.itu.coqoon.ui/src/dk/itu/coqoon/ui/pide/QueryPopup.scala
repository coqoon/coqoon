package dk.itu.coqoon.ui.pide

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Shell, Composite}
import org.eclipse.swt.graphics.Point
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.jface.layout.{GridDataFactory => GDF}
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
  override def createDialogArea(parent: Composite) = {
    import org.eclipse.swt.layout.GridLayout
    val c = super.createDialogArea(parent).asInstanceOf[Composite]
    c.getLayout.asInstanceOf[GridLayout].numColumns = 2
    val queryText = new Text(c, SWT.BORDER)
    queryText.setFont(JFaceResources.getTextFont)
    this.queryText = Some(queryText)
    queryText.setLayoutData(
      GDF.fillDefaults.align(SWT.FILL, SWT.FILL).grab(
        true, false).create)
    val queryButton = new Button(c, SWT.ARROW | SWT.RIGHT)
    this.queryButton = Some(queryButton)
    queryButton.setLayoutData(
      GDF.fillDefaults.align(SWT.FILL, SWT.FILL).create)
    val queryResults =
      new StyledText(c, SWT.V_SCROLL | SWT.WRAP | SWT.BORDER | SWT.READ_ONLY)
    queryResults.setFont(JFaceResources.getTextFont)
    queryResults.setLayoutData(
      GDF.fillDefaults.align(SWT.FILL, SWT.FILL).grab(true, true).
        span(2, 1).create)
    this.queryResults = Some(queryResults)

    import org.eclipse.swt.events.{ SelectionEvent, SelectionListener }
    object Listener
      extends org.eclipse.swt.events.SelectionListener {
      override def widgetSelected(ev: SelectionEvent) =
        runQuery
      override def widgetDefaultSelected(ev: SelectionEvent) =
        runQuery

      def runQuery() = {
        val query = queryText.getText.trim
        queryText.setText("")
        if (query.length > 0)
          QueryPopup.this.runQuery(query)
      }
    }
    queryText.addSelectionListener(Listener)
    queryButton.addSelectionListener(Listener)

    c
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
