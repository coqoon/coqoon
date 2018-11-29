package dk.itu.coqoon.ui.pide

trait QueryListener {
  def onQueryResult(result : Either[String, Seq[String]])
}

trait QueryHost[C] {
  import org.eclipse.jface.viewers.StyledString.Styler
  var queryHistory : Seq[(String, Styler)] = Seq()

  def findActiveCommand() : Option[C]

  def runQuery(
      command : C, queryContent : String, responseListener : QueryListener)
  def clearQuery() : Unit = ()
}