package dk.itu.coqoon.ui.loadpath

import dk.itu.coqoon.ui.utilities.{Event, Listener}
import dk.itu.coqoon.core.model.ICoqModel
import dk.itu.coqoon.core.utilities.{TryCast, CacheSlot}
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Button, Widget, Composite}
import org.eclipse.core.runtime.{Path, IPath, IAdaptable}
import org.eclipse.core.resources.IProject
import org.eclipse.jface.preference.PreferencePage

class LoadPathOverridePreferencePage
    extends PreferencePage with IWorkbenchPropertyPage {
  private[ui] var overrides = CacheSlot(actualOverrides.toList)
  private def actualOverrides() = TryCast[IProject](element).map(
      ICoqModel.toCoqProject).map(_.getLocalOverrides).getOrElse(Map())

  private var element : IAdaptable = null
  override def getElement : IProject = TryCast[IProject](element).get
  override def setElement(element : IAdaptable) = (this.element = element)

  import org.eclipse.jface.viewers.{
      TableViewer, TextCellEditor, TableViewerColumn}
  private[ui] var tableViewer : Option[TableViewer] = None

  import dk.itu.coqoon.ui.utilities.UIXML
  override def createContents(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="2" equal-width="false" />
          <table-viewer name="tv0">
            <grid-data grab="true" />
            <column style="left" label="External path" />
            <column style="left" label="Override" />
          </table-viewer>
          <composite>
            <grid-data />
            <grid-layout columns="1" />
            <button name="add">
              <grid-data />
              Add
            </button>
            <button name="remove">
              <grid-data />
              Remove
            </button>
          </composite>
        </composite>, parent)
    val tv = names.get[TableViewer]("tv0").get
    this.tableViewer = Some(tv)
    tv.getTable.setLinesVisible(true)
    tv.getTable.setHeaderVisible(true)
    tv.setColumnProperties(Array("from", "to"))
    tv.setCellEditors(Array(
        new TextCellEditor(tv.getTable),
        new TextCellEditor(tv.getTable)))
    tv.setCellModifier(new LoadPathOverrideCellModifier(this))
    for (i <- tv.getTable.getColumns)
      i.pack

    tv.setLabelProvider(new LoadPathOverrideLabelProvider)
    tv.setContentProvider(new LoadPathOverrideContentProvider)
    tv.setInput(overrides)

    Listener.Selection(names.get[Button]("add").get, Listener {
      case Event.Selection(_) =>
        val element =
          new Path("from") -> new Path("to")
        overrides.set(Some(overrides.get :+ element))
        tv.refresh()
    })

    Listener.Selection(names.get[Button]("remove").get, Listener {
      case Event.Selection(_) =>
        tv.getSelection match {
          case s : org.eclipse.jface.viewers.IStructuredSelection =>
            import scala.collection.JavaConversions._
            val o = overrides.get
            val e =
              for ((p : IPath, q : IPath) <- s.iterator.toList)
                yield (p -> q)
            val d = o.diff(e)
            if (d != o) {
              overrides.set(Some(d))
              tv.refresh()
            }
        }
    })

    names.get[Composite]("root").get
  }

  override def performOk() = {
    if (overrides.get != actualOverrides)
      TryCast[IProject](element).map(ICoqModel.toCoqProject).foreach(
          _.setLocalOverrides(overrides.get.toMap))
    true
  }
}

import org.eclipse.jface.viewers.{BaseLabelProvider, ITableLabelProvider}
private class LoadPathOverrideLabelProvider
    extends BaseLabelProvider with ITableLabelProvider {
  override def getColumnImage(element : Any, column : Int) = null
  override def getColumnText(element : Any, column : Int) =
    (element, column) match {
      case ((f : IPath, _), 0) =>
        f.toString
      case ((_, t : IPath), 1) =>
        t.toString
      case _ =>
        null
    }
}

import org.eclipse.jface.viewers.{Viewer, IStructuredContentProvider}
private class LoadPathOverrideContentProvider
    extends IStructuredContentProvider {
  override def dispose() = ()
  override def inputChanged(v : Viewer, oldInput : Any, newInput : Any) =
    v.refresh
  override def getElements(input : Any) = input match {
    case o : CacheSlot[List[(IPath, IPath)]] => o.get.toArray
    case q => Array.empty
  }
}

import org.eclipse.jface.viewers.ICellModifier
private class LoadPathOverrideCellModifier(
    private val pp : LoadPathOverridePreferencePage) extends ICellModifier {
  override def canModify(elem : Any, property : String) = elem match {
    case (_ : IPath, _ : IPath)
        if property == "to" || property == "from" =>
      true
    case _ =>
      false
  }
  override def getValue(elem : Any, property : String) = elem match {
    case (from : IPath, _) if property == "from" =>
      from.toString
    case (_, to : IPath) if property == "to" =>
      to.toString
    case _ =>
      null
  }
  override def modify(elem_ : Any, property : String, newValue : Any) = {
    val elem = elem_ match {
      case i : org.eclipse.swt.widgets.Item => i.getData
      case e => e
    }
    (elem, newValue) match {
      case ((f : IPath, t : IPath), newValue : String) =>
        val n =
          (if (property == "from") new Path(newValue) else f) ->
          (if (property == "to") new Path(newValue) else t)
        var o = pp.overrides.get
        o = o.updated(o.indexOf(f -> t), n)
        pp.overrides.set(Some(o))
        pp.tableViewer.foreach(_.refresh())
      case _ =>
    }
  }
}
