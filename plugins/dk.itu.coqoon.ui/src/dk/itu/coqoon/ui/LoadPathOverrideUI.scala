package dk.itu.coqoon.ui

import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Widget, Composite}
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.jface.preference.PreferencePage

class LoadPathOverridePreferencePage
    extends PreferencePage with IWorkbenchPropertyPage {
  override def getElement() = null
  override def setElement(element : IAdaptable) = ()

  import dk.itu.coqoon.ui.utilities.UIXML
  override def createContents(parent : Composite) = {
    import org.eclipse.jface.viewers.TableViewer
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="2" equal-width="false" />
          <composite name="tv-container">
            <grid-data h-grab="true" v-grab="true" />
            <fill-layout />
          </composite>
          <composite>
            <grid-data />
            <grid-layout columns="1" />
            <button>
              <grid-data />
              Add...
            </button>
            <label separator="horizontal">
              <grid-data />
            </label>
            <button>
              <grid-data />
              Remove
            </button>
            <button enabled="false">
              <grid-data />
              Edit...
            </button>
          </composite>
        </composite>, parent)
    val tv = new TableViewer(names.get[Composite]("tv-container").get)
    names.get[Composite]("root").get
  }
}