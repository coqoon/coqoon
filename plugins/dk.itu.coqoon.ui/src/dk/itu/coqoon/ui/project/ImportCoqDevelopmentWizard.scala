package dk.itu.coqoon.ui.project

import dk.itu.coqoon.ui.utilities.UIXML
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.{IWorkbench, IImportWizard}
import org.eclipse.ui.model.{WorkbenchLabelProvider, WorkbenchContentProvider}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Text, Button, Control, Composite}
import org.eclipse.jface.wizard.{Wizard, WizardPage}
import org.eclipse.jface.viewers.{
  Viewer, CellEditor, TreeViewer, ViewerFilter, ComboBoxCellEditor,
  IStructuredSelection, ITreeContentProvider}
import java.io.File

class ImportCoqDevelopmentWizard extends Wizard with IImportWizard {
  val p = new ImportCoqDevelopmentWizardPage
  override def init(
      workbench : IWorkbench, selection : IStructuredSelection) = ()
  override def addPages() = {
    addPage(p)
  }
  override def performFinish() = {
    false
  }
}

class ImportCoqDevelopmentWizardPage extends WizardPage("icdwp") {
  setTitle("Import an existing Coq development")
  setDescription(
      "Load an existing non-Coqoon Coq development into the workspace.")

  private[project] var directory : Option[File] = None
  private[project] var dispositions : Map[File, Int] = Map()

  import org.eclipse.jface.viewers.{BaseLabelProvider, ITableLabelProvider}
  class FilesystemLabelProvider
      extends BaseLabelProvider with ITableLabelProvider {
    import dk.itu.coqoon.ui.utilities.UIUtils.getWorkbench
    import org.eclipse.ui.ISharedImages
    def getColumnImage(el : AnyRef, column : Int) = (el, column) match {
      case (f : File, 0) if f.isDirectory =>
        getWorkbench.getSharedImages.getImage(ISharedImages.IMG_OBJ_FOLDER)
      case (f : File, 0) if f.isFile =>
        getWorkbench.getSharedImages.getImage(ISharedImages.IMG_OBJ_FILE)
      case _ =>
        null
    }
    override def isLabelProperty(el : AnyRef, property : String) =
      (el, property) match {
        case (f : File, "disposition") =>
          true
        case _ =>
          false
      }
    def getColumnText(el : AnyRef, column : Int) = (el, column) match {
      case (f : File, 0) =>
        f.getName
      case (f : File, 1) =>
        ImportCoqDevelopmentWizardPage.dispositionLabels(
            dispositions.get(f).getOrElse(0))
      case _ =>
        null
    }
  }

  override def createControl(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="3" />
          <label>
            Directory:
          </label>
          <text name="dt" border="true">
            <grid-data h-align="fill" h-grab="true" />
          </text>
          <button name="db">
            Browse...
          </button>
          <tree-viewer name="tv">
            <grid-data align="fill" grab="true" h-span="3" height-hint="300" />
            <column label="Folder" style="left" />
            <column label="Type" style="left" />
          </tree-viewer>
        </composite>, parent)
    setControl(names.get[Composite]("root").get)
    import dk.itu.coqoon.ui.utilities.{Event, UIUtils, Listener}
    val dt = names.get[Text]("dt").get
    val db = names.get[Button]("db").get
    val tv = names.get[TreeViewer]("tv").get

    Listener.Modify(dt, Listener {
      case Event.Modify(_) =>
        val path = dt.getText.trim
        directory =
          if (path.length > 0) {
            Some(new File(path))
          } else None
        directory match {
          case Some(f : File) if f.exists && f.isDirectory =>
            val files = f.list()
            if (files.contains(".project") ||
                files.contains(".coqoonProject")) {
              directory = None
              setErrorMessage(
                  s"""The directory "$path" already contains """ +
                  "an Eclipse or Coqoon project")
            } else {
              setErrorMessage(null)
            }
          case Some(f : File) if f.exists =>
            directory = None
            setErrorMessage(
                s"""The path "$path" is not a directory""")
          case _ =>
            directory = None
            setErrorMessage(s"""The path "$path" does not exist""")
        }
        if (getErrorMessage == null) {
          directory.foreach(tv.setInput)
        } else tv.setInput(null)
        dispositions = Map()
        tv.refresh()
        getContainer.updateButtons
    })

    Listener.Selection(db, Listener {
      case Event.Selection(_) =>
        UIUtils.Dialog.directory(
            "Select a directory",
            "Select a directory containing a Coq development.").foreach(
                dt.setText)
    })

    tv.getTree.setHeaderVisible(true)
    tv.setContentProvider(new FilesystemContentProvider)
    tv.setLabelProvider(new FilesystemLabelProvider)
    tv.setFilters(Array(
        FilesystemExcludeHiddenFilter, FilesystemDirectoryFilter))
    tv.setColumnProperties(Array("resource", "disposition"))
    val cbce = new ComboBoxCellEditor(
        tv.getTree, ImportCoqDevelopmentWizardPage.dispositionLabels,
        SWT.BORDER | SWT.READ_ONLY)
    tv.setCellEditors(Array[CellEditor](null, cbce))
    tv.setCellModifier(new org.eclipse.jface.viewers.ICellModifier {
      override def canModify(el : AnyRef, property : String) =
        (property == "disposition")
      override def getValue(el : AnyRef, property : String) =
        (el, property) match {
          case (f : File, "disposition") =>
            Int.box(dispositions.get(f).getOrElse(0))
          case _ => null
        }
      override def modify(el_ : AnyRef, property : String, value : AnyRef) = {
        val el = el_ match {
          case i : org.eclipse.swt.widgets.Item => i.getData
          case i => i
        }
        (el, property, value) match {
          case (f : File, "disposition", i : Integer) =>
            dispositions += (f -> i)
            tv.update(f, Array("disposition"))
          case _ =>
        }
      }
    })

    for (i <- tv.getTree.getColumns)
      i.pack
  }

  override def isPageComplete() = (directory != None)
}
object ImportCoqDevelopmentWizardPage {
  final val dispositionLabels = Array[String]("", "Coq source folder")
}

class FilesystemContentProvider extends ITreeContentProvider {
  override def dispose() = ()
  override def inputChanged(
      v: Viewer, oldInput: AnyRef, newInput: AnyRef) = ()
  override def getChildren(el: AnyRef) : Array[Object] =
    TryCast[File](el) match {
      case Some(f) if f.isDirectory =>
        f.listFiles.sortBy(f => (f.isFile, f.getName.toLowerCase)).toArray
      case _ =>
        Array.empty
    }
  override def getElements(el: AnyRef) = getChildren(el)
  override def getParent(el: AnyRef) =
    TryCast[File](el).map(_.getParentFile).orNull
  override def hasChildren(el: AnyRef) =
    TryCast[File](el).flatMap(f => Option(f.list)).map(
        !_.isEmpty).getOrElse(false)
}

object FilesystemExcludeHiddenFilter extends ViewerFilter {
  override def select(viewer : Viewer, parent : AnyRef, el : AnyRef) =
    el match {
      case f : File if f.isHidden() =>
        false
      case _ =>
        true
    }
}

object FilesystemDirectoryFilter extends ViewerFilter {
  override def select(viewer : Viewer, parent : AnyRef, el : AnyRef) =
    el match {
      case f : File if f.isDirectory() =>
        true
      case _ =>
        false
    }
}