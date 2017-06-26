package dk.itu.coqoon.ui.project

import dk.itu.coqoon.ui.utilities.UIXML
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.{IWorkbench, IImportWizard}
import org.eclipse.ui.model.{WorkbenchLabelProvider, WorkbenchContentProvider}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Text, Button, Control, Composite}
import org.eclipse.core.runtime.Path
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

  private[project] var directory : Option[Path] = None
  private[project] var name : Option[String] = None
  private[project] var source : Option[Path] = None

  override def createControl(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="3" spacing="5" margin="5" />
          <label>
            Development:
          </label>
          <text name="dt" border="true">
            <grid-data h-align="fill" h-grab="true" />
          </text>
          <button>
            <listener kind="select-directory" target="dt" />
            Browse...
          </button>
          <label separator="horizontal">
            <grid-data h-grab="true" h-align="fill" h-span="3" />
          </label>
          <label name="nl" enabled="false">
            Project name:
          </label>
          <text name="nt" border="true" enabled="false">
            <grid-data h-align="fill" h-grab="true" h-span="2" />
          </text>
          <label name="sl" enabled="false">
            Main source folder:
          </label>
          <text name="st" border="true" enabled="false">
            <grid-data h-align="fill" h-grab="true" />
          </text>
          <button name="sb" enabled="false">
            <listener kind="select-directory" target="st" />
            Browse...
          </button>
          <label enabled="false" wrap="true" align="center" name="extra">
            Other project settings can be configured from the Coq Load Path
            property page once a project has been created for this development.
            <grid-data align="fill" grab="true" h-span="3" width-hint="300" />
          </label>
        </composite>, parent)
    setControl(names.get[Composite]("root").get)
    import dk.itu.coqoon.ui.utilities.{Event, UIUtils, Listener}
    val dt = names.get[Text]("dt").get
    val nt = names.get[Text]("nt").get
    val st = names.get[Text]("st").get

    Listener.Modify(dt, Listener {
      case Event.Modify(_) =>
        val path = dt.getText.trim
        directory =
          if (path.length > 0) {
            Some(new Path(path))
          } else None
        val file = directory.map(_.toFile)
        file match {
          case Some(f) if f.exists && f.isDirectory =>
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
          case Some(f) if f.exists =>
            directory = None
            setErrorMessage(
                s"""The path "$path" is not a directory""")
          case _ =>
            directory = None
            setErrorMessage(s"""The path "$path" does not exist""")
        }
        names.getMany[Control](
            "nl", "nt", "sl", "st", "sb", "extra").flatten.foreach(
                _.setEnabled(directory != None))
        if (directory != None) {
          file.foreach(directory => {
            nt.setText(directory.getName)
            ImportCoqDevelopmentWizardPage.candidateDirectories.find(
                d => directory.list().contains(d)).foreach(
                    cd => st.setText(new File(directory, cd).toString))
          })
        }
        getContainer.updateButtons
    })

    Listener.Modify(nt, Listener {
      case Event.Modify(_) =>
        val name = nt.getText.trim
        if (name == "fred") {
          this.name = None
          setErrorMessage("Not fred")
        } else {
          this.name = Some(name)
          setErrorMessage(null)
        }
        getContainer.updateButtons
    })

    Listener.Modify(st, Listener {
      case Event.Modify(_) =>
        val path = st.getText.trim
        source =
          if (path.length > 0) {
            Some(new Path(path))
          } else None
        val file = source.map(_.toFile)
        (directory, source, file) match {
          case (Some(d), Some(s), Some(f))
              if d.isPrefixOf(s) && f.isDirectory =>
            setErrorMessage(null)
          case (Some(d), _, Some(f)) if f.isDirectory =>
            source = None
            setErrorMessage(
                s"""The path "$path" is not within the development""")
          case (_, _, Some(f)) if f.exists =>
            source = None
            setErrorMessage(
                s"""The path "$path" is not a directory""")
          case _ =>
            source = None
            setErrorMessage(s"""The path "$path" does not exist""")
        }
        getContainer.updateButtons
    })
  }

  override def isPageComplete() =
    (directory != None && name != None && source != None)
}
private object ImportCoqDevelopmentWizardPage {
  private val candidateDirectories = Seq("theories", "src")
}