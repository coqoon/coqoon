/* LoadPathUI.scala
 * User interfaces for viewing and manipulating Coq project load paths
 * Copyright © 2013, 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.loadpath

import org.eclipse.core.resources.IProject
import dk.itu.coqoon.ui.utilities.{Event, UIXML, UIUtils, Listener}
import dk.itu.coqoon.ui.OnlyFoldersFilter
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.{TryCast, CacheSlot}

import org.eclipse.jface.wizard._
import org.eclipse.jface.viewers._
import org.eclipse.jface.window.Window

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{Text, Composite, Button}
import org.eclipse.core.runtime.{Path, IPath, IAdaptable}
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.jface.preference.PreferencePage

protected object LoadPathModel {
  abstract class LPBase(private val parent : Option[LPProvider]) {
    final def getParent() : Option[LPProvider] = parent
    def getChildren() : Seq[LPBase] = Seq()
    def hasChildren() : Boolean = getChildren.length > 0
    def hasAncestor[A]()(implicit a0 : Manifest[A]) : Boolean =
      getParent match {
        case Some(f : A) => true
        case Some(f) => f.hasAncestor[A]
        case None => false
      }
  }

  /* Load path model entries backed directly by an ICoqLoadPathProvider */
  abstract class LPProvider(
      parent : Option[LPProvider], index : Int) extends LPBase(parent) {
    def getIndex() = index
  }

  /* Load path model entries backed by an ICoqLoadPathProvider whose children
   * represent namespaces and locations */
  abstract class LPNSChild(parent : Option[LPProvider], index : Int)
      extends LPProvider(parent, index) {
    def getLoadPath() : Seq[LoadPathEntry]

    override def getChildren = {
      var result = Seq[LPBase]()
      var children = getLoadPath
      while (children != Nil) children = children match {
        case Nil =>
          Nil
        case head :: tail =>
          result ++= Seq(
              NamespaceSLPE(Some(this), head),
              LocationSLPE(Some(this), head))
          if (tail != Nil)
            result :+= SeparatorSLPE(Some(this))
          tail
      }
      result
    }
    override def hasChildren = getLoadPath.size > 0
  }

  case class AbstractLPE(parent : Option[LPProvider], identifier : String,
      index : Int) extends LPNSChild(parent, index) {
    override def getLoadPath = AbstractLoadPath(identifier).getLoadPath
  }

  import org.eclipse.core.resources.IFolder
  case class SourceLPE(parent : Option[LPProvider], folder : IFolder,
      output : Option[IFolder], index : Int)
      extends LPProvider(parent, index) {
    override def getChildren = Seq(OutputSLPE(Some(this), output))
    override def hasChildren = true
  }

  case class DefaultOutputLPE(parent : Option[LPProvider],
      folder : IFolder, index : Int) extends LPProvider(parent, index)

  case class ProjectLPE(parent : Option[LPProvider], project : IProject,
      index : Int) extends LPProvider(parent, index) {
    override def getChildren = Option(
        ICoqModel.toCoqProject(project)).toSeq.map(
            _.getLoadPathProviders).flatMap(translate(Some(this), _))
    override def hasChildren = getChildren.size > 0
  }

  case class ExternalLPE(parent : Option[LPProvider], fsPath : IPath,
      dir : Seq[String], index : Int) extends LPProvider(parent, index) {
    override def getChildren =
      Seq(NamespaceSLPE(Some(this), LoadPathEntry(fsPath, dir)))
    override def hasChildren = true
  }

  case class OutputSLPE(parent : Option[LPProvider],
      output : Option[IFolder]) extends LPBase(parent)
  case class NamespaceSLPE(parent : Option[LPProvider],
      cl : LoadPathEntry) extends LPBase(parent)
  case class LocationSLPE(parent : Option[LPProvider],
      cl : LoadPathEntry) extends LPBase(parent)
  case class SeparatorSLPE(parent : Option[LPProvider]) extends LPBase(parent)

  def translate(parent : Option[LPProvider],
      providers : Seq[LoadPathProvider]) : Seq[LPProvider] =
    for ((provider, index) <- providers.zipWithIndex)
      yield translate(parent, provider, index)

  def translate(parent : Option[LPProvider],
      provider : LoadPathProvider, index : Int) : LPProvider =
    provider match {
      case AbstractLoadPath(a) => AbstractLPE(parent, a, index)
      case SourceLoadPath(a, b) => SourceLPE(parent, a, b, index)
      case DefaultOutputLoadPath(a) => DefaultOutputLPE(parent, a, index)
      case ProjectLoadPath(a) => ProjectLPE(parent, a, index)
      case ExternalLoadPath(a, b) => ExternalLPE(parent, a, b, index)
    }
}

private class LoadPathLabelProvider extends StyledCellLabelProvider {
  import LoadPathModel._
  import LoadPathLabelProvider._

  override def update(cell : ViewerCell) = cell.getElement match {
    case l : LPBase =>
      val s = new StyledString
      l match {
        case AbstractLPE(_, identifier, i) =>
          s.append(s"${i + 1}. Library: ")
          AbstractLoadPath(identifier).getImplementation match {
            case Some(impl) =>
              s.append(impl.getName, ColourStyler(VALID))
            case None =>
              s.append(identifier, ColourStyler(ERROR))
          }
        case SourceLPE(_, folder, _, i) =>
          s.append(s"${i + 1}. Source folder: ")
          s.append(
              folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case DefaultOutputLPE(_, folder, i) =>
          s.append(s"${i + 1}. Default output folder: ")
          s.append(
              folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case ProjectLPE(_, project, i) =>
          import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}
          s.append(s"${i + 1}. Project: ")
          val styler = ColourStyler(
            if (project.exists() && project.hasNature(CMI.NATURE_COQ))
              VALID else ERROR)
          s.append(project.getName.toString, styler)
        case ExternalLPE(_, fsPath, _, i) =>
          import dk.itu.coqoon.core.project.CoqNature
          s.append(s"${i + 1}. External development: ")
          val styler =
            ColourStyler(if (fsPath.toFile.exists) VALID else ERROR)
          s.append(fsPath.addTrailingSeparator.toString, styler)

        case NamespaceSLPE(_, lpe) =>
          s.append("Namespace: ")
          val label =
            if (lpe.coqdir != Nil) {
              lpe.coqdir.mkString(".")
            } else "(root)"
          s.append(label, ColourStyler(NSLOC))
        case LocationSLPE(_, lpe) =>
          s.append("Location: ")
          s.append(lpe.path.toString, ColourStyler(NSLOC))
        case OutputSLPE(_, output) =>
          s.append("Output folder: ")
          s.append(output.map(
                  _.getProjectRelativePath.toString).getOrElse("(default)"),
              ColourStyler(NSLOC))
        case SeparatorSLPE(_) =>
          s.append("--")
        case _ =>
      }
      cell.setText(s.getString)
      cell.setStyleRanges(s.getStyleRanges)
      super.update(cell)
    case _ =>
      super.update(cell)
  }
}
private object LoadPathLabelProvider {
  import dk.itu.coqoon.ui.utilities.UIUtils.{Color => ColorI}
  import org.eclipse.swt.graphics.{Color, TextStyle}

  final val VALID = ColorI(0, 128, 0)
  final val ERROR = ColorI(255, 0, 0)
  final val NSLOC = ColorI(0, 0, 255)

  case class ColourStyler(c : Color) extends StyledString.Styler {
    override def applyStyles(s : TextStyle) = (s.foreground = c)
  }
}

private class LoadPathContentProvider extends ITreeContentProvider {
  import LoadPathModel._

  override def dispose = ()

  override def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) =
    viewer.refresh

  override def getElements(input : Any) = input match {
    case s : Seq[LoadPathProvider] =>
      translate(None, s).toArray
    case _ => Array.empty
  }

  override def getChildren(parent : Any) =
    TryCast[LPBase](parent).toSeq.flatMap(_.getChildren).toArray

  override def getParent(child : Any) =
    TryCast[LPBase](child).flatMap(_.getParent).getOrElse(null)
  override def hasChildren(parent : Any) =
    TryCast[LPBase](parent).map(_.hasChildren).getOrElse(false)
}

class LoadPathConfigurationPage
    extends PreferencePage with IWorkbenchPropertyPage {
  import LoadPathModel._

  private var loadPath = CacheSlot(actualLoadPath.toBuffer)
  private def actualLoadPath() = TryCast[IProject](element).map(
      ICoqModel.toCoqProject).map(_.getLoadPathProviders).getOrElse(Nil)

  override def performOk() = {
    if (loadPath.get.toSet != actualLoadPath.toSet)
      TryCast[IProject](element).map(ICoqModel.toCoqProject).foreach(
          _.setLoadPathProviders(loadPath.get, null))
    true
  }

  private var element : IAdaptable = null
  override def getElement : IProject = TryCast[IProject](element).get
  override def setElement(element : IAdaptable) = (this.element = element)
  override def createContents(c : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="2" />
          <composite name="tvc-container">
            <grid-data h-grab="true" v-grab="true" />
            <fill-layout />
          </composite>
          <composite>
            <grid-data />
            <grid-layout columns="2" equal-width="true" />
            <button name="afb">
              <grid-data h-span="2" />
              Add...
            </button>
            <label separator="horizontal">
              <grid-data h-span="2" />
            </label>
            <button name="dfb">
              <grid-data h-span="2" />
              Remove
            </button>
            <button enabled="false">
              <grid-data h-span="2" />
              Edit...
            </button>
            <button name="upb">
              <grid-data />
              Up
            </button>
            <button name="dob">
              <grid-data />
              Down
            </button>
            <label align="center" read-only="true" wrap="true">
              <grid-data h-span="2" v-grab="true" width-hint="100" />
              Higher entries take priority over lower ones. Use the Up and Down
              buttons to reorder entries.
            </label>
          </composite>
        </composite>, c)

    val tv1 = new TreeViewer(
        names.get[Composite]("tvc-container").get, SWT.SINGLE | SWT.BORDER)
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider)
    tv1.setInput(loadPath.get)

    Listener.Selection(names.get[Button]("afb").get, Listener {
      case Event.Selection(_) =>
        val wiz = new NewLoadPathWizard(getElement)
        if (new WizardDialog(c.getShell, wiz).open == Window.OK) {
          val lp = loadPath.get
          wiz.getResult.filter(r => !lp.contains(r)).foreach(lp.append(_))
          tv1.refresh()
        }
    })

    val dfb = names.get[Button]("dfb").get
    Listener.Selection(dfb, Listener {
      case Event.Selection(_) =>
        Option(tv1.getSelection.
            asInstanceOf[TreeSelection].getFirstElement) match {
          case Some(p : LPProvider) =>
            loadPath.get.remove(p.getIndex)
            tv1.refresh()
          case _ =>
        }
    })

    val upb = names.get[Button]("upb").get
    Listener.Selection(upb, Listener {
      case Event.Selection(_) =>
        Option(tv1.getSelection.
            asInstanceOf[TreeSelection].getFirstElement) match {
          case Some(p : LPProvider)
              if p.getIndex > 0 =>
            val element = loadPath.get.remove(p.getIndex)
            loadPath.get.insert(p.getIndex - 1, element)
            tv1.refresh()
            tv1.setSelection(new TreeSelection(new TreePath(Array(
                loadPath.get,
                translate(None, element, p.getIndex - 1)))))
          case _ =>
        }
    })

    val dob = names.get[Button]("dob").get
    Listener.Selection(dob, Listener {
      case Event.Selection(_) =>
        Option(tv1.getSelection.
            asInstanceOf[TreeSelection].getFirstElement) match {
          case Some(p : LPProvider)
              if p.getIndex < (loadPath.get.length - 1) =>
            val element = loadPath.get.remove(p.getIndex)
            loadPath.get.insert(p.getIndex + 1, element)
            tv1.refresh()
            tv1.setSelection(new TreeSelection(new TreePath(Array(
                loadPath.get,
                translate(None, element, p.getIndex + 1)))))
          case _ =>
        }
    })

    tv1.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) = {
        val selection =
          Option(ev.getSelection.asInstanceOf[TreeSelection].getFirstElement)
        selection match {
          case Some(_ : DefaultOutputLPE) =>
            /* Deleting default output entries is really not a good idea */
            dfb.setEnabled(false)
          case Some(e : LPProvider) if e.getParent != None =>
            /* Only allow top-level entries to be deleted */
            dfb.setEnabled(false)
          case _ =>
            dfb.setEnabled(true)
        }
        selection.flatMap(TryCast[LPProvider]).foreach(p => {
          /* Only allow top-level entries to be moved up and down */
          if (p.getParent == None) {
            upb.setEnabled(p.getIndex != 0);
            dob.setEnabled(p.getIndex != loadPath.get.length - 1)
          } else Seq(upb, dob).foreach(_.setEnabled(false))
        })
      }
    })

    names.get[Composite]("root").get
  }
}

class NewLoadPathWizard(val project : IProject) extends Wizard {
  private val selectionPage = new NLPSelectionPage
  addPage(selectionPage)
  setForcePreviousAndNextButtons(true)

  private var result : Option[LoadPathProvider] = None
  def getResult() = result

  override def canFinish = selectionPage.isPageComplete &&
      Option(selectionPage.getNextPage).exists(_.isPageComplete)
  override def performFinish = {
    result =
      if (canFinish()) {
        selectionPage.getNextPage.createLoadPathEntry
      } else None
    canFinish
  }
}

class NLPSelectionPage extends WizardPage(
    "nlpSP", "Selection", null) {
  setDescription(
      "Select a kind of load path entry to add to this project.")

  setPageComplete(false)
  val subpages = Array(
      new NLPAbstractEntryPage,
      new NLPSourceEntryPage,
      new NLPProjectEntryPage,
      new NLPExternalEntryPage)

  private var nextPage : Option[NLPWizardPage] = None
  override def getNextPage : NLPWizardPage = nextPage.orNull

  override def createControl(parent : Composite) = {
    val lv = new ListViewer(parent, SWT.BORDER)
    lv.setContentProvider(new IStructuredContentProvider {
      override def dispose = ()
      override def inputChanged(v : Viewer, o : Any, n : Any) = ()
      override def getElements(input : Any) = input match {
        case a : Array[AnyRef] => a
        case _ => Array.empty
      }
    })
    lv.setLabelProvider(new LabelProvider {
      override def getText(element : Any) = TryCast[NLPWizardPage](
          element).map(_.getTitle).getOrElse(super.getText(element))
    })
    lv.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) = {
        nextPage = TryCast[IStructuredSelection](ev.getSelection).map(
            _.getFirstElement).flatMap(TryCast[NLPWizardPage])
        nextPage.foreach(_.setWizard(getWizard))
        setPageComplete(nextPage != None)
        getContainer.updateButtons()
      }
    })
    lv.setInput(subpages)
    setControl(lv.getControl)
  }
}

import org.eclipse.jface.resource.ImageDescriptor

abstract class NLPWizardPage(
    name : String, title : String, descriptor : ImageDescriptor = null)
        extends WizardPage(name, title, descriptor) {
  override def getWizard() : NewLoadPathWizard =
    super.getWizard.asInstanceOf[NewLoadPathWizard]

  /* Calling this method should be basically free, so it also serves as the
   * implementation of isPageComplete */
  def createLoadPathEntry() : Option[LoadPathProvider]
  override def isPageComplete() = (createLoadPathEntry != None)
}

class NLPAbstractEntryPage extends NLPWizardPage(
    "nlpAEP", "Abstract dependency") {
  setDescription(
      "Add an identifier for a dependency which the environment must provide.")

  private var identifier : Option[String] = None
  override def createLoadPathEntry = identifier.map(AbstractLoadPath(_))

  override def createControl(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout />
          <button name="b1" style="radio">
            <grid-data h-grab="true" />
            Select from the list of available abstract dependencies
          </button>
          <composite name="lv-container">
            <grid-data h-grab="true" v-grab="true" />
            <fill-layout />
          </composite>
          <button name="b2" style="radio">
            <grid-data h-grab="true" />
            Manually specify the identifier of an abstract dependency
          </button>
          <text name="at" border="true">
            <grid-data h-grab="true" />
          </text>
        </composite>, parent)

    val lv = new TreeViewer(
        names.get[Composite]("lv-container").get, SWT.SINGLE | SWT.BORDER)
    lv.setContentProvider(new FuturisticContentProvider[TreeViewer] {
      override def actuallyGetChildren(element : AnyRef) =
        element match {
          case a : LoadPathManager =>
            for (i <- a.getProviders;
                 f <- Some(i.getImplementations))
              yield (if (f.size == 1) f.head else i)
          case p : LoadPathImplementationProvider => p.getImplementations
          case _ => Seq()
        }
      override def getParent(element : AnyRef) =
        TryCast[LoadPathImplementation](
            element).map(_.getProvider).orNull
    })
    lv.setLabelProvider(new FuturisticLabelProvider {
      override def actuallyGetText(element : AnyRef) = element match {
        case p : LoadPathImplementationProvider => Some(p.getName)
        case i : LoadPathImplementation => Some(i.getName)
        case _ => None
      }
      import org.eclipse.ui.ISharedImages
      override def actuallyGetImage(element : AnyRef) = element match {
        case p : LoadPathImplementationProvider =>
          Some((UIUtils.getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJ_FOLDER), false))
        case i : LoadPathImplementation =>
          Some((UIUtils.getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJ_ELEMENT), false))
        case _ => None
      }
    })
    lv.setInput(AbstractLoadPathManager.getInstance)

    val at = names.get[Text]("at").get
    Listener.Modify(at, Listener {
      case Event.Modify(_) =>
        val identifier = at.getText.trim
        var success = false
        if (!identifier.isEmpty) {
          val impl = AbstractLoadPath(at.getText.trim).getImplementation
          import LoadPathImplementation._
          impl.map(_.getLoadPath()) match {
            case Some(Right(_)) =>
              success = true
            case Some(Left(VersionMismatch)) =>
              setErrorMessage(impl.get.getName + " is installed, " +
                  "but is not compatible with the version you requested")
            case Some(Left(Broken)) =>
              setErrorMessage(impl.get.getName + " is installed, " +
                  "but doesn't work")
            case Some(Left(_ : NotAvailable)) =>
              setErrorMessage(impl.get.getName + " is not installed")
            case None =>
              setErrorMessage(s"""No implementation could be found for "${identifier}"""")
          }
        }
        NLPAbstractEntryPage.this.identifier =
          if (success) {
            setErrorMessage(null)
            Some(identifier)
          } else None
        getContainer.updateButtons
    })

    lv.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) =
        TryCast[IStructuredSelection](ev.getSelection).map(
            _.getFirstElement) match {
          case Some(i : LoadPathImplementation) =>
            at.setText(i.getIdentifier)
          case _ =>
        }
    })

    val b1 = names.get[Button]("b1").get
    Listener.Selection(b1, Listener {
      case Event.Selection(_) =>
        lv.getControl.setEnabled(true)
        at.setEnabled(false)
    })
    b1.setSelection(true)

    val b2 = names.get[Button]("b2").get
    Listener.Selection(b2, Listener {
      case Event.Selection(_) =>
        at.setEnabled(true)
        lv.getControl.setEnabled(false)
    })

    at.setEnabled(false);

    setControl(names.get[Composite]("root").get)
  }
}

class NLPSourceEntryPage extends NLPWizardPage(
    "nlpSEP", "Another source folder in this project") {
  setDescription(
      "Add an additional source folder to this project.")

  import org.eclipse.core.resources.IFolder
  private var folder : Option[IFolder] = None
  private var output : Option[IFolder] = None
  override def createLoadPathEntry =
    (folder, output) match {
      case (Some(f), Some(o)) if o.exists() =>
        Some(SourceLoadPath(f, Some(o)))
      case (Some(f), None) =>
        Some(SourceLoadPath(f, None))
      case _ =>
        None
    }

  override def createControl(parent : Composite) = {
    import org.eclipse.ui.dialogs.ISelectionStatusValidator
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="3" />
          <label>
            Source folder:
          </label>
          <text name="st" border="true">
            <grid-data h-grab="true" />
          </text>
          <button name="sb" style="push">
            Browse...
          </button>
          <label>
            Output folder:
          </label>
          <text name="ot" border="true">
            <grid-data h-grab="true" />
          </text>
          <button name="ob" style="push">
            Browse...
          </button>
        </composite>, parent)

    val st = names.get[Text]("st").get
    Listener.Modify(st, Listener {
      case Event.Modify(_) =>
        val path = st.getText.trim
        folder =
          if (path.length > 0) {
            Some(getWizard.project.getFolder(new Path(path)))
          } else None
        folder match {
          case Some(f : IFolder) if f.exists =>
            setErrorMessage(null)
          case _ =>
            folder = None
            setErrorMessage(
                "The folder \"" + path + "\" does not exist in this project")
        }
        getContainer.updateButtons
    })
    val sb = names.get[Button]("sb").get
    Listener.Selection(sb, Listener {
      case Event.Selection(_) =>
        val ed = UIUtils.createWorkspaceElementDialog(sb.getShell)
        ed.addFilter(new OnlyFoldersFilter)
        ed.setInput(getWizard.project)
        if (ed.open == Window.OK)
          Option(ed.getFirstResult) match {
            case Some(f : IFolder) =>
              st.setText(f.getProjectRelativePath.toString)
            case _ =>
          }
    })

    val ot = names.get[Text]("ot").get
    Listener.Modify(ot, Listener {
      case Event.Modify(_) =>
        val path = ot.getText.trim
        val folder =
          if (path.length > 0) {
            Some(getWizard.project.getFolder(new Path(path)))
          } else None
        output = folder
        if (!folder.forall(_.exists())) {
          setErrorMessage(
                "The folder \"" + path + "\" does not exist in this project")
        } else setErrorMessage(null)
        getContainer.updateButtons
    })
    val ob = names.get[Button]("ob").get
    Listener.Selection(ob, Listener {
      case Event.Selection(_) =>
        val ed = UIUtils.createWorkspaceElementDialog(ob.getShell)
        ed.addFilter(new OnlyFoldersFilter)
        ed.setInput(getWizard.project)
        if (ed.open == Window.OK)
          Option(ed.getFirstResult) match {
            case Some(f : IFolder) =>
              ot.setText(f.getProjectRelativePath.toString)
            case _ =>
          }
    })

    setControl(names.get[Composite]("root").get)
  }
}

class NLPProjectEntryPage extends NLPWizardPage(
    "nlpPEP", "Another Coqoon project") {
  setDescription(
      "Add a dependency on another Coqoon project in the workspace.")
  private var project : Option[IProject] = None
  override def createLoadPathEntry = project.map(ProjectLoadPath(_))

  override def createControl(parent : Composite) = {
    import org.eclipse.ui.model.{
      WorkbenchLabelProvider, WorkbenchContentProvider}
    val tv = new TreeViewer(parent, SWT.BORDER | SWT.SINGLE)
    tv.setLabelProvider(
        WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider())
    tv.setContentProvider(new WorkbenchContentProvider {
      import org.eclipse.core.resources.IWorkspaceRoot
      override def getChildren(el : AnyRef) = el match {
        case e : IWorkspaceRoot => super.getChildren(el)
        case _ => Array()
      }
    })
    tv.setInput(
        org.eclipse.core.resources.ResourcesPlugin.getWorkspace.getRoot)
    tv.addFilter(new ViewerFilter {
      import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}
      override def select(
          viewer : Viewer, parent : AnyRef, element : AnyRef) =
        element match {
          case e : IProject if e.isOpen && e.hasNature(CMI.NATURE_COQ) =>
            true
          case _ => false
        }
    })
    tv.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) = {
        val p = TryCast[IStructuredSelection](ev.getSelection).map(
            _.getFirstElement).flatMap(TryCast[IProject])
        project = p
        getContainer.updateButtons()
      }
    })

    setControl(tv.getControl)
  }
}

class NLPExternalEntryPage extends NLPWizardPage(
    "nlpEEP", "External development") {
  setDescription(
      "Add a dependency on a Coq development built outside of Coqoon.")

  private var fspath : Option[IPath] = None
  private var dir : Option[Seq[String]] = None
  override def createLoadPathEntry =
    fspath.map(ExternalLoadPath(_, dir.getOrElse(Seq())))

  override def createControl(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="3" />
          <label>
            Folder:
          </label>
          <text name="ft" border="true">
            <grid-data h-grab="true" />
          </text>
          <button name="fb" style="push">
            Browse...
          </button>
          <label>
            Coq namespace:
          </label>
          <text name="nt" border="true">
            <grid-data h-grab="true" h-span="2" />
          </text>
        </composite>, parent)

    val ft = names.get[Text]("ft").get
    Listener.Modify(ft, Listener {
      case Event.Modify(_) =>
        val path = ft.getText.trim
        fspath =
          if (path.length > 0) {
            Some(new Path(path))
          } else None
        fspath match {
          case Some(f : IPath) if f.toFile.exists =>
            setErrorMessage(null)
          case _ =>
            fspath = None
            setErrorMessage(
                "The path \"" + path + "\" does not exist")
        }
        getContainer.updateButtons
    })
    val fb = names.get[Button]("fb").get
    Listener.Selection(fb, Listener {
      case Event.Selection(_) =>
        import org.eclipse.swt.widgets.DirectoryDialog
        val ed = new DirectoryDialog(fb.getShell, SWT.OPEN)
        Option(ed.open) match {
          case Some(p : String) =>
            ft.setText(p)
          case _ =>
        }
    })

    val nt = names.get[Text]("nt").get
    Listener.Modify(nt, Listener {
      case Event.Modify(_) =>
        val coqdir = nt.getText.trim
        val split = coqdir.split('.')

        var error : Option[String] = None
        for (part <- split if error == None) {
          if (part.length == 0) {
            error = Some("Coq namespace components cannot be empty")
          } else if (!part.head.isLetter || !part.forall(_.isLetterOrDigit)) {
            error = Some("The Coq namespace component \"" + part + "\" is invalid")
          }
        }

        error.foreach(setErrorMessage)
        dir =
          if (error == None) {
            Some(split)
          } else None
        getContainer.updateButtons
    })

    setControl(names.get[Composite]("root").get)
  }
}

abstract class FuturisticContentProvider[A <: Viewer](
    implicit a0 : Manifest[A]) extends ITreeContentProvider {
  override def dispose() = viewer.foreach(inputChanged(_, null, null))

  private var viewer : Option[A] = None
  override def inputChanged(viewer : Viewer,
      oldInput : AnyRef, newInput : AnyRef) = {
    this.viewer.foreach(unsubscribe(_, oldInput))
    this.viewer = TryCast[A](viewer)
    this.viewer.foreach(unsubscribe(_, newInput))
  }

  protected def subscribe(viewer : A, input : AnyRef) = ()
  protected def unsubscribe(viewer : A, input : AnyRef) = ()

  override final def getElements(input : AnyRef) =
    actuallyGetElements(input).toArray
  def actuallyGetElements(input : AnyRef) : Seq[AnyRef] =
    actuallyGetChildren(input)

  override final def getChildren(element : AnyRef) =
    actuallyGetChildren(element).toArray
  def actuallyGetChildren(element : AnyRef) : Seq[AnyRef]

  override def hasChildren(element : AnyRef) =
    !actuallyGetChildren(element).isEmpty

  override def getParent(element : AnyRef) : AnyRef = null
}

class FuturisticLabelProvider extends LabelProvider {
  override def getText(element : AnyRef) = actuallyGetText(element).orNull
  def actuallyGetText(element : AnyRef) : Option[String] = None

  override def dispose() = {
    for ((_, image) <- cache)
      image.dispose
    cache = Map()
  }

  import org.eclipse.swt.graphics.Image
  private var cache : Map[AnyRef, Image] = Map()
  override def getImage(element : AnyRef) =
    cache.get(element) match {
      case Some(image) => image
      case None => actuallyGetImage(element) match {
        case Some((image, manage)) =>
          if (manage)
            cache += (element -> image)
          image
        case _ => null
      }
    }

  /* Returns an image and a boolean specifying whether or not this image should
   * be cached and automatically disposed */
  def actuallyGetImage(
      element : AnyRef) : Option[(Image, Boolean)] = None
}