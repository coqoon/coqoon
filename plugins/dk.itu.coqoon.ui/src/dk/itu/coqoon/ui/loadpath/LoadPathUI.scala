/* LoadPathUI.scala
 * User interfaces for viewing and manipulating Coq project load paths
 * Copyright © 2013, 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.loadpath

import org.eclipse.core.resources.{IFolder, IProject}
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
          <tree-viewer name="tv1">
            <grid-data h-grab="true" v-grab="true" />
          </tree-viewer>
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
            <button name="dfb" enabled="false">
              <grid-data h-span="2" />
              Remove
            </button>
            <button name="edb" enabled="false">
              <grid-data h-span="2" />
              Edit...
            </button>
            <button name="upb" enabled="false">
              <grid-data />
              Up
            </button>
            <button name="dob" enabled="false">
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
    val Seq(Some(afb), Some(dfb), Some(edb), Some(upb), Some(dob)) =
      names.getMany[Button]("afb", "dfb", "edb", "upb", "dob")

    val tv1 = names.get[TreeViewer]("tv1").get
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider)
    tv1.setInput(loadPath.get)

    Listener.Selection(afb, Listener {
      case Event.Selection(_) =>
        val wiz = new NewLoadPathWizard(getElement)
        if (new WizardDialog(c.getShell, wiz).open == Window.OK) {
          val lp = loadPath.get
          wiz.getResult.filter(r => !lp.contains(r)).foreach(lp.append(_))
          tv1.refresh()
        }
    })
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
    Listener.Selection(edb, Listener {
      case Event.Selection(_) =>
        val selection = Option(tv1.getSelection).flatMap(
            TryCast[TreeSelection]).map(_.getFirstElement)
        /* XXX: pass the current value to the page */
        val (page, index) =
          selection match {
            case Some(AbstractLPE(_, _, index)) =>
              (Some(new NLPAbstractEntryPage), index)
            case Some(SourceLPE(_, _, _, _, index)) =>
              (Some(new NLPSourceEntryPage), index)
            case Some(DefaultOutputLPE(_, _, index)) =>
              (Some(new NLPDefaultOutputEntryPage), index)
            case Some(ProjectLPE(_, _, index)) =>
              (Some(new NLPProjectEntryPage), index)
            case Some(ExternalLPE(_, _, _, index)) =>
              (Some(new NLPExternalEntryPage), index)
            case _ =>
              (None, -1)
          }
        page.foreach(page => {
          val entry =
            selection.flatMap(TryCast[LPProvider]).map(_.getProvider).get
          val wiz = new EditLoadPathWizard(getElement, page, entry)
          if (new WizardDialog(c.getShell, wiz).open == Window.OK) {
            val lp = loadPath.get
            wiz.getResult.filter(!lp.contains(_)).foreach(element => {
              lp.update(index, element)
              tv1.refresh()
              tv1.setSelection(new TreeSelection(new TreePath(Array(
                  loadPath.get, translate(None, element, index)))))
            })
          }
        })
    })
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
          case Some(e : DefaultOutputLPE) if e.getParent == None =>
            /* Deleting default output entries is really not a good idea */
            dfb.setEnabled(false)
            edb.setEnabled(true)
          case Some(e : LPProvider) if e.getParent == None =>
            /* Only allow top-level entries to be edited or deleted */
            dfb.setEnabled(true)
            edb.setEnabled(true)
          case _ =>
            dfb.setEnabled(false)
            edb.setEnabled(false)
        }

        /* Only allow top-level entries to be moved up and down...*/
        selection match {
          case Some(p : LPProvider) =>
            /* ... and don't allow the first element to be moved up or the last
             * to be moved down */
            upb.setEnabled(p.getIndex != 0);
            dob.setEnabled(p.getIndex != loadPath.get.length - 1)
          case _ =>
            upb.setEnabled(false)
            dob.setEnabled(false)
        }
      }
    })

    names.get[Composite]("root").get
  }
}

protected abstract class LoadPathWizard(
    val project : IProject) extends Wizard {
  private var result : Option[LoadPathProvider] = None
  protected def setResult(r : Option[LoadPathProvider]) = (result = r)
  def getResult() = result
}

class NewLoadPathWizard(
    override val project : IProject) extends LoadPathWizard(project) {
  private val selectionPage = new NLPSelectionPage
  addPage(selectionPage)
  setForcePreviousAndNextButtons(true)

  override def canFinish = selectionPage.isPageComplete &&
      Option(selectionPage.getNextPage).exists(_.isPageComplete)
  override def performFinish = {
    setResult(
      if (canFinish()) {
        selectionPage.getNextPage.createLoadPathEntry
      } else None)
    canFinish
  }
}

class EditLoadPathWizard(override val project : IProject, page : LPWizardPage,
    input : LoadPathProvider) extends LoadPathWizard(project) {
  addPage(page)

  override def canFinish = page.isPageComplete
  override def performFinish = {
    setResult(
      if (canFinish()) {
        page.createLoadPathEntry
      } else None)
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

  private var nextPage : Option[LPWizardPage] = None
  override def getNextPage : LPWizardPage = nextPage.orNull

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
      override def getText(element : Any) = TryCast[LPWizardPage](
          element).map(_.getTitle).getOrElse(super.getText(element))
    })
    lv.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) = {
        nextPage = TryCast[IStructuredSelection](ev.getSelection).map(
            _.getFirstElement).flatMap(TryCast[LPWizardPage])
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

abstract class LPWizardPage(
    name : String, title : String, descriptor : ImageDescriptor = null)
        extends WizardPage(name, title, descriptor) {
  override def getWizard() : LoadPathWizard =
    super.getWizard.asInstanceOf[LoadPathWizard]

  /* Calling this method should be basically free, so it also serves as the
   * implementation of isPageComplete */
  def createLoadPathEntry() : Option[LoadPathProvider]
  override def isPageComplete() = (createLoadPathEntry != None)
}

class NLPAbstractEntryPage extends LPWizardPage(
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
          <tree-viewer name="lv">
            <grid-data grab="true" />
          </tree-viewer>
          <button name="b2" style="radio">
            <grid-data h-grab="true" />
            Manually specify the identifier of an abstract dependency
          </button>
          <text name="at" border="true">
            <grid-data h-grab="true" />
          </text>
        </composite>, parent)

    val lv = names.get[TreeViewer]("lv").get
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

class NLPSourceEntryPage extends LPWizardPage(
    "nlpSEP", "Another source folder in this project") {
  setDescription(
      "Add an additional source folder to this project.")

  private var folder : Option[IFolder] = None
  private var coqdir : Option[Seq[String]] = Some(Seq())
  private var output : Option[IFolder] = None
  override def createLoadPathEntry =
    (folder, coqdir, output) match {
      case (Some(f), Some(Seq()), Some(o)) if o.exists() =>
        Some(SourceLoadPath(f, Some(o)))
      case (Some(f), Some(Seq()), None) =>
        Some(SourceLoadPath(f, None))

      case (Some(f), Some(cd), Some(o)) if o.exists() =>
        Some(SourceLoadPath(f, Some(o), cd))
      case (Some(f), Some(cd), None) =>
        Some(SourceLoadPath(f, None, cd))
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
            Namespace prefix:
          </label>
          <text name="nt" border="true">
            <grid-data h-grab="true" h-span="2" />
            <tool-tip>
The part of the coqdir namespace that this source folder corresponds to
(optional; defaults to the root).
            </tool-tip>
          </text>
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

    val nt = names.get[Text]("nt").get
    Listener.Modify(nt, Listener {
      case Event.Modify(_) =>
        val ct = nt.getText.trim
        val cd =
          if (!ct.isEmpty) {
            ct.split("\\.")
          } else Array()
        /* XXX: Coq library name validation should be done properly
         * somewhere! */
        if (!cd.forall(
            part => (!part.isEmpty) && part.forall(_.isLetterOrDigit))) {
          coqdir = None
          setErrorMessage(s""""$ct" is not a valid Coq namespace prefix""")
        } else {
          coqdir = Some(cd)
          setErrorMessage(null)
        }
        getContainer.updateButtons
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

class NLPDefaultOutputEntryPage extends LPWizardPage(
    "nlpDOEP", "Default output directory") {
  setDescription(
      "Change the default output directory for source folders.")
  private var output : Option[IFolder] = None
  override def createLoadPathEntry = output.map(DefaultOutputLoadPath(_))

  override def createControl(parent : Composite) = {
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="3" />
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
          output = None
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

class NLPProjectEntryPage extends LPWizardPage(
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

class NLPExternalEntryPage extends LPWizardPage(
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
          <button style="push">
            <listener kind="select-directory" target="ft" />
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
