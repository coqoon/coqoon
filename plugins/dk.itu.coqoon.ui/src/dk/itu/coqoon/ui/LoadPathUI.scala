/* LoadPathUI.scala
 * User interfaces for viewing and manipulating Coq project load paths
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.core.resources.IProject
import dk.itu.coqoon.ui.utilities.UIUtils
import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.{TryCast, CacheSlot}

import org.eclipse.jface.wizard._
import org.eclipse.jface.viewers._

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{
  Text, Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.runtime.IAdaptable
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

  /* Load path model entries backed directly by an ICoqLoadPathProvider whose
   * children should represent namespaces and locations */
  abstract class LPNSChild(parent : Option[LPProvider],
      cl : ICoqLoadPathProvider, index : Int)
          extends LPProvider(parent, index) {
    override def getChildren = {
      var result = Seq[LPBase]()
      var children = cl.getLoadPath
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
    override def hasChildren = cl.getLoadPath.size > 0
  }

  case class AbstractLPE(parent : Option[LPProvider], cl : AbstractLoadPath,
      index : Int) extends LPNSChild(parent, cl, index)

  case class SourceLPE(parent : Option[LPProvider], cl : SourceLoadPath,
      index : Int) extends LPProvider(parent, index) {
    override def getChildren = Seq(OutputSLPE(Some(this), cl))
    override def hasChildren = true
  }

  case class DefaultOutputLPE(parent : Option[LPProvider],
      cl : DefaultOutputLoadPath, index : Int)
          extends LPProvider(parent, index)

  case class ProjectLPE(parent : Option[LPProvider], cl : ProjectLoadPath,
      index : Int) extends LPProvider(parent, index) {
    override def getChildren = Option(
        ICoqModel.toCoqProject(cl.project)).toSeq.map(
            _.getLoadPathProviders).flatMap(translate(Some(this), _))
    override def hasChildren = getChildren.size > 0
  }

  case class ExternalLPE(parent : Option[LPProvider], cl : ExternalLoadPath,
      index : Int) extends LPProvider(parent, index)

  case class OutputSLPE(
      parent : Option[LPProvider], cl : SourceLoadPath) extends LPBase(parent)
  case class NamespaceSLPE(
      parent : Option[LPProvider], cl : CoqLoadPath) extends LPBase(parent)
  case class LocationSLPE(
      parent : Option[LPProvider], cl : CoqLoadPath) extends LPBase(parent)
  case class SeparatorSLPE(
      parent : Option[LPProvider]) extends LPBase(parent)

  def translate(parent : Option[LPProvider],
      providers : Seq[ICoqLoadPathProvider]) : Seq[LPProvider] =
    for ((provider, index) <- providers.zipWithIndex)
      yield translate(parent, provider, index)

  def translate(parent : Option[LPProvider],
      provider : ICoqLoadPathProvider, index : Int) : LPProvider =
    provider match {
      case p @ AbstractLoadPath(_) => AbstractLPE(parent, p, index)
      case p @ SourceLoadPath(_, _) => SourceLPE(parent, p, index)
      case p @ DefaultOutputLoadPath(_) => DefaultOutputLPE(parent, p, index)
      case p @ ProjectLoadPath(_) => ProjectLPE(parent, p, index)
      case p @ ExternalLoadPath(_, _) => ExternalLPE(parent, p, index)
    }
}

private class LoadPathLabelProvider extends StyledCellLabelProvider {
  import LoadPathModel._
  import LoadPathLabelProvider._

  override def update(cell : ViewerCell) = cell.getElement match {
    case l : LPBase =>
      val s = new StyledString
      l match {
        case AbstractLPE(_, lpe, i) =>
          s.append(s"${i}. Library: ")
          lpe.getProvider match {
            case Some(provider) =>
              s.append(provider.getName, ColourStyler(VALID))
            case None =>
              s.append(lpe.identifier, ColourStyler(ERROR))
          }
        case SourceLPE(_, lpe, i) =>
          s.append(s"${i}. Source folder: ")
          s.append(
              lpe.folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case DefaultOutputLPE(_, lpe, i) =>
          s.append(s"${i}. Default output folder: ")
          s.append(
              lpe.folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case ProjectLPE(_, lpe, i) =>
          import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}
          s.append(s"${i}. Project: ")
          val styler = ColourStyler(
            if (lpe.project.exists() && lpe.project.hasNature(CMI.NATURE_COQ))
              VALID else ERROR)
          s.append(lpe.project.getName.toString, styler)
        case ExternalLPE(_, lpe, i) =>
          import dk.itu.coqoon.core.project.CoqNature
          s.append(s"${i}. External development: ")
          val styler =
            ColourStyler(if (lpe.fsPath.toFile.exists) VALID else ERROR)
          s.append(lpe.fsPath.addTrailingSeparator.toString, styler)

        case NamespaceSLPE(_, lpe) =>
          s.append("Namespace: ")
          s.append(lpe.coqdir.getOrElse("(root)"), ColourStyler(NSLOC))
        case LocationSLPE(_, lpe) =>
          s.append("Location: ")
          s.append(lpe.path.toString, ColourStyler(NSLOC))
        case OutputSLPE(_, lpe) =>
          s.append("Output folder: ")
          s.append(lpe.output.map(
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
    case s : Seq[ICoqLoadPathProvider] =>
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
    import org.eclipse.swt.events._, org.eclipse.swt.layout._
    import org.eclipse.ui.dialogs.{
      ElementTreeSelectionDialog, ISelectionStatusValidator}
    import org.eclipse.core.runtime.IStatus
    import org.eclipse.core.resources.IFolder
    import org.eclipse.jface.layout._
    import org.eclipse.jface.window.Window

    val c1 = new Composite(c, SWT.NONE)
    c1.setLayout(GridLayoutFactory.fillDefaults.numColumns(2).create)

    val tv1 = new TreeViewer(c1, SWT.SINGLE | SWT.BORDER)
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider)
    tv1.setInput(loadPath.get)
    tv1.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).grab(true, true).create)

    val c1r = new Composite(c1, SWT.NONE)
    c1r.setLayout(GridLayoutFactory.swtDefaults().
        numColumns(2).equalWidth(true).create)
    c1r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).create)

    val afb = new Button(c1r, SWT.NONE)
    afb.setText("Add...")
    afb.setLayoutData(GridDataFactory.swtDefaults.span(2, 1).
        align(SWT.FILL, SWT.FILL).create)
    afb.addSelectionListener(new SelectionAdapter {
      override def widgetSelected(ev : SelectionEvent) = {
        val wiz = new NewLoadPathWizard
        if (new WizardDialog(c.getShell, wiz).open == Window.OK) {
          wiz.getResult.foreach(loadPath.get.append(_))
          tv1.refresh()
        }
      }
    })

    new Label(c1r, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(
        GridDataFactory.swtDefaults.
            align(SWT.FILL, SWT.FILL).span(2, 1).create)

    val dfb = new Button(c1r, SWT.NONE)
    dfb.setText("Remove")
    dfb.setLayoutData(GridDataFactory.swtDefaults.span(2, 1).
        align(SWT.FILL, SWT.FILL).create)
    dfb.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
        Option(tv1.getSelection.
            asInstanceOf[TreeSelection].getFirstElement) match {
          case Some(p : LPProvider) =>
            loadPath.get.remove(p.getIndex)
            tv1.refresh()
          case _ =>
        }
      }
    })

    val edb = new Button(c1r, SWT.NONE)
    edb.setText("Edit...")
    edb.setLayoutData(GridDataFactory.swtDefaults.span(2, 1).
        align(SWT.FILL, SWT.FILL).create)

    val upb = new Button(c1r, SWT.NONE)
    upb.setText("Up")
    upb.setLayoutData(GridDataFactory.swtDefaults.
        align(SWT.FILL, SWT.FILL).create)
    upb.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
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
      }
    })

    val dob = new Button(c1r, SWT.NONE)
    dob.setText("Down")
    dob.setLayoutData(GridDataFactory.swtDefaults.
        align(SWT.FILL, SWT.FILL).create)
    dob.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
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
      }
    })

    new Label(c1r, SWT.SEPARATOR | SWT.HORIZONTAL).setLayoutData(
        GridDataFactory.swtDefaults.align(
            SWT.FILL, SWT.FILL).span(2, 1).create)

    val inl = new Label(c1r, SWT.CENTER | SWT.READ_ONLY | SWT.WRAP)
    inl.setLayoutData(GridDataFactory.swtDefaults.hint(100, SWT.DEFAULT).align(
        SWT.FILL, SWT.BEGINNING).grab(false, true).span(2, 1).create)
    inl.setText("Higher entries take priority over lower ones. " +
        "Use the Up and Down buttons to reorder entries.")

    c1
  }
}

class NewLoadPathWizard extends Wizard {
  private val selectionPage = new NLPSelectionPage
  addPage(selectionPage)
  setForcePreviousAndNextButtons(true)

  private var result : Option[ICoqLoadPathProvider] = None
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
  /* Calling this method should be basically free, so it also serves as the
   * implementation of isPageComplete */
  def createLoadPathEntry() : Option[ICoqLoadPathProvider]
  override def isPageComplete() = (createLoadPathEntry != None)
}

class NLPAbstractEntryPage extends NLPWizardPage(
    "nlpAEP", "Abstract dependency") {
  setDescription(
      "Add an identifier for a dependency which the environment must provide.")

  private var identifier : Option[String] = None
  override def createLoadPathEntry = identifier.map(AbstractLoadPath)

  override def createControl(parent : Composite) = {
    import org.eclipse.jface.layout.{GridDataFactory => GDF, GridLayoutFactory}
    val c = new Composite(parent, SWT.NONE)
    c.setLayout(GridLayoutFactory.fillDefaults.create)

    val b1 = new Button(c, SWT.RADIO)
    b1.setText("Select from the list of available abstract dependencies")
    b1.setLayoutData(GDF.fillDefaults.grab(true, false).
        align(SWT.FILL, SWT.FILL).create)
    val lv = new TreeViewer(c, SWT.SINGLE | SWT.BORDER)
    lv.getControl.setLayoutData(
        GDF.fillDefaults.grab(true, true).align(SWT.FILL, SWT.FILL).create)
    lv.setContentProvider(new FuturisticContentProvider[TreeViewer] {
      override def actuallyGetChildren(element : AnyRef) =
        element match {
          case a : AbstractLoadPathManager =>
            for (i <- a.getProviders;
                 f <- Some(i.getImplementations))
              yield (if (f.size == 1) f.head else i)
          case p : AbstractLoadPathProvider => p.getImplementations
          case _ => Seq()
        }
      override def getParent(element : AnyRef) =
        TryCast[AbstractLoadPathImplementation](
            element).map(_.getProvider).orNull
    })
    lv.setLabelProvider(new FuturisticLabelProvider {
      override def actuallyGetText(element : AnyRef) = element match {
        case p : AbstractLoadPathProvider => Some(p.getName)
        case i : AbstractLoadPathImplementation => Some(i.getName)
        case _ => None
      }
      import org.eclipse.ui.ISharedImages
      override def actuallyGetImage(element : AnyRef) = element match {
        case p : AbstractLoadPathProvider =>
          Some((UIUtils.getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJ_FOLDER), false))
        case i : AbstractLoadPathImplementation =>
          Some((UIUtils.getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJ_ELEMENT), false))
        case _ => None
      }
    })
    lv.setInput(AbstractLoadPathManager.getInstance)

    val b2 = new Button(c, SWT.RADIO)
    b2.setText("Manually specify the identifier of an abstract dependency")
    b2.setLayoutData(
        GDF.fillDefaults.grab(true, false).align(SWT.FILL, SWT.FILL).create)

    import org.eclipse.swt.events.{ModifyEvent, ModifyListener}
    val at = new Text(c, SWT.BORDER)
    at.setLayoutData(
        GDF.fillDefaults.grab(true, false).align(SWT.FILL, SWT.FILL).create)
    at.addModifyListener(new ModifyListener {
      override def modifyText(ev : ModifyEvent) = {
        identifier = Option(at.getText).map(_.trim).filter(_.length > 0)
        getContainer.updateButtons
      }
    })

    lv.addSelectionChangedListener(new ISelectionChangedListener {
      override def selectionChanged(ev : SelectionChangedEvent) =
        TryCast[IStructuredSelection](ev.getSelection).map(
            _.getFirstElement) match {
          case Some(i : AbstractLoadPathImplementation) =>
            import AbstractLoadPathImplementation._
            i.getLoadPath match {
              case Right(_) =>
                setErrorMessage(null)
                at.setText(i.getIdentifier)
              case Left(VersionMismatch) =>
                setErrorMessage(i.getName + " is installed, " +
                    "but is not compatible with the version you requested")
              case Left(Broken) =>
                setErrorMessage(i.getName + " is installed, but doesn't work")
              case Left(_ : NotAvailable) =>
                setErrorMessage(i.getName + " is not installed")
            }
          case _ =>
        }
    })

    setControl(c)
  }
}

class NLPSourceEntryPage extends NLPWizardPage(
    "nlpSEP", "Another source folder in this project") {
  setDescription(
      "Add an additional source folder to this project.")

  import org.eclipse.core.resources.IFolder
  private var folder : Option[IFolder] = None
  private var output : Option[IFolder] = None
  override def createLoadPathEntry = folder.map(SourceLoadPath(_, output))

  override def createControl(parent : Composite) = {
    import org.eclipse.swt.layout.{GridData, GridLayout}
    val c = new Composite(parent, SWT.NONE)
    c.setLayout(new GridLayout(3, false))

    new Label(c, SWT.NONE).setText("Source folder: ")
    new Text(c, SWT.BORDER).setLayoutData(new GridData(
        SWT.FILL, SWT.FILL, true, false))
    new Button(c, SWT.PUSH).setText("Browse...")

    new Label(c, SWT.NONE).setText("Output folder:")
    new Text(c, SWT.BORDER).setLayoutData(new GridData(
        SWT.FILL, SWT.FILL, true, false))
    new Button(c, SWT.PUSH).setText("Browse...")

    setControl(c)
  }
}

class NLPProjectEntryPage extends NLPWizardPage(
    "nlpPEP", "Another Coqoon project") {
  setDescription(
      "Add a dependency on another Coqoon project in the workspace.")
  private var project : Option[IProject] = None
  override def createLoadPathEntry = project.map(ProjectLoadPath)

  override def createControl(parent : Composite) = {
    import org.eclipse.swt.layout.{GridData, GridLayout}
    val c = new Composite(parent, SWT.NONE)
    c.setLayout(new FillLayout)

    import org.eclipse.ui.model.{
      WorkbenchLabelProvider, WorkbenchContentProvider}
    import org.eclipse.core.resources.ResourcesPlugin
    val tv = new TreeViewer(c, SWT.BORDER | SWT.SINGLE)
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

    setControl(c)
  }
}

class NLPExternalEntryPage extends NLPWizardPage(
    "nlpEEP", "External development") {
  setDescription(
      "Add a dependency on a Coq development built outside of Coqoon.")

  import org.eclipse.core.runtime.IPath
  private var fspath : Option[IPath] = None
  private var dir : Option[String] = None
  override def createLoadPathEntry = fspath.map(ExternalLoadPath(_, dir))

  override def createControl(parent : Composite) = {
    import org.eclipse.swt.layout.{GridData, GridLayout}
    val c = new Composite(parent, SWT.NONE)
    c.setLayout(new GridLayout(3, false))

    new Label(c, SWT.NONE).setText("Folder: ")
    new Text(c, SWT.BORDER).setLayoutData(new GridData(
        SWT.FILL, SWT.FILL, true, false))
    new Button(c, SWT.PUSH).setText("Browse...")

    new Label(c, SWT.NONE).setText("Coq namespace:")
    new Text(c, SWT.BORDER).setLayoutData(new GridData(
        SWT.FILL, SWT.FILL, true, false, 2, 1))

    setControl(c)
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
