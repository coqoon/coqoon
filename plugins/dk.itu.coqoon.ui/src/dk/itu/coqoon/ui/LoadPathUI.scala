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

protected object LoadPathModel {
  abstract class LPBase(private val parent : Option[LPBase]) {
    final def getParent() : Option[LPBase] = parent
    def getChildren() : Seq[LPBase]
    def hasChildren() : Boolean
    def hasAncestor[A]()(implicit a0 : Manifest[A]) : Boolean =
      getParent match {
        case Some(f : A) => true
        case Some(f) => f.hasAncestor[A]
        case None => false
      }
  }

  abstract class LPNSChild(parent : Option[LPBase],
      cl : ICoqLoadPathProvider) extends LPBase(parent) {
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

  abstract class LPNSNoChild(
      parent : Option[LPBase]) extends LPBase(parent) {
    override def getChildren = Seq()
    override def hasChildren = false
  }

  case class AbstractLPE(parent : Option[LPBase],
      cl : AbstractLoadPath) extends LPNSChild(parent, cl)

  case class SourceLPE(
      parent : Option[LPBase], cl : SourceLoadPath) extends LPBase(parent) {
    override def getChildren = Seq(OutputSLPE(Some(this), cl))
    override def hasChildren = true
  }

  case class DefaultOutputLPE(parent : Option[LPBase],
      cl : DefaultOutputLoadPath) extends LPBase(parent) {
    override def getChildren = Seq()
    override def hasChildren = false
  }

  case class ProjectLPE(parent : Option[LPBase],
      cl : ProjectLoadPath) extends LPBase(parent) {
    override def getChildren = Option(
        ICoqModel.toCoqProject(cl.project)).toSeq.map(
            _.getLoadPathProviders).flatMap(translate(Some(this), _))
    override def hasChildren = getChildren.size > 0
  }

  case class ExternalLPE(parent : Option[LPBase],
      cl : ExternalLoadPath) extends LPNSChild(parent, cl)

  case class OutputSLPE(
      parent : Option[LPBase], cl : SourceLoadPath) extends LPNSNoChild(parent)
  case class NamespaceSLPE(
      parent : Option[LPBase], cl : CoqLoadPath) extends LPNSNoChild(parent)
  case class LocationSLPE(
      parent : Option[LPBase], cl : CoqLoadPath) extends LPNSNoChild(parent)
  case class SeparatorSLPE(
      parent : Option[LPBase]) extends LPNSNoChild(parent)

  def translate(
      parent : Option[LPBase], providers : Seq[ICoqLoadPathProvider]) =
    providers.map(_ match {
      case p : AbstractLoadPath => AbstractLPE(parent, p)
      case p : SourceLoadPath => SourceLPE(parent, p)
      case p : DefaultOutputLoadPath => DefaultOutputLPE(parent, p)
      case p : ProjectLoadPath => ProjectLPE(parent, p)
      case p : ExternalLoadPath => ExternalLPE(parent, p)
    })
}

import org.eclipse.jface.viewers.{StyledCellLabelProvider, StyledString,
  ILabelProviderListener, ITreeContentProvider, Viewer, ViewerCell}

private class LoadPathLabelProvider extends StyledCellLabelProvider {
  import LoadPathModel._
  import LoadPathLabelProvider._

  override def update(cell : ViewerCell) = cell.getElement match {
    case l : LPBase =>
      val s = new StyledString
      l match {
        case AbstractLPE(_, lpe) =>
          s.append("Library: ")
          lpe.getProvider match {
            case Some(provider) =>
              s.append(provider.getName, ColourStyler(VALID))
            case None =>
              s.append(lpe.identifier, ColourStyler(ERROR))
          }
        case SourceLPE(_, lpe) =>
          s.append("Source folder: ")
          s.append(
              lpe.folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case DefaultOutputLPE(_, lpe) =>
          s.append("Default output folder: ")
          s.append(
              lpe.folder.getProjectRelativePath.addTrailingSeparator.toString,
              ColourStyler(VALID))
        case ProjectLPE(_, lpe) =>
          import dk.itu.coqoon.core.{ManifestIdentifiers => CMI}
          s.append("Project: ")
          val styler = ColourStyler(
            if (lpe.project.exists() && lpe.project.hasNature(CMI.NATURE_COQ))
              VALID else ERROR)
          s.append(lpe.project.getName.toString, styler)
        case ExternalLPE(_, lpe) =>
          import dk.itu.coqoon.core.project.CoqNature
          s.append("External development: ")
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

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{
  Text, Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.jface.preference.PreferencePage
import org.eclipse.jface.viewers.TreeViewer

class LoadPathConfigurationPage
    extends PreferencePage with IWorkbenchPropertyPage {
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
    import org.eclipse.jface.viewers._

    val c1 = new Composite(c, SWT.NONE)
    c1.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv1 = new TreeViewer(c1)
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider)
    tv1.setInput(loadPath.get)
    tv1.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c1r = new Composite(c1, SWT.NONE)
    c1r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c1r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    val afb = new Button(c1r, SWT.NONE)
    afb.setText("Add Folder...")
    afb.addSelectionListener(new SelectionAdapter {
      override def widgetSelected(ev : SelectionEvent) = {
        val dialog = UIUtils.createWorkspaceElementDialog(getShell)
        dialog.setInput(getElement)
        dialog.addFilter(MultiFilter(new OnlyFoldersFilter,
            new NoOutputFoldersFilter, new NoHiddenResourcesFilter))
        dialog.setValidator(new SelectionValidator {
          override def check(selection : Object) : Option[String] = {
            for (i <- loadPath.get;
                 j <- TryCast[SourceLoadPath](i)
                     if j.folder == selection)
              return Some(j.folder.getName + " is already in the load path")
            None
          }
        })
        dialog.setAllowMultiple(false)
        if (dialog.open == Window.OK) {
          Option(dialog.getFirstResult) match {
            case Some(f : IFolder) =>
              loadPath.get += new SourceLoadPath(f, None)
              tv1.refresh()
            case _ =>
          }
        }
      }
    })
    new Label(c1r, SWT.SEPARATOR | SWT.HORIZONTAL)
    val dfb = new Button(c1r, SWT.NONE)
    dfb.setText("Delete")
    dfb.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
        for (i <- tv1.getSelection.asInstanceOf[TreeSelection].iterator)
          Option(i) match {
            case Some(slp : SourceLoadPath) =>
              loadPath.get -= slp
            case _ =>
          }
        tv1.refresh()
      }
    })
    c1
  }
}