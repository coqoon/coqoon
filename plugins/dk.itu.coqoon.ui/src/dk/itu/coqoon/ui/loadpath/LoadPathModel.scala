/* LoadPathModel.scala
 * A simple JFace model for displaying Coqoon load path information
 * Copyright © 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.loadpath

import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.core.runtime.IPath
import org.eclipse.core.resources.IProject
import org.eclipse.jface.viewers.{Viewer, ViewerCell, StyledString,
  ITreeContentProvider, StyledCellLabelProvider}

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
    def getProvider() : LoadPathProvider
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
    override def getLoadPath = getProvider.getLoadPath
    override def getProvider = AbstractLoadPath(identifier)
  }

  import org.eclipse.core.resources.IFolder
  case class SourceLPE(parent : Option[LPProvider], folder : IFolder,
      output : Option[IFolder], index : Int)
      extends LPProvider(parent, index) {
    override def getChildren = Seq(OutputSLPE(Some(this), output))
    override def hasChildren = true
    override def getProvider = SourceLoadPath(folder, output)
  }

  case class DefaultOutputLPE(parent : Option[LPProvider],
      folder : IFolder, index : Int) extends LPProvider(parent, index) {
    override def getProvider = DefaultOutputLoadPath(folder)
  }

  case class ProjectLPE(parent : Option[LPProvider], project : IProject,
      index : Int) extends LPProvider(parent, index) {
    override def getChildren = Option(
        ICoqModel.toCoqProject(project)).toSeq.map(
            _.getLoadPathProviders).flatMap(translate(Some(this), _))
    override def hasChildren = getChildren.size > 0
    override def getProvider = ProjectLoadPath(project)
  }

  case class ExternalLPE(parent : Option[LPProvider], fsPath : IPath,
      dir : Seq[String], index : Int) extends LPProvider(parent, index) {
    override def getChildren =
      Seq(NamespaceSLPE(Some(this), LoadPathEntry(fsPath, dir)))
    override def hasChildren = true
    override def getProvider = ExternalLoadPath(fsPath, dir)
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
      case SourceLoadPath(a, b, _ /* FIXME */) =>
        SourceLPE(parent, a, b, index)
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
