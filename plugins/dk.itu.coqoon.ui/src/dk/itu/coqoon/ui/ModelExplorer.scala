/* ModelExplorer.scala
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.part.ViewPart
import org.eclipse.swt.widgets.Composite
import org.eclipse.jface.viewers._

class ModelExplorer extends ViewPart {
  private var tree : TreeViewer = null

  override def createPartControl(parent : Composite) = {
    tree = new TreeViewer(parent)
    tree.addOpenListener(new IOpenListener {
      import dk.itu.coqoon.ui.utilities.UIUtils
      import org.eclipse.ui.ide.IDE
      override def open(ev : OpenEvent) = ev.getSelection match {
        case s : IStructuredSelection => s.getFirstElement match {
          case f : ICoqVernacFile =>
            OpenDeclarationHandler.openEditorOn(f)
          case s : ICoqScriptSentence =>
            OpenDeclarationHandler.highlightSentence(s)
          case _ =>
        }
        case _ =>
      }
    })
    tree.setLabelProvider(new ModelLabelProvider)
    tree.setContentProvider(new ModelContentProvider)
    tree.setInput(ICoqModel.getInstance)
  }

  override def setFocus() = Option(tree).foreach(_.getControl.setFocus)
}

class ModelLabelProvider extends LabelProvider {
  import org.eclipse.ui.model.WorkbenchLabelProvider
  private val workbenchProvider = new WorkbenchLabelProvider

  override def dispose = {
    super.dispose
    workbenchProvider.dispose
  }

  override def getImage(a : Any) = a match {
    case p : ICoqPackageFragment =>
      Activator.getDefault.getImageRegistry.get(
          ManifestIdentifiers.Images.PACKAGE_FRAGMENT)
    case p : ICoqPackageFragmentRoot =>
      Activator.getDefault.getImageRegistry.get(
          ManifestIdentifiers.Images.PACKAGE_FRAGMENT_ROOT)
    case p : ICoqElement if p.getCorrespondingResource != None =>
      workbenchProvider.getImage(p.getCorrespondingResource.get)
    case a => null
  }
  override def getText(a : Any) = a match {
    case p : ICoqPackageFragment =>
      val myPath = p.getCorrespondingResource.get.getFullPath
      val parentPath =
        p.getParent.flatMap(_.getCorrespondingResource).get.getFullPath
      val packageName =
        myPath.removeFirstSegments(parentPath.segmentCount).segments
      if (packageName.length == 0) {
        "(default package)"
      } else packageName.mkString(".")
    case p : ICoqElement if p.getCorrespondingResource != None =>
      p.getCorrespondingResource.get.getName
    case a =>
      Option(a).map(_.toString.trim).getOrElse("null")
  }
}

class ModelContentProvider
    extends ITreeContentProvider with CoqElementChangeListener {
  private var viewer : Option[Viewer] = None
  private var input : Option[ICoqModel] = None

  override def dispose = input.foreach(_.removeListener(this))
  override def inputChanged(v : Viewer, o : Any, n : Any) = {
    viewer = Option(v)
    input.foreach(_.removeListener(this))
    input = TryCast[ICoqModel](n)
    input.foreach(_.addListener(this))
    viewer.foreach(_.refresh)
  }

  override def coqElementChanged(ev : CoqElementChangeEvent) =
    viewer.foreach(_.refresh)

  private def toCE(o : Any) = Option(o).flatMap(TryCast[ICoqElement])
  override def getChildren(o : Any) = toCE(o).flatMap(
      TryCast[IParent]).toSeq.flatMap(_.getChildren).toArray
  override def getElements(o : Any) = getChildren(o)
  override def getParent(o : Any) = toCE(o).flatMap(_.getParent).orNull
  override def hasChildren(o : Any) = toCE(o).flatMap(
      TryCast[IParent]).map(_.hasChildren).getOrElse(false)
}