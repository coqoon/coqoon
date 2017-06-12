/* ModelExplorer.scala
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.model._
import dk.itu.coqoon.core.model.CoqEnforcement.{Issue, Severity}
import dk.itu.coqoon.core.utilities.TryCast

import org.eclipse.ui.part.ViewPart
import org.eclipse.swt.widgets.Composite
import org.eclipse.jface.action.{
  Action, MenuManager, IMenuManager, IMenuListener}
import org.eclipse.jface.viewers._

private object ActionFactory {
  import org.eclipse.jface.action.Action
  def apply(label : String, action: => Unit) =
    new Action(label) {
      override def run = action
    }
}

class ModelExplorer extends ViewPart {
  private var tree : TreeViewer = null

  private val menuManager : MenuManager = new MenuManager
  menuManager.setRemoveAllWhenShown(true)
  menuManager.addMenuListener(new IMenuListener {
    private def _makePath(c : ICoqElement) : Seq[ICoqElement] =
      c.getParent match {
        case Some(parent) =>
          _makePath(parent) :+ c
        case None =>
          Seq()
      }
    private def makePath(c : ICoqElement) =
      new TreePath(_makePath(c).toArray)
    override def menuAboutToShow(manager : IMenuManager) =
      Option(tree).map(_.getSelection) match {
        case Some(s : IStructuredSelection) => s.getFirstElement match {
          case f : ICoqVernacFile =>
            menuManager.add(ActionFactory("Show corresponding object", {
              f.getObjectFile.foreach(o =>
                tree.setSelection(new TreeSelection(makePath(o))))
            }))
          case f : ICoqObjectFile =>
            menuManager.add(ActionFactory("Show corresponding source file", {
              f.getVernacFiles.headOption.foreach(s =>
                tree.setSelection(new TreeSelection(makePath(s))))
            }))
          case _ =>
        }
        case _ =>
      }
  })

  override def createPartControl(parent : Composite) = {
    tree = new TreeViewer(parent)
    tree.addOpenListener(new IOpenListener {
      import org.eclipse.ui.ide.IDE
      override def open(ev : OpenEvent) = ev.getSelection match {
        case s : IStructuredSelection => s.getFirstElement match {
          case f : ICoqVernacFile =>
            OpenDeclarationHandler.openEditorOn(f)
          case g : ICoqScriptGroup =>
            OpenDeclarationHandler.highlightElement(g.getDeterminingSentence)
          case s : ICoqScriptSentence =>
            OpenDeclarationHandler.highlightElement(s)
          case _ =>
        }
        case _ =>
      }
    })
    tree.getControl.setMenu(menuManager.createContextMenu(tree.getControl))
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

  import dk.itu.coqoon.ui.utilities.UIUtils.getWorkbench
  import org.eclipse.ui.ISharedImages
  override def getImage(a : Any) = a match {
    case p : ICoqPackageFragment =>
      Activator.getDefault.getImageRegistry.get(
          if (p.hasCoqFiles) {
            ManifestIdentifiers.Images.PACKAGE_FRAGMENT
          } else ManifestIdentifiers.Images.EMPTY_PACKAGE_FRAGMENT)
    case p : ICoqPackageFragmentRoot =>
      Activator.getDefault.getImageRegistry.get(
          ManifestIdentifiers.Images.PACKAGE_FRAGMENT_ROOT)
    case p : ICoqElement if p.getCorrespondingResource != None =>
      workbenchProvider.getImage(p.getCorrespondingResource.get)
    case (_ : Issue, s : Severity) =>
      s match {
        case Severity.Error =>
          getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJS_ERROR_TSK)
        case Severity.Information =>
          getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJS_INFO_TSK)
        case Severity.Warning =>
          getWorkbench.getSharedImages.getImage(
              ISharedImages.IMG_OBJS_WARN_TSK)
        case _ =>
          null
      }
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
    case g : ICoqScriptGroup =>
      g.getDeterminingSentence.getText.trim
    case s : ICoqScriptSentence =>
      s.getText.trim
    case (i : Issue, _ : Severity) =>
      i.message.trim
    case a =>
      Option(a).map(_.toString.trim).getOrElse("null")
  }
}

class ModelContentProvider
    extends ITreeContentProvider with CoqElementChangeListener {
  private var viewer : Option[StructuredViewer] = None
  private var input : Option[ICoqElement] = None

  override def dispose = input.foreach(_.getModel.removeListener(this))
  override def inputChanged(v : Viewer, o : Any, n : Any) = {
    viewer = TryCast[StructuredViewer](v)
    input.foreach(_.getModel.removeListener(this))
    input = TryCast[ICoqElement](n)
    input.foreach(_.getModel.addListener(this))
    viewer.foreach(_.refresh)
  }

  import dk.itu.coqoon.ui.utilities.UIUtils
  override def coqElementChanged(ev : CoqElementEvent) =
    UIUtils.asyncExec {
      viewer.foreach(v => {
        v.refresh(ev.element)
        ev.element.getParent.foreach(v.refresh)
      })
    }

  private def toCE(o : Any) = Option(o).flatMap(TryCast[ICoqElement])
  override def getChildren(el_ : Any) = toCE(el_) match {
    case Some(el : ICoqElement) =>
      (el match {
        case g : ICoqScriptGroup =>
          /* ICoqScriptGroups should always have at least a determining sentence,
           * so calling getChildren.tail should always be safe */
          g.getChildren.tail.toArray
        case p : IParent =>
          p.getChildren.toArray
        case _ =>
          Array.empty[Object]
      }) ++ el.getIssues.toSeq
    case _ =>
      Array.empty
  }
  override def getElements(o : Any) = getChildren(o)
  override def getParent(o : Any) = toCE(o).flatMap(_.getParent).orNull
  override def hasChildren(o : Any) = toCE(o) match {
    case Some(p : IParent) =>
      p.hasChildren || !p.getIssues.isEmpty
    case Some(el : ICoqElement) =>
      !el.getIssues.isEmpty
    case _ =>
      false
  }
}