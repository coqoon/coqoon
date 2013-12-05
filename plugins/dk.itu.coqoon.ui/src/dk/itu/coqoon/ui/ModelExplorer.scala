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
    tree.setLabelProvider(new LabelProvider {
      override def getText(a : Any) =
        Option(a).map(_.toString.trim).getOrElse("null")
    })
    tree.setContentProvider(new ModelContentProvider)
    tree.setInput(ICoqModel.getInstance)
  }

  override def setFocus() = Option(tree).foreach(_.getControl.setFocus)
}

class ModelContentProvider
    extends ITreeContentProvider with CoqElementChangeListener {
  private var viewer : Viewer = null
  private var input : ICoqModel = null

  override def dispose = Option(input).foreach(_.removeListener(this))
  override def inputChanged(v : Viewer, o : Any, n : Any) = {
    viewer = v
    Option(input).foreach(_.removeListener(this))
    n match {
      case m : ICoqModel =>
        input = m
        m.addListener(this)
      case _ =>
        input = null
    }
  }

  override def coqElementChanged(ev : CoqElementChangeEvent) =
    Option(viewer).foreach(_.refresh)

  private def toCE(o : Any) = Option(o).flatMap(TryCast[ICoqElement])
  override def getChildren(o : Any) = toCE(o).flatMap(
      TryCast[IParent]).toSeq.flatMap(_.getChildren).toArray
  override def getElements(o : Any) = getChildren(o)
  override def getParent(o : Any) = toCE(o).flatMap(_.getParent).orNull
  override def hasChildren(o : Any) = toCE(o).flatMap(
      TryCast[IParent]).map(_.hasChildren).getOrElse(false)
}