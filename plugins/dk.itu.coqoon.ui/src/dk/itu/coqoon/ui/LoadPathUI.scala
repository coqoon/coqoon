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

import org.eclipse.jface.viewers.{
  LabelProvider, ILabelProviderListener, ITreeContentProvider, Viewer}

private class LoadPathLabelProvider extends LabelProvider {
  override def getText(input : Any) = input match {
    case q @ AbstractLoadPath(identifier) => q.getProvider.map(
        a => a.getName).getOrElse("(missing library " + identifier + ")")
    case _ => super.getText(input)
  }
}

private class LoadPathContentProvider(
    filter : ICoqLoadPathProvider => Boolean) extends ITreeContentProvider {
  override def dispose = ()

  override def inputChanged(viewer : Viewer, oldInput : Any, newInput : Any) =
    viewer.refresh

  override def getElements(input : Any) = input match {
    case c : CacheSlot[_] => getElements(c.get)
    case a : Seq[_] => a.flatMap(_ match {
      case q : ICoqLoadPathProvider if filter(q) => Some(q)
      case _ => None
    }).toArray
    case _ => Array()
  }

  override def getChildren(parent : Any) = parent match {
    case a : ICoqLoadPathProvider => a.getLoadPath.toArray
    case _ => Array()
  }

  override def getParent(child : Any) = null
  override def hasChildren(parent : Any) =
    (TryCast[ICoqLoadPathProvider](parent) != None)
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

    val folder = new TabFolder(c, SWT.NONE)

    val t1 = new TabItem(folder, SWT.NONE)
    t1.setText("Source")

    val c1 = new Composite(folder, SWT.NONE)
    t1.setControl(c1)
    c1.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv1 = new TreeViewer(c1)
    tv1.setLabelProvider(new LoadPathLabelProvider)
    tv1.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[SourceLoadPath]))
    tv1.setInput(loadPath)
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

    val oll = new Label(c1, SWT.NONE)
    oll.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).span(2, 1).create)
    oll.setText("Default output location:")

    val olt = new Text(c1, SWT.BORDER)
    olt.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    val olb = new Button(c1, SWT.NONE)
    olb.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)
    olb.setText("Browse...")

    val t2 = new TabItem(folder, SWT.NONE)
    t2.setText("Projects")

    val c2 = new Composite(folder, SWT.NONE)
    t2.setControl(c2)
    c2.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv2 = new TreeViewer(c2)
    tv2.setLabelProvider(new LoadPathLabelProvider)
    tv2.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[ProjectLoadPath]))
    tv2.setInput(loadPath)
    tv2.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c2r = new Composite(c2, SWT.NONE)
    c2r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c2r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    val apb = new Button(c2r, SWT.NONE)
    apb.setText("Add Project...")
    apb.addSelectionListener(new SelectionAdapter {
      override def widgetSelected(ev : SelectionEvent) = {
        val dialog = UIUtils.createWorkspaceElementDialog(getShell)
        dialog.setInput(getElement.getWorkspace.getRoot)
        dialog.addFilter(MultiFilter(
            new OnlyProjectsFilter, new OmitResourcesFilter(getElement)))
        dialog.setValidator(new SelectionValidator {
          override def check(selection : Object) : Option[String] = {
            for (i <- loadPath.get;
                 j <- TryCast[ProjectLoadPath](i)
                     if j.project == selection)
              return Some(j.project.getName + " is already in the load path")
            None
          }
        })
        dialog.setAllowMultiple(false)
        if (dialog.open == Window.OK) {
          Option(dialog.getFirstResult) match {
            case Some(p : IProject) =>
              loadPath.get += new ProjectLoadPath(p)
              tv2.refresh()
            case _ =>
          }
        }
      }
    })
    new Label(c2r, SWT.SEPARATOR | SWT.HORIZONTAL)
    val dpb = new Button(c2r, SWT.NONE)
    dpb.setText("Delete")
    dpb.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
        for (i <- tv2.getSelection.asInstanceOf[TreeSelection].iterator)
          Option(i) match {
            case Some(plp : ProjectLoadPath) =>
              loadPath.get -= plp
            case _ =>
          }
        tv2.refresh()
      }
    })

    val t3 = new TabItem(folder, SWT.NONE)
    t3.setText("Libraries")

    val c3 = new Composite(folder, SWT.NONE)
    t3.setControl(c3)
    c3.setLayout(GridLayoutFactory.swtDefaults().numColumns(2).create)

    val tv3 = new TreeViewer(c3)
    tv3.setLabelProvider(new LoadPathLabelProvider)
    tv3.setContentProvider(new LoadPathContentProvider(
        a => a.isInstanceOf[ExternalLoadPath] ||
            a.isInstanceOf[AbstractLoadPath]))
    tv3.setInput(loadPath)
    tv3.getControl.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.FILL).hint(0, 0).grab(true, true).create)

    val c3r = new Composite(c3, SWT.NONE)
    c3r.setLayout(RowLayoutFactory.swtDefaults().
        `type`(SWT.VERTICAL).fill(true).create())
    c3r.setLayoutData(GridDataFactory.swtDefaults().
        align(SWT.FILL, SWT.TOP).create)

    val alb = new Button(c3r, SWT.NONE)
    alb.setText("Add Library...")
    alb.addSelectionListener(new SelectionAdapter {
      import org.eclipse.swt.widgets.DirectoryDialog
      import org.eclipse.core.runtime.Path
      override def widgetSelected(ev : SelectionEvent) : Unit = {
        val dialog = new DirectoryDialog(getShell)
        Option(dialog.open).map(f => new Path(f)) match {
          case Some(p) =>
            for (i <- loadPath.get;
                 j <- TryCast[ExternalLoadPath](i)
                     if j.fsPath == p)
              return
            loadPath.get += new ExternalLoadPath(p, None)
            tv3.refresh()
          case _ =>
        }
      }
    })

    val aslb = new Button(c3r, SWT.NONE)
    aslb.setText("Add System Library...")
    aslb.setEnabled(false)
    new Label(c3r, SWT.SEPARATOR | SWT.HORIZONTAL)
    val dlb = new Button(c3r, SWT.NONE)
    dlb.setText("Delete")
    dlb.addSelectionListener(new SelectionAdapter {
      import scala.collection.JavaConversions._
      override def widgetSelected(ev : SelectionEvent) = {
        for (i <- tv3.getSelection.asInstanceOf[TreeSelection].iterator)
          Option(i) match {
            case Some(elp : ExternalLoadPath) =>
              loadPath.get -= elp
            /* case Some(alp : AbstractLoadPath) =>
              loadPath.get -= alp */
            case _ =>
          }
        tv3.refresh()
      }
    })

    folder.pack
    folder
  }
}