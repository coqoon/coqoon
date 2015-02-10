/* BuildScriptUI.scala
 * A preference page for checking and updating project build scripts
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{
  Text, Composite, Button, Label, TabFolder, TabItem}
import org.eclipse.core.resources.IProject
import org.eclipse.jface.preference.PreferencePage

class BuildScriptManagementPage
    extends PreferencePage with IWorkbenchPropertyPage {
  import org.eclipse.core.runtime.IAdaptable

  import dk.itu.coqoon.core.project.CoqBuildScript

  override def performOk() = {
    automaticCheckbox.foreach(autob => {
      val currentValue = CoqBuildScript.WriteBuildScript.get(getElement)
      val newValue = Some(autob.getSelection)
      if (currentValue != newValue) {
        CoqBuildScript.WriteBuildScript.set(getElement, newValue)
        if (newValue == Some(true))
          (/* XXX: trigger a build operation immediately */)
      }
    })
    true
  }

  private var projectVersionLabel : Option[Label] = None
  private var updateButton : Option[Button] = None
  private var automaticCheckbox : Option[Button] = None

  private var element : IAdaptable = null
  override def getElement : IProject = TryCast[IProject](element).get
  override def setElement(element : IAdaptable) = (this.element = element)
  override def createContents(c : Composite) = {
    import org.eclipse.swt.events._
    import org.eclipse.swt.layout._
    import org.eclipse.core.runtime.IStatus
    import org.eclipse.jface.layout._
    import org.eclipse.jface.window.Window

    val c1 = new Composite(c, SWT.NONE)
    c1.setLayout(GridLayoutFactory.fillDefaults.numColumns(2).create)

    val pvl = new Label(c1, SWT.NONE)
    pvl.setLayoutData(GridDataFactory.fillDefaults.create)
    pvl.setText("Project version:")
    
    val pv = new Label(c1, SWT.NONE)
    pv.setLayoutData(GridDataFactory.fillDefaults.grab(true, false).create)
    projectVersionLabel = Some(pv)
    pv.setToolTipText("The version of the build script currently included " +
                      "in this project.")

    val mrl = new Label(c1, SWT.NONE)
    mrl.setLayoutData(GridDataFactory.fillDefaults.create)
    mrl.setText("Most recent version:")

    val mr = new Label(c1, SWT.NONE)
    mr.setLayoutData(GridDataFactory.fillDefaults.grab(true, false).create)
    mr.setText(s"${CoqBuildScript.currentVersion}")
    mr.setToolTipText("The most recent version of the build script known to " +
                      "this version of Coqoon.")

    val upd = new Button(c1, SWT.NONE)
    upd.setLayoutData(GridDataFactory.fillDefaults.align(SWT.RIGHT, SWT.TOP).
        grab(true, false).span(2, 1).create)
    upd.addSelectionListener(new SelectionAdapter {
      override def widgetSelected(ev : SelectionEvent) =
        if (getElement != null) {
          import org.eclipse.core.runtime.IProgressMonitor
          import org.eclipse.core.resources.{
            IWorkspace, ResourcesPlugin, IWorkspaceRunnable}
          ResourcesPlugin.getWorkspace().run(new IWorkspaceRunnable {
            override def run(monitor : IProgressMonitor) =
              CoqBuildScript.install(getElement)
          }, getElement, IWorkspace.AVOID_UPDATE, null)
          updateControls
        }
    })
    updateButton = Some(upd)

    new Label(c1, SWT.HORIZONTAL | SWT.SEPARATOR).setLayoutData(
        GridDataFactory.fillDefaults.grab(true, false).span(2, 1).create)
    
    val autob = new Button(c1, SWT.CHECK)
    autob.setLayoutData(GridDataFactory.fillDefaults.grab(true, false).
        span(2, 1).create)
    autob.setText("Automatically update the build script for this project")
    autob.setToolTipText("If this option is enabled, then the build " +
                         "process for this project will automatically " +
                         "update its " +
                         "build script when a new version is available. " +
                         "(Setting this option on several computers may " +
                         "lead to merge conflicts when using version " +
                         "control systems.)")
    automaticCheckbox = Some(autob)

    updateControls

    c1
  }

  private def updateControls() =
    Option(getElement) match {
      case Some(p : IProject) =>
        val version = CoqBuildScript.extractScriptVersion(p)
        version match {
          case Some(v) =>
            projectVersionLabel.foreach(_.setText(s"${v}"))
            updateButton.foreach(upd => {
              if (v < CoqBuildScript.currentVersion) {
                upd.setEnabled(true)
                upd.setText("Update build script")
              } else {
                upd.setEnabled(false)
                upd.setText("Already up-to-date")
              }
            })
          case _ =>
            projectVersionLabel.foreach(_.setText("none found"))
            updateButton.foreach(upd => {
              /* Since we were unable to find the version number, as a safety
               * precaution, only enable the button if the file doesn't already
               * exist */
              if (p.getFile("configure.coqoon.py").exists) {
                upd.setEnabled(false)
                upd.setText("Unrecognised version")
              } else {
                upd.setEnabled(true)
                upd.setText("Add build script")
              }
            })
        }
        automaticCheckbox.foreach(autob => {
          val auto = CoqBuildScript.WriteBuildScript.get(p).getOrElse(false)
          if (autob.getSelection != auto)
            autob.setSelection(auto)
        })
      case _ =>
    }
}
