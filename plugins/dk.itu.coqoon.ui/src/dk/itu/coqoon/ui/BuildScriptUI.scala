/* BuildScriptUI.scala
 * A preference page for checking and updating project build scripts
 * Copyright © 2014, 2015 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.model.ICoqModel
import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.ui.IWorkbenchPropertyPage
import org.eclipse.swt.widgets.{Composite, Button, Label}
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
          performInstall
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
    import org.eclipse.core.runtime.IStatus
    import org.eclipse.jface.window.Window

    import dk.itu.coqoon.ui.utilities.{UIXML, Event, Listener}
    val names = UIXML(
        <composite name="root">
          <grid-layout columns="2" />
          <label>
            <grid-data />
            Project version:
          </label>
          <label name="cur">
            <grid-data h-grab="true" />
            <tool-tip>
              The version of the build script currently included in this
              project.
            </tool-tip>
          </label>
          <label>
            <grid-data />
            Most recent version:
          </label>
          <label name="mrl">
            <grid-data h-grab="true" />
            <tool-tip>
              The most recent version of the build script known to this version
              of Coqoon.
            </tool-tip>
          </label>
          <composite>
            <row-layout type="horizontal" />
            <grid-data h-grab="true" h-span="2" h-align="end" />
            <button name="upd" />
            <button name="var">
              Update configure.coqoon.vars
            </button>
          </composite>
          <label separator="horizontal">
            <grid-data h-fill="true" h-span="2" />
          </label>
          <button style="check" name="auto">
            <grid-data h-grab="true" h-span="2" />
            Automatically update the build script for this project
            <tool-tip>
              If this option is enabled, then the build process for this
              project will automatically update its build script when a new
              version is available. (Setting this option on several computers
              may lead to merge conflicts when using version control systems.)
            </tool-tip>
          </button>
        </composite>, c)

    projectVersionLabel = names.get[Label]("cur")
    names.get[Label]("mrl").foreach(
        _.setText(s"${CoqBuildScript.currentVersion}"))

    updateButton = names.get[Button]("upd")
    Listener.Selection(updateButton.get, Listener {
      case Event.Selection(_) if getElement != null =>
        performInstall
        updateControls
    })

    val reb = names.get[Button]("var").get
    Listener.Selection(reb, Listener {
      case Event.Selection(_) if getElement != null =>
        performVarsUpdate
    })

    automaticCheckbox = names.get("auto")

    updateControls
    names.get[Composite]("root").get
  }

  private def performInstall() =
    runUnderProject(CoqBuildScript.install(getElement))
  private def performVarsUpdate() =
    runUnderProject(CoqBuildScript.generateVars(
        ICoqModel.toCoqProject(getElement)))

  /* This blocks for the duration, which isn't always ideal */
  private def runUnderProject(f : => Unit) = {
    import org.eclipse.core.runtime.IProgressMonitor
    import org.eclipse.core.resources.{
      IWorkspace, ResourcesPlugin, IWorkspaceRunnable}
    ResourcesPlugin.getWorkspace().run(new IWorkspaceRunnable {
      override def run(monitor : IProgressMonitor) =
        f
      }, getElement, IWorkspace.AVOID_UPDATE, null)
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
                upd.setText("Update script")
              } else {
                upd.setEnabled(false)
                upd.setText("Script is already up-to-date")
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
                upd.setText("Script version is unrecognised")
              } else {
                upd.setEnabled(true)
                upd.setText("Add script")
              }
            })
        }
        updateButton.foreach(_.getShell.pack)
        automaticCheckbox.foreach(autob => {
          val auto = CoqBuildScript.WriteBuildScript.get(p).getOrElse(false)
          if (autob.getSelection != auto)
            autob.setSelection(auto)
        })
      case _ =>
    }
}
