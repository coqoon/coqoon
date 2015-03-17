/* UIUtils.scala
 * Convenience methods and wrappers for interacting with the UI thread
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui.utilities

import dk.itu.coqoon.ui.Activator
import dk.itu.coqoon.core.utilities.TryService

object UIUtils {
  import org.eclipse.swt.widgets.Shell

  import org.eclipse.ui.PlatformUI
  def getWorkbench = PlatformUI.getWorkbench
  def getDisplay = getWorkbench.getDisplay
  def getActiveShell = getDisplay.getActiveShell

  import org.eclipse.ui.model.{
    WorkbenchLabelProvider, WorkbenchContentProvider}
  import org.eclipse.ui.dialogs.ElementTreeSelectionDialog
  def createWorkspaceElementDialog(parent : Shell) =
    new ElementTreeSelectionDialog(parent,
        WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider(),
        new WorkbenchContentProvider)

  def exec[A](r : => A) : A = {
    object ResultHolder {
      var result : A = _
    }
    getDisplay.syncExec(new Runnable() {
      override def run =
        ResultHolder synchronized (ResultHolder.result = r)
    })
    ResultHolder synchronized (ResultHolder.result)
  }

  def asyncExec(r : => Unit) : Unit = getDisplay.asyncExec(new Runnable() {
    override def run = r
  })

  object Dialog {
    import org.eclipse.jface.dialogs.MessageDialog._
    def confirm(t : String, m : String) = openConfirm(getActiveShell, t, m)
    def error(t : String, m : String) = openError(getActiveShell, t, m)
    def information(t : String, m : String) =
      openInformation(getActiveShell, t, m)
    def question(t : String, m : String) = openQuestion(getActiveShell, t, m)
    def warning(t : String, m : String) = openWarning(getActiveShell, t, m)
  }

  import org.eclipse.ui.IEditorPart
  def getActionBars(editor : IEditorPart) =
    editor.getEditorSite.getActionBars
  def getStatusLineManager(editor : IEditorPart) =
    getActionBars(editor).getStatusLineManager
  def getProgressMonitor(editor : IEditorPart) =
    getStatusLineManager(editor).getProgressMonitor

  object Color {
    import org.eclipse.swt.graphics.{RGB, Color}
    def apply(r : Int, g : Int, b : Int) : Color =
      new Color(getDisplay, r, g, b)
    def apply(t : (Int, Int, Int)) : Color = apply(t._1, t._2, t._3)
    def apply(c : RGB) = new Color(getDisplay, c)

    def fromPreference(key : String) =
      apply(org.eclipse.jface.preference.PreferenceConverter.getColor(
          Activator.getDefault.getPreferenceStore, key))
  }

  import org.eclipse.ui.commands.ICommandService
  def refreshElements(commandIdentifier : String) : Unit =
    TryService[ICommandService](UIUtils.getWorkbench).foreach(
        _.refreshElements(commandIdentifier, null))
}

class SupersedableTask(delay : Long) {
  private val lock = new Object

  import java.util.TimerTask

  private var last : Option[TimerTask] = None

  def schedule(f : => Unit) : Unit = lock synchronized {
    last.map(_.cancel)
    last = Some(new TimerTask() {
      override def run = { f }
    })
    last.map(SupersedableTask.timer.schedule(_, delay))
  }
}
object SupersedableTask {
  private val lock = new Object

  import java.util.Timer
  private val timer = new Timer()

  def purge() : Unit = timer.purge()
}
