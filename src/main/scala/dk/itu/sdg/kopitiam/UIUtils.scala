/* UIUtils.scala
 * Convenience methods and wrappers for interacting with the UI thread
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

object UIUtils {
  import org.eclipse.ui.PlatformUI
  def getWorkbench = PlatformUI.getWorkbench
  def getDisplay = getWorkbench.getDisplay
  def getActiveShell = getDisplay.getActiveShell
  
  def syncExec(r : Runnable) : Unit = getDisplay.syncExec(r)
  def syncExec(r : => Unit) : Unit = syncExec(new Runnable() {
    override def run = r
  })
  
  def asyncExec(r : Runnable) : Unit = getDisplay.asyncExec(r)
  def asyncExec(r : => Unit) : Unit = asyncExec(new Runnable() {
    override def run = r
  })
  
  object Dialog {
    import org.eclipse.swt.widgets.Shell
    protected def bindStockDialog[A](
        f : (Shell, String, String) => A) : (String, String) => A =
      f(getDisplay.getActiveShell, _ : String, _ : String)
    
    import org.eclipse.jface.dialogs.MessageDialog
    def confirm = bindStockDialog(MessageDialog.openConfirm)
    def error = bindStockDialog(MessageDialog.openError)
    def information = bindStockDialog(MessageDialog.openInformation)
    def question = bindStockDialog(MessageDialog.openQuestion)
    def warning = bindStockDialog(MessageDialog.openWarning)
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
}

class SupersedableTask(delay : Long) {
  private val lock = new Object
  
  import java.util.TimerTask
  
  var last : Option[TimerTask] = None
  
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
