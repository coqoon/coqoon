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
  
  def openWarning(title : String, message : String) = syncExec {
    org.eclipse.jface.dialogs.MessageDialog.openWarning(
        getDisplay.getActiveShell, title, message)
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