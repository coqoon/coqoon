/* UIUtils.scala
 * Convenience methods and wrappers for interacting with the UI thread
 * Copyright © 2013 Alexander Faithfull
 * 
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.sdg.kopitiam

import org.eclipse.ui.PlatformUI

object UIUtils {
  import org.eclipse.ui.PlatformUI
  def getDisplay = PlatformUI.getWorkbench.getDisplay
  
  def syncExec(r : Runnable) : Unit = getDisplay.syncExec(r)
  def syncExec(r : => Unit) : Unit = syncExec(new Runnable() {
    override def run = r
  })
  
  def asyncExec(r : Runnable) : Unit = getDisplay.asyncExec(r)
  def asyncExec(r : => Unit) : Unit = asyncExec(new Runnable() {
    override def run = r
  })
}