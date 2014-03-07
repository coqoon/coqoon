/* (c) 2010-2012 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.coqoon.ui.utilities

import dk.itu.coqoon.core.utilities.CacheSlot

object EclipseConsole {
  import org.eclipse.ui.console._

  private val console_ = CacheSlot[MessageConsole] {
    val c = new MessageConsole("Coq", null)
    ConsolePlugin.getDefault.getConsoleManager.addConsoles(Array(c))
    c
  }
  private val out_ = CacheSlot[MessageConsoleStream] {
    val s = console_.get.newMessageStream
    s.setEncoding("UTF-8")
    s
  }
  private val err_ = CacheSlot[MessageConsoleStream] {
    val s = console_.get.newMessageStream
    s.setEncoding("UTF-8")
    UIUtils.exec {
      s.setColor(RED)
    }
    s
  }

  def out = out_.get
  def err = err_.get

  import org.eclipse.swt.graphics.Color
  private final val RED = new Color(UIUtils.getDisplay, 255, 0, 0)
}
