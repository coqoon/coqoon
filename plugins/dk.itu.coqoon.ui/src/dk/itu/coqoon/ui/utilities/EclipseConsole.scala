/* EclipseConsole.scala
 * Wrappers around the standard output and error streams of the Coq console
 * Copyright © 2013, 2014 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

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
