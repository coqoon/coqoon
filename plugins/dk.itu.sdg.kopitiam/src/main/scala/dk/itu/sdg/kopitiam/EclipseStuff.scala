/* (c) 2010-2012 Hannes Mehnert and David Christiansen
 * Copyright © 2013 Alexander Faithfull */

package dk.itu.sdg.kopitiam

object EclipseConsole {
  private val lock = new Object
  import org.eclipse.ui.console.{
    MessageConsole,MessageConsoleStream,ConsolePlugin}

  private var console_ : Option[MessageConsole] = None
  private var out_ : Option[MessageConsoleStream] = None
  private var err_ : Option[MessageConsoleStream] = None

  private def console : MessageConsole = lock synchronized {
    console_ match {
      case Some(a) => a
      case None =>
        console_ = Some(new MessageConsole("Coq", null))
        ConsolePlugin.getDefault.getConsoleManager.addConsoles(
            console_.toArray)
        console_.get
    }
  }

  def out : MessageConsoleStream = lock synchronized {
    out_ match {
      case Some(a) => a
      case None =>
        out_ = Some(console.newMessageStream)
        out_.foreach(_.setEncoding("UTF-8"))
        out_.get
    }
  }

  def err : MessageConsoleStream = lock synchronized {
    err_ match {
      case Some(a) => a
      case None =>
        err_ = Some(console.newMessageStream)
        err_.foreach(a => {
          a.setEncoding("UTF-8")
          UIUtils.exec {
            a.setColor(RED)
          }
        })
        err_.get
    }
  }

  import org.eclipse.swt.graphics.Color
  private final val RED = new Color(UIUtils.getDisplay, 255, 0, 0)
}
