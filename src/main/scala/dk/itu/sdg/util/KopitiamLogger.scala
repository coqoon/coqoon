package dk.itu.sdg.util

import java.util.logging.{ Logger, FileHandler, ConsoleHandler, Level }

/**
* Mix in this trait if you want you be able to log message from your class.
*/
trait KopitiamLogger {
  protected val log = Logger.getLogger(this.getClass().getName());
  log.setLevel(Level.INFO)
}
