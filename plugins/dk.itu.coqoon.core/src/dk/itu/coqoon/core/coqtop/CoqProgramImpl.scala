package dk.itu.coqoon.core.coqtop

import dk.itu.coqoon.core.debug.CoqoonDebugPreferences

private class CoqProgramInstanceImpl(argv : Seq[String],
    start : ProcessBuilder => Process) extends CoqProgramInstance {
  import dk.itu.coqoon.core.CoqoonPreferences
  CoqoonDebugPreferences.PrintProcessInvocations.log(
      "RUN " + argv.mkString("[", ", ", "]"))

  protected val (in, out, pr) = {
    import java.io.{InputStreamReader, OutputStreamWriter}

    val pb = new ProcessBuilder(argv : _*)
    val pr = start(pb)
    val in = new OutputStreamWriter(pr.getOutputStream, "UTF-8")
    val out = new InputStreamReader(pr.getInputStream, "UTF-8")
    (in, out, pr)
  }

  override def stdin = in
  override def stdout = out

  override def kill = pr.destroy
  override def waitFor = pr.waitFor
  override def interrupt = ()
}

private class CoqProgramInstanceImplWindows(
    argv : Seq[String], start : ProcessBuilder => Process)
        extends CoqProgramInstanceImpl(argv.map(a => '"' + a + '"'), start) {
  override def interrupt = if (pr != null) {
    import WindowsConsoleUtilities._
    CoqProgramInstanceImplWindows.extractHandle(
        pr).map(getProcessId).foreach(pid => {
      freeConsole
      attachConsole(pid)
      ignoreCtrlC
      sendCtrlC
      unignoreCtrlC
      freeConsole
    })
  }
}
private object CoqProgramInstanceImplWindows {
  def extractHandle(p : Process) : Option[Long] = try {
    p.exitValue
    None
  } catch {
    case _ : IllegalThreadStateException =>
      val klass = p.getClass
      val handleField = try {
        Some(klass.getDeclaredField("handle"))
      } catch {
        case _ : NoSuchFieldException => None
      }
      handleField.map(hf => {
        hf.setAccessible(true)
        hf.getLong(p)
      })
  }
}

private class CoqProgramInstanceImplPOSIX(argv : Seq[String],
    start : ProcessBuilder => Process) extends CoqProgramInstanceImpl(
        Seq("/bin/sh", "-c", "echo $$; exec \"$@\"", "wrapper") ++ argv,
            start) {
  private var pid : String = {
    var pid = ""
    var a : Int = stdout.read
    while (a != -1 && a != '\n') {
      pid += a.asInstanceOf[Char]
      a = stdout.read
    }
    pid
  }

  import scala.sys.process.Process
  override def interrupt() = Process(Seq("kill", "-INT", pid)).run
}

private object PlatformUtilities {
  def getOSName = System.getProperty("os.name").toLowerCase
  def isWindows = getOSName.contains("windows")
}