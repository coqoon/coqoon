package dk.itu.coqoon.core.coqtop

import dk.itu.coqoon.core.CoqoonPreferences
import dk.itu.coqoon.core.utilities.TotalReader

trait CoqProgram {
  def path() : String
  def check() : Boolean = new java.io.File(path).exists
  def run(args : Seq[String],
      start : ProcessBuilder => Process = (a => a.start)) : CoqProgramInstance
}
object CoqProgram extends CoqProgram {
  private class ProgramImpl(name : String) extends CoqProgram {
    override def path : String = CoqoonPreferences.CoqPath.get match {
      case Some(path) => path + java.io.File.separator + name
      case _ => name
    }
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplPOSIX(path +: args, start)
  }

  private class ProgramImplWindows(
      name : String) extends ProgramImpl(name + ".exe") {
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplWindows(path +: args, start)
  }

  private lazy val program : CoqProgram =
    if (PlatformUtilities.isWindows) {
      new ProgramImplWindows("coqtop")
    } else new ProgramImpl("coqtop")

  override def path = program.path
  override def check = program.check
  override def run(args : Seq[String],
      start : ProcessBuilder => Process = (a => a.start)) =
    program.run(args, start)

  def version() = run(Seq("-v")).readAll match {
    case (0, Version(v)) =>
      Some(v)
    case _ =>
      None
  }

  private final val Version = raw"version (.+) ".r.unanchored
}

trait CoqProgramInstance {
  import java.io.{Reader, Writer}

  def stdin : Writer
  def stdout : Reader
  def readAll : (Int, String) = {
    val r = TotalReader.read(stdout)
    (waitFor, r)
  }

  def kill
  def waitFor : Int
  def interrupt
}