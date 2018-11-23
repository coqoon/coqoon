package dk.itu.coqoon.core.coqtop

import dk.itu.coqoon.core.CoqoonPreferences
import dk.itu.coqoon.core.utilities.TotalReader

trait CoqProgram {
  def path() : String
  def check() : Boolean = new java.io.File(path).exists
  def run(args : Seq[String],
      start : ProcessBuilder => Process = (a => a.start)) : CoqProgramInstance
  def version() = run(Seq("-v")).readAll match {
    case (0, CoqProgram.Version(v)) =>
      Some(v)
    case _ =>
      None
  }
}
object CoqProgram extends CoqProgram {
  private final val Version = raw"version (.+) ".r.unanchored

  private trait RunPOSIXProgram extends CoqProgram {
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplPOSIX(path +: args, start)
  }
  private trait RunWindowsProgram extends CoqProgram {
    override def run(args : Seq[String],
        start : ProcessBuilder => Process) : CoqProgramInstance =
      new CoqProgramInstanceImplWindows(path +: args, start)
  }

  private abstract class PathFromPreferencesProgram(
      name : String) extends CoqProgram {
    override def path : String = CoqoonPreferences.CoqPath.get match {
      case Some(path) => path + java.io.File.separator + name
      case _ => name
    }
  }
  private abstract class FixedPathProgram(path_ : String) extends CoqProgram {
    override def path : String = path_
  }

  private class PrefProgramImpl(name : String)
      extends PathFromPreferencesProgram(name) with RunPOSIXProgram
  private class PrefProgramImplWindows(name : String)
      extends PathFromPreferencesProgram(name + ".exe") with RunWindowsProgram

  private class FixedProgramImpl(path_ : String)
      extends FixedPathProgram(path_) with RunPOSIXProgram
  private class FixedProgramImplWindows(path_ : String)
      extends FixedPathProgram(path_) with RunWindowsProgram

  private lazy val program : CoqProgram =
    if (PlatformUtilities.isWindows) {
      new PrefProgramImplWindows("coqtop")
    } else new PrefProgramImpl("coqtop")

  override def path = program.path
  override def check = program.check
  override def run(args : Seq[String],
      start : ProcessBuilder => Process = (a => a.start)) =
    program.run(args, start)

  def makeProgram(path : String) : CoqProgram =
    if (PlatformUtilities.isWindows) {
      new FixedProgramImplWindows(path + ".exe")
    } else new FixedProgramImpl(path)
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