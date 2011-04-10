import sbt._

import java.io._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends DefaultProject(info) with ScctProject {
  // Reading the version from MA
  override def version = OpaqueVersion(readVersion)

  // Forcing SBT to use the project structure that Eclipse uses
  override def outputDirectoryName = "bin"

  override def libraryDependencies = Set(
    "org.scalatest" % "scalatest" % "1.3" % "test->default"
  ) ++ super.libraryDependencies

  /*
    Read the version from the MANIFEST.MF file.
  */
  def readVersion : String = {
    val manifestFile = new File(List("META-INF","MANIFEST.MF").mkString(File.separator))
    val manifest     = readContentsOfFile(manifestFile)
    val r = """Bundle-Version: (\S*)""".r
    r.findFirstMatchIn(manifest).map( _.group(1) ).get
  }

  /*
    Can't use any of the code in the project so have to implement this
    instead of using dk.itu.sdg.util.IO
  */
  def readContentsOfFile(file : File) : String = {
    val reader = new BufferedReader(new FileReader(file))
    var text = ""
    var line = reader.readLine()
    while (line != null) {
      text = text + " " + line
      line = reader.readLine()
    }
    text
  }
}
