import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info)
{
  
  // Forcing SBT to use the project structure that Eclipse uses
  override def outputDirectoryName = "bin"
  override def mainScalaSourcePath = "src"
  override def testScalaSourcePath = "automated-tests"
   
  override def libraryDependencies = Set(
    "org.scalatest" % "scalatest" % "1.3" % "test->default"
  ) ++ super.libraryDependencies  
}
