package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.util.parsing.input.StreamReader
import dk.itu.sdg.javaparser._

class JavaParserSpec extends FlatSpec with ShouldMatchers with JavaOutputter with JavaAST {

  System.setProperty("file.encoding", "UTF-8")

  /*
    Parse each file in automated-tests/resources/javaparser/source and
    test that result is equal to the contents of the file with the same
    name in the automated-tests/resources/javaparser/expected folder.
  */
  "Result of parsing all files in source" should "equal to the contents of the associated file in expected" in {

    val sourcePath = List("automated-tests", "resources", "javaparser", "source").mkString(File.separator)
    val source = new File(sourcePath)

    source.listFiles().foreach { file =>
      val in = StreamReader(new InputStreamReader(new FileInputStream(file)))
      val result = FinishAST.doitHelper(parseH(in)).map(out(_, 0)).reduceLeft(_ + "" + _).trim()
      val expected = readContentsOfFile(file).trim()
      result should equal(expected)
    }
  }

  // given a file in sources it will find the file with the expected result of parsing
  // the file.
  def getExpectedOutputOfFile(file : File) : String = {
    val path = List("automated-tests", "resources", "javaparser", "expected", file.getName).mkString(File.separator)
    readContentsOfFile(new File(path))
  }

  // Returns the contents of a file as a String
  def readContentsOfFile(file : File) : String = {
    val reader = new BufferedReader(new FileReader(file))
    var text = ""
    var line = reader.readLine()
    while (line != null) {
      text = text + line
      line = reader.readLine()
    }
    text
  }

}
