package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.util.parsing.input.StreamReader
import dk.itu.sdg.javaparser._
import dk.itu.sdg.util.{ IO }

/*
  Parse each file in automated-tests/resources/javaparser/source and
  test that the result is equal to the contents of the file with the same
  name in the automated-tests/resources/javaparser/expected folder.
*/

class JavaParserSpec extends FlatSpec with ShouldMatchers 
                                      with JavaOutputter 
                                      with JavaAST {

  val pathToSource   = List("src","test","resources","javaparser","source").mkString(File.separator)
  val pathToExpected = List("src","test","resources","javaparser","expected").mkString(File.separator)

  System.setProperty("file.encoding", "UTF-8")

  "Result of parsing all files in source" should 
  "equal to the contents of the associated file in expected" in {

    val source     = new File(pathToSource)
    source.listFiles().foreach { file =>
      val in       = StreamReader(new InputStreamReader(new FileInputStream(file)))
      val result   = FinishAST.doitHelper(parseH(in)).map(out(_, 0)).reduceLeft(_ + " " + _)                                    
      val expected = getExpectedOutputOfFile(file)
      simplify(result) should equal(simplify(expected))
    }
  }

  /*
    simplifies strings so they're comparable
  */
  def simplify(text : String) : String = {
    text.split('\n').filterNot(_ == "")
                    .reduceLeft(_ + " " + _)
                    .replace("    "," ")
                    .replace("   "," ")
                    .replace("  "," ")
                    .trim()
  }

  /*
    given a file in sources it will find the file with the expected result of 
    parsing the file.
  */
  def getExpectedOutputOfFile(file : File) : String = {
    val path = List(pathToExpected, file.getName).mkString(File.separator)
    IO.readContentsOfFile(new File(path))
  }

}
