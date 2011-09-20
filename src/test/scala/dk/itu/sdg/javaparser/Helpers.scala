/*
  Contains things to make testing more convenient. 
*/

package dk.itu.sdg.javaparser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.{ InputStreamReader, FileInputStream, File }
import scala.util.parsing.input.{ StreamReader }

trait ASTSpec extends FlatSpec with ShouldMatchers with JavaAST {
  /*
    Returns the JavaAST produced by parsing the file named "name" inside of the
    folder src/test/resources/javaparser/source.
  */
  def getJavaASTbyParsingFileNamed(name : String) : List[JStatement] = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(getSourceFileNamed(name))))
    FinishAST.javaTermsToJavaAST(parseH(in))
  }

  def getASTbyParsingFileNamed(name : String) : List[SJDefinition] = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(getSourceFileNamed(name))))
    FinishAST.doitHelper(parseH(in))
  }

  def getSourceFileNamed(name : String) : File = {
    new File(List("src", "test", "resources", "javaparser", "source", name).mkString(File.separator))
  }

  def getCoqOutputFromFile (classfile : String, name : String) : List[String] = {
    val parsed = getASTbyParsingFileNamed(classfile)
    FinishAST.coqoutput(parsed, false, name)
  }
}
