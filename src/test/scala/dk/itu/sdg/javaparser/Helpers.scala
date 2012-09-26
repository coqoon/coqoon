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
  def getJavaASTbyParsingFileNamed(name : String,
                                   path: List[String] = List("src", "test", "resources", "javaparser", "source")) : List[JStatement] = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(getSourceFileNamed(name,path))))
    FinishAST.javaTermsToJavaAST(parseH(in))
  }

  def getASTbyParsingFileNamed(name : String,
                               path: List[String] = List("src", "test", "resources", "javaparser", "source")) : List[SJDefinition] = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(getSourceFileNamed(name,path))))
    FinishAST.doitHelper(parseH(in))
  }

  def getSourceFileNamed(name : String, path: List[String]): File = {
    new File((path :+ name).mkString(File.separator))
  }

  def getCoqOutputFromFile (classfile : String, name : String) : List[String] = {
    val parsed = getASTbyParsingFileNamed(classfile)
    val (p, r) = FinishAST.coqoutput(parsed, false, name)
    p
  }
}
