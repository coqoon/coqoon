package dk.itu.sdg.javaparser

/**
 * Parser for an untyped lambda calculus
 *
 * Usage: scala examples.parsing.lambda.Main <file>
 *
 * (example files: see test/ *.kwi)
 *
 * @author Miles Sabin (adapted slightly by Adriaan Moors)
 *
 * adapted by Paul Phillips (https://github.com/paulp/scala-lang-combinators)
 *
 * adapted by Hannes Mehnert (https://github.com/hannesm/Kopitiam)
 */
object Main extends Application with JavaOutputter with JavaAST
{
  import java.io.{FileInputStream,InputStreamReader,OutputStreamWriter,FileOutputStream,File}
  import scala.util.parsing.input.StreamReader

  override def main(args: Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    val in = StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8"))
    val res = FinishAST.doitHelper(parseH(in)).map(out(_, 0)).reduceLeft(_ + "\n\n" + _)
    Console.println(res)
  }
}
