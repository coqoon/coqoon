package dk.itu.sdg.javaparser

/**
 * Parser for an untyped lambda calculus
 *
 * Usage: scala examples.parsing.lambda.Main <file>
 *
 * (example files: see test/ *.kwi)
 *
 * @author Miles Sabin (adapted slightly by Adriaan Moors)
 */
object Main extends Application with JavaAST
{
  import java.io.{FileInputStream,InputStreamReader,PrintWriter,File}
  import scala.util.parsing.input.StreamReader

  override def main(args: Array[String]) = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "ISO-8859-1"))
    val outfile = args(0) + ".v"
    val out = new PrintWriter(new File(outfile))
    val res = parse(in)
    out.println(res)
    out.close
  }
}
