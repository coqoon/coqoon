package dk.itu.sdg.javaparser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.StreamReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.PrintWriter

import scala.util.parsing.input._

trait JavaAST extends JavaParser {
  def parse(r: Reader[Char], out : PrintWriter) : ParseResult[Any] = {
    val p = phrase(compilationUnit)(new lexical.Scanner(r))
    p match {
      case Success(x @ ~(_,_), _) =>
        val ast = FinishAST.doit(x, out);
      case Failure(msg, remainder) => Console.println("Failure: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
      case Error(msg, remainder) => Console.println("Error: "+msg+"\n"+"Remainder: \n"+remainder.pos.longString)
    }
    p
  }
}

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
  override def main(args: Array[String]) = {
    val in = StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "ISO-8859-1"))
    val outfile = args(0) + ".v"
    val out = new PrintWriter(new File(outfile))
    parse(in, out)
    out.close
  }
}
