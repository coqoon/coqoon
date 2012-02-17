package dk.itu.sdg.javaparser

/*
 * A collection of objects with main methods that we can start through SBT.
 * They're simply used for quick testing and playing with the code while you're
 * developing.
 *
 * They can be started by running 'run arg0 arg1' etc. in an SBT session. See each
 * main method for information about the specific arguments needed
 *
 */

import scala.io.Source
import scala.util.parsing.input.CharArrayReader

trait Playground {

  def printBeforeAndAfter(txts: (String,String)*): Unit = {
    println("\n")
    txts foreach { case (label, program) =>
      println("##%s\n".format(label))
      println(program)
    }
    println("\n")
  }
}

/*
 * Use this to parse a java file and output the resulting non-optimized simple java program.
 *
 *  - Original Java program
 *  - Un-optimized Simple Java Program
 *
 * Arguments
 *   arg0: The path to the file that contains that java program you want to translate
 */
object SimpleJavaMain extends App with JavaOutputter with Playground {

  override def main(args: Array[String]): Unit = {

    val path = args(0)
    val progText = Source.fromFile(path).mkString
    val simpleJavaProg = JavaOutput.parseandoutput(progText)

    printBeforeAndAfter(
      ("Java Program",progText),
      ("Simple Java Program", simpleJavaProg)
    )
  }

}

/*
 * Use this to parse a java file and output the:
 *
 *  - Original Java program
 *  - Un-optimized Simple Java Program
 *  - Optimized Simple Java Program
 *
 * Arguments
 *   arg0: The path to the file that contains that java program you want to translate
 */
object SimpleJavaOptimizedMain extends App with JavaAST with JavaOutputter with Playground  {
  import dk.itu.sdg.analysis.Optimizer

  val path = args(0)
  val progText = Source.fromFile(path).mkString

  val simpleJavaProg = JavaOutput.parseandoutput(progText)

  val ast = FinishAST.doitHelper(parseH(new CharArrayReader(progText.toArray)))
  val optimizedAST = Optimizer.removeDeadVariables(ast)

  val optimizedProgText = optimizedAST.map( (x: SJDefinition) => outputDefinition(x,0) ).mkString("")

  printBeforeAndAfter(
    ("Java Program",progText),
    ("Simple Java Program", simpleJavaProg),
    ("Optimized Simple Java Program",optimizedProgText)
  )
}