/* (c) 2012 HAnnes Mehnert */

package dk.itu.sdg.javaparser

//import scala.util.parsing.input.Position
import dk.itu.sdg.parsing.LengthPosition
import dk.itu.sdg.util.KopitiamLogger

case class SJWarning (message : String, position : LengthPosition) { }

object SimpleJavaChecker extends KopitiamLogger {
  private var warnings : List[SJWarning] = List[SJWarning]()

  def warn (msg : String, pos : LengthPosition) : Unit = {
    warnings ::= new SJWarning(msg, pos)
  }

  def check (xs : List[SJDefinition]) : List[SJWarning] = {
    warnings = List[SJWarning]()
    xs.foreach(checkDefinition(_))
    warnings
  }

  def checkDefinition (d : SJDefinition) : Unit = {
    d match {
      case SJClassDefinition(m, id, sup, inter, body, outer, fields) =>
        checkBodyDefinitions(body)
      case _ =>
    }
  }

  def checkBodyDefinitions (d : List[SJBodyDefinition]) : Unit = {
    var i : Int = 0
    for (x <- d) {
      x match {
        case y@SJMethodDefinition(m, id, jtype, params, body, locals) =>
          if ((i == 0) || (! d(i - 1).isInstanceOf[Postcondition]))
            warn("no postcondition provided for method", y.pos)
          else if (i > 0) {
            val post = d(i - 1).asInstanceOf[Postcondition].data
            if (jtype == "void") {
              if (post.startsWith("\"") && post.contains("\","))
                warn("return type is void, but postcondition contains a return value", y.pos)
            } else
              if (! (post.startsWith("\"") && post.contains("\",")))
                warn("no binding to return value in postcondition", y.pos)
          }
          if ((i < 2) || (! d(i - 2).isInstanceOf[Precondition]))
            warn("no precondition provided for method", y.pos)
          if ((i < 3) || (! d(i - 3).isInstanceOf[Quantification]))
            warn("no logical variables provided for method", y.pos)
          checkStatements(body)
        case _ =>
      }
      i = i + 1
    }
  }

  import dk.itu.sdg.kopitiam.CoqTop
  import dk.itu.sdg.parsing.NoLengthPosition
  def checkStatements (b : List[SJStatement]) : Unit = {
    var i : Int = 0
    for (x <- b) {
      log.info("AST node" + x + " position: " + x.pos)
      x match {
        case y : SJConditional =>
          if (y.pos == NoLengthPosition)
            log.info("no position information for conditional...")
          checkStatements(y.consequent)
          checkStatements(y.alternative)
        case y : SJWhile =>
          if (y.pos == NoLengthPosition)
            log.info("no position information for while...")
          if ((i == 0) || (! b(i - 1).isInstanceOf[Loopinvariant]))
            warn("missing loop invariant", y.pos)
          checkStatements(y.body)
        case x@RawSpecification(data) =>
          if (x.pos == NoLengthPosition)
            log.info("no position information for spec...")
          if (data.contains("\n"))
            warn("proof script or specification contains a newline, that is invalid", x.pos)
          val eoc = CoqTop.findNextCommand(data)
          if (eoc == -1)
            warn("no command found", x.pos)
          else if (CoqTop.findNextCommand(data.drop(eoc)) != -1)
            warn("multiple commands found", x.pos)
        case x =>
          if (x.pos == NoLengthPosition)
            log.info("no position information for " + x)
      }
      i = i + 1
    }
  }
}

