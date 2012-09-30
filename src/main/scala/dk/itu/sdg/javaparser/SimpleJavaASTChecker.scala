/* (c) 2012 HAnnes Mehnert */

package dk.itu.sdg.javaparser

import scala.util.parsing.input.Position
case class SJWarning (message : String, position : Position) { }

object SimpleJavaChecker {
  private var warnings : List[SJWarning] = List[SJWarning]()

  def warn (msg : String, pos : Position) : Unit = {
    warnings ::= new SJWarning(msg, pos)
  }

  def check (xs : List[SJDefinition]) : List[SJWarning] = {
    warnings = List[SJWarning]()
    Console.println("trollolo checking!!!")
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
          if ((i < 2) || (! d(i - 2).isInstanceOf[Precondition]))
            warn("no precondition provided for method", y.pos)
          if ((i < 3) || (! d(i - 3).isInstanceOf[Quantification]))
            warn("no quantification provided for method", y.pos)
          checkStatements(body)
        case _ =>
      }
      i = i + 1
    }
  }

  import dk.itu.sdg.kopitiam.CoqTop
  def checkStatements (b : List[SJStatement]) : Unit = {
    var i : Int = 0
    for (x <- b) {
      x match {
        case y : SJWhile =>
          if ((i == 0) || (! b(i - 1).isInstanceOf[Loopinvariant]))
            warn("missing loop invariant", y.pos)
        case x@RawSpecification(data) =>
          if (data.contains("\n"))
            warn("proof script or specification contains a newline, that is invalid", x.pos)
          val eoc = CoqTop.findNextCommand(data)
          if (eoc == -1)
            warn("no command found", x.pos)
          else if (CoqTop.findNextCommand(data.drop(eoc)) != -1)
            warn("multiple commands found", x.pos)
        case _ =>
      }
      i = i + 1
    }
  }
}

