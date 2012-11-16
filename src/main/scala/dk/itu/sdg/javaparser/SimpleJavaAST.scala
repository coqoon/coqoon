/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.collection.immutable.HashMap
import dk.itu.sdg.parsing.LengthPositional
//import scala.util.parsing.input.Positional

sealed trait CPosition {
  def offset : Int
  def length : Int

  override def toString = "(" + offset + ", " + length + ")"
}

case object NoCPosition extends CPosition {
  def offset = 0
  def length = 0

  override def toString = "(no position)"
}

case class CRegion (offset : Int, length : Int) extends CPosition { }

trait CoqPositional {
  private var coqPos : CPosition = NoCPosition

  def setCoqPos (o : Int, l : Int) : this.type = {
    this.coqPos = CRegion(o, l)
    this
  }

  def getCoqPos () : CPosition = { this.coqPos }

  private var javaPos : CPosition = NoCPosition
  def setJavaPos (o : Int, l : Int) : this.type = {
    this.javaPos = CRegion(o, l)
    this
  }

  def getJavaPos () : CPosition = { this.javaPos }
}

trait CoqOutput {
  private var coqString : Option[String] = None

  def setCoqString (s : Option[String]) : Unit = { coqString = s }
  def getCoqString () : Option[String] = coqString
  def prependCoqString (s : String) : Unit = {
    coqString = Some(s + "\n" + coqString.getOrElse(""))
  }
  def appendCoqString (s : String) : Unit = {
    coqString match {
      case None => coqString = Some(s)
      case Some(x) => coqString = Some(x + "\n" + s)
    }
  }

  def getLength () : Int = {
    coqString match {
      case None => 0
      case Some(x) => x.length
    }
  }

  private var specOff : Int = 0
  def setSpecOff (n : Int) : Unit = specOff = n
  def getSpecOff () : Int = specOff

  private var specLength : Int = 0
  def setSpecLength (n : Int) : Unit = specLength = n
  def getSpecLength () : Int = specLength

  private var specs : List[Specification] = List[Specification]()
  def getSpecs () : List[Specification] = specs
  def addSpec (x : Specification) : Unit = specs ::= x
}

trait CoqClassOutput { //rather "CoqProgramOutput"
  private var program : Option[String] = None
  private var spec : Option[String] = None
  private var classCorrectness : Option[String] = None
  private var specLength : Int = 0
  private var programLength : Int = 0

  def setProgram (s : Option[String]) : Unit = {
    program = s
    s match {
      case None => programLength = 0
      case Some(x) => programLength = x.length
    }
  }
  def appendProgram (s : String) : Unit = {
    program match {
      case None =>
        program = Some(s)
        programLength = s.length
      case Some(x) =>
        program = Some(x + "\n" + s)
        programLength = programLength + s.length + 1
    }
  }
  def getProgram () : Option[String] = program

  def setSpec (s : Option[String]) : Unit = {
    spec = s
    s match {
      case None => specLength = 0
      case Some(x) => specLength = x.length
    }
  }
  def appendSpec (s : String) : Unit = {
    spec match {
      case None =>
        spec = Some(s)
        specLength = s.length
      case Some(x) =>
        spec = Some(x + "\n" + s)
        specLength = specLength + s.length + 1
    }
  }
  def getSpec () : Option[String] = spec

  def setClassCorrectness (s : Option[String]) : Unit = classCorrectness = s
  def getClassCorrectness () : Option[String] = classCorrectness

  def getProofOffset () : Int = programLength + specLength + 1 //note the \n!
  def getSpecOffset () : Int = programLength
}

sealed abstract class SJDefinition () extends CoqClassOutput {
  val id : String
  val body: List[SJBodyDefinition]
}

case class SJClassDefinition (
  modifiers : Set[JModifier],
  override val id : String,
  superclass : String,
  interfaces : List[String],
  override val body : List[SJBodyDefinition],
  outerclass : Option[String],
  fields : HashMap[String, String]
) extends SJDefinition

case class SJInterfaceDefinition (
  modifiers : Set[JModifier],
  override val id : String,
  interfaces : List[String],
  override val body : List[SJBodyDefinition]
) extends SJDefinition

trait SJBodyDefinition extends LengthPositional with CoqPositional with CoqOutput

// common interface for methods and constructors
sealed abstract class SJInvokable() extends SJBodyDefinition {
  val id: String
  val jtype: String
  val parameters: List[SJArgument]
  val body: List[SJStatement]
  val localvariables: HashMap[String, String]
}

case class SJFieldDefinition (
  modifiers : Set[JModifier],
  id : String,
  jtype : String
) extends SJBodyDefinition

case class SJMethodDefinition (
  modifiers : Set[JModifier],
  override val id : String,
  override val jtype : String,
  override val parameters : List[SJArgument],
  override val body : List[SJStatement],
  override val localvariables : HashMap[String, String]
) extends SJInvokable

case class SJConstructorDefinition (
  modifiers : Set[JModifier],
  override val jtype : String,
  override val parameters : List[SJArgument],
  override val body : List[SJStatement],
  override val localvariables : HashMap[String, String]
) extends SJInvokable {
  override val id = "constructor"
}

case class SJBodyBlock (
  modifier : Option[Static],
  body : List[SJStatement]
) extends SJBodyDefinition

sealed case class SJArgument (id : String, jtype : String)


trait SJStatement extends LengthPositional with CoqPositional
case class SJAssert (assertion : SJExpression) extends SJStatement
case class SJWhile (test : SJExpression, body : List[SJStatement]) extends SJStatement
case class SJConditional (test : SJExpression, consequent : List[SJStatement], alternative : List[SJStatement]) extends SJStatement

case class SJAssignment (left : SJVariableAccess, right : SJExpression) extends SJStatement //x := e
case class SJFieldWrite (variable : SJVariableAccess, field : String, value : SJExpression) extends SJStatement //x.f := e
case class SJFieldRead (value : SJVariableAccess, variable : SJVariableAccess, field : String) extends SJStatement //x := y.f
//actually, ret should either be a SJVariableAccess or a SJLiteral
case class SJReturn (ret : SJExpression) extends SJStatement //return e
//actually, receiver should be a SJVariableAccess or a SJLiteral (class methods)
case class SJCall (value : Option[SJVariableAccess], receiver : SJExpression, fun : String, arguments : List[SJExpression]) extends SJStatement //x := y.m(e)
case class SJNewExpression (value : SJVariableAccess, jtype : String, arguments : List[SJExpression]) extends SJStatement //x := alloc C

trait SJExpression extends SJStatement
case class SJBinaryExpression (operation : String, left : SJExpression, right : SJExpression) extends SJExpression //different types: a != b (number), a != b (pointer), a != b (boolean)
case class SJUnaryExpression (operation : String, expr : SJExpression) extends SJExpression //different types
case class SJLiteral (value : String) extends SJExpression //different types: numbers, strings, boolean
case class SJVariableAccess (variable : String) extends SJExpression

