/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.collection.immutable.HashMap
import scala.util.parsing.input.Positional

sealed abstract class SJDefinition () { val id : String ; val body: List[SJBodyDefinition] }
case class SJClassDefinition (modifiers : Set[JModifier], override val id : String, superclass : String, interfaces : List[String], override val body : List[SJBodyDefinition], outerclass : Option[String], fields : HashMap[String, String]) extends SJDefinition
case class SJInterfaceDefinition (modifiers : Set[JModifier], override val id : String, interfaces : List[String], override val body : List[SJBodyDefinition]) extends SJDefinition

trait SJBodyDefinition extends Positional

// common interface for methods and constructors
sealed abstract class SJInvokable() extends SJBodyDefinition {
  val id: String
  val jtype: String
  val parameters: List[SJArgument]
  val body: List[SJStatement]
  val localvariables: HashMap[String, String]
}

//not sure whether we really need this here
case class SJFieldDefinition (modifiers : Set[JModifier], id : String, jtype : String) extends SJBodyDefinition
case class SJMethodDefinition (modifiers : Set[JModifier], override val id : String, override val jtype : String, override val parameters : List[SJArgument], override val body : List[SJStatement], override val localvariables : HashMap[String, String]) extends SJInvokable
case class SJConstructorDefinition (modifiers : Set[JModifier], override val jtype : String, override val parameters : List[SJArgument], override val body : List[SJStatement], override val localvariables : HashMap[String, String]) extends SJInvokable {
  override val id = "constructor"
}
case class SJBodyBlock (modifier : Option[Static], body : List[SJStatement]) extends SJBodyDefinition

sealed case class SJArgument (id : String, jtype : String)

trait SJStatement extends Positional
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

