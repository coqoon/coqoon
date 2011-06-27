/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

import scala.collection.immutable.HashMap

sealed abstract class SJDefinition () { val id : String }
case class SJClassDefinition (modifiers : Set[JModifier], override val id : String, superclass : String, interfaces : List[String], body : List[SJBodyDefinition], outerclass : Option[String], fields : HashMap[String, String]) extends SJDefinition
case class SJInterfaceDefinition (modifiers : Set[JModifier], override val id : String, interfaces : List[String], body : List[SJBodyDefinition]) extends SJDefinition

sealed abstract class SJBodyDefinition () { }
//not sure whether we really need this here
case class SJFieldDefinition (modifiers : Set[JModifier], id : String, jtype : String) extends SJBodyDefinition
case class SJMethodDefinition (modifiers : Set[JModifier], id : String, jtype : String, parameters : List[SJArgument], body : List[SJStatement], localvariables : HashMap[String, String]) extends SJBodyDefinition
case class SJConstructorDefinition (modifiers : Set[JModifier], jtype : String, parameters : List[SJArgument], body : List[SJStatement], localvariables : HashMap[String, String]) extends SJBodyDefinition
case class SJBodyBlock (modifier : Option[Static], body : List[SJStatement]) extends SJBodyDefinition

sealed case class SJArgument (id : String, jtype : String)

sealed abstract class SJStatement
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


