/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

sealed abstract class SJDefinition { }
case class SJClassDefinition (id : String, superclass : String, interfaces : List[String], body : List[JStatement], outerclass : Option[String]) extends SJDefinition
case class SJInterfaceDefinition (id : String, interfaces : List[String], body : List[SJStatement]) extends SJDefinition

sealed abstract class SJBodyDefinition { }

case class SJFieldDefinition (id : String, jtype : String) extends SJBodyDefinition
case class SJMethodDefinition (id : String, jtype : String, parameters : List[SJArgument], body : List[SJBodyStatement]) extends SJBodyDefinition

case class SJArgument (id : String, jtype : String)

sealed abstract class SJStatement
case class SJBlock (body : List[SJStatement]) extends SJStatement
case class SJAssert (assertion : SJExpression) extends SJStatement
case class SJAssignment (left : String, right : SJStatement) extends SJStatement
case class SJFieldWrite (variable : SJVariableAccess, field : String, value : SJExpression) extends SJStatement
case class SJReturn (ret : SJExpression) extends SJStatement
case class SJBinding (name : String, jtype : String, init : Option[SJStatement]) extends SJStatement
case class SJWhile (test : SJExpression, body : SJBlock) extends SJStatement
case class SJConditional (test : SJExpression, consequent : SJStatement, alternative : SJStatement) extends SJStatement
case class SJCall (receiver : SJVariableAccess, fun : String, arguments : List[SJExpression]) extends SJStatement
case class SJNewExpression (jtype : String, arguments : List[SJExpression]) extends SJStatement
case class SJFieldAccess (variable : SJVariableAccess, field : String) extends SJStatement

trait SJExpression extends SJStatement
case class SJBinaryExpression (operation : String, left : SJExpression, right : SJExpression) extends SJExpression
case class SJUnaryExpression (operation : String, expr : SJExpression) extends SJExpression
case class SJLiteral (value : String) extends SJExpression
case class SJVariableAccess (variable : String) extends SJExpression


