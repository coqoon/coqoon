package dk.itu.sdg.javaparser

/**
 * Parser for an untyped lambda calculus: abstract syntax tree
 *
 * @author Miles Sabin (adapted slightly by Adriaan Moors)
 *
 * adapted by Paul Phillips (https://github.com/paulp/scala-lang-combinators)
 *
 * adapted by Hannes Mehnert (https://github.com/hannesm/Kopitiam)
 */

trait JavaTerms
{
  import scala.util.parsing.input.Positional
  //import dk.itu.sdg.parsing.LengthPositional
  trait Term extends Positional

  // case class PACKAGE(a: Option[ANNOTS], id: QualId) extends Term

  // opt(opt(annotations) <~ "package" ~> qualifiedId <~ ";") ^^ PACKAGE

  case class Lit (value : Any) extends Term
  case class Modifier (word : Any) extends Term
  case class JClass (id : Term, jtype : Option[Any], superclass : Option[Any], interfaces : Option[Any], body : List[Any]) extends Term
  case class JInterface (id : Term, jtype : Option[Any], interfaces : Option[Any], body : List[Any]) extends Term
  case class Throws (exceptions : List[QualId]) extends Term

  case class BodyDeclaration (modifiers : List[Modifier], x : Any) extends Term

  case class MethodDeclarator (parameters : Option[Any], throws : List[Throws], body : Any) extends Term

  case class MethodDeclaration (id : Term, jtype : Any, parameters : Option[Any], throws : List[Throws], body : Any) extends Term
  case class FieldDeclaration (id : Term, jtype : Any, rest : Any) extends Term
  case class ConstructorDeclaration (id : Term, parameters : Option[Any], throws : List[Throws], body : Any) extends Term

  case class FormalVariable (modifiers : Any, jtype : Any, id : Term) extends Term

  case class SomethingWithModifiers(modifiers: List[Modifier], something: Any)

  // expressions
  trait AnyExpr extends Term
  case class Expr (e : Any) extends AnyExpr //{ override def toString = "EXPR" }

  case class SpecStmt (e : String) extends AnyExpr

  case class ParExpr (e : AnyExpr) extends AnyExpr
  case class PrimaryExpr (e : Any) extends AnyExpr
  case class PostFixExpression (e : Any) extends AnyExpr
  case class PostExpr(k : Key, x : Any) extends AnyExpr

  case class NewExpr (e : Any) extends AnyExpr
  case class NewExpression (mtype : Any, arguments : List[AnyExpr]) extends AnyExpr

  case class BinaryExpr (op : Key, left : AnyExpr, right : AnyExpr) extends AnyExpr
  case class UnaryExpr (op : String, expr : Any) extends AnyExpr

  // statements
  trait Statement extends AnyExpr
  case class Block (xs : List[AnyExpr]) extends Statement

  case class Call (fun : QualId, arguments : List[AnyExpr]) extends AnyExpr

  case class Assignment (left : QualId, right : AnyExpr) extends Statement
  //might be test ? consequent : alternative at expr location, therefore AnyExpr?
  case class Conditional (test : ParExpr, consequent : AnyExpr, alternative : Option[AnyExpr]) extends Statement
  case class Return (x : Option[AnyExpr]) extends Statement
  trait Loop extends Statement
  case class For (what : Any) extends Loop
  case class While (test : Any, body : AnyExpr) extends Loop
  case class DoWhile (what : Any) extends Loop
  //try/switch/synchronized/throw/break/continue/label
  case class LocalVar (x : Any) extends Statement
  case class AnyStatement (x : Any) extends Statement

  // types
  trait AnyType extends Term
  // case class TYPE (x : List[(Id, TYPEARGS)], braces : Int) extends AnyType
  case class Primitive (t : Any) extends AnyType
  case class ArrayType (t : Any, braces : Int) extends AnyType

  // basic literals straight from tokens
  case class Str (x : String) extends Term
  case class Op (x : String) extends Term
  case class Id (x : String) extends Term
  case class Num (x : String) extends Term
  case class Key (x : String) extends Term

  case class QualId (xs : List[Any]) extends Term
  // case class expression (x : Term) extends Term { override def toString = x }
  case class InstanceOf (x : AnyExpr, y : Any) extends AnyExpr

  case class Import (static : Boolean, id : QualId, wildcard : Boolean) extends Term

  case class Name (name : String) extends Term
}
