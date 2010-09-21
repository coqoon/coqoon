// package examples.parsing.lambda

/**
 * Parser for an untyped lambda calculus: abstract syntax tree
 *
 * @author Miles Sabin (adapted slightly by Adriaan Moors)
 */
trait JavaTerms
{
  this: JavaParser =>

  trait Term

  // case class PACKAGE(a: Option[ANNOTS], id: QualId) extends Term

  // opt(opt(annotations) <~ "package" ~> qualifiedId <~ ";") ^^ PACKAGE

  case class Lit (value : Any) extends Term
  case class Modifier (word : Any) extends Term
  case class JClass (id : Term, jtype : Option[Any], superclass : Option[Any], interfaces : Option[Any], body : List[Any]) extends Term
  case class JInterface (id : Term, jtype : Option[Any], interfaces : Option[Any], body : List[Any]) extends Term
  case class Throws (exceptions : List[QualId]) extends Term

  case class MethodDeclarator (parameters : Option[Any], throws : List[Throws], body : Any) extends Term

  case class MethodDeclaration (id : Term, jtype : Any, parameters : Option[Any], throws : List[Throws], body : Any) extends Term
  case class FieldDeclaration (id : Term, jtype : Any, rest : Any) extends Term

  case class FormalVariable (modifiers : Any, jtype : Any, id : Term) extends Term

  // expressions
  trait AnyExpr extends Term
  case class Expr (e : Any) extends AnyExpr { override def toString = "EXPR" }
  case class ParExpr (e : AnyExpr) extends AnyExpr
  case class PrimaryExpr (e : Any) extends AnyExpr {
    override def toString = {
      e match {
        case Lit(x) => x.toString
        case _ => e.toString
      }
    }
  }

  case class NewExpr (e : Any) extends AnyExpr

  // statements
  case class Block (xs : List[BlockStmt]) extends Term
  case class BlockStmt (x : Any) extends Term
  case class Stmt (x : Any) extends Term

  // types
  trait AnyType extends Term
  // case class TYPE (x : List[(Id, TYPEARGS)], braces : Int) extends AnyType
  case class Primitive (t : Any) extends AnyType
  case class ArrayType (t : Any, braces : Int) extends AnyType

  // basic literals straight from tokens
  case class Str (x : String) extends Term { override def toString = "\"" + x + "\"" }
  case class Op (x : String) extends Term { override def toString = "'" + x + "'" }
  case class Id (x : String) extends Term { override def toString = x }
  case class Num (x : String) extends Term { override def toString = x }
  case class Key (x : String) extends Term { override def toString = "'" + x + "'" }

  case class QualId (xs : List[Any]) extends Term {
    val id = xs.reduceLeft(_ + "." + _)
    override def toString = id.toString
  }
  // case class expression (x : Term) extends Term { override def toString = x }

  case class Import (static : Boolean, id : QualId, wildcard : Boolean) extends Term

  case class Program (terms : List[Any]) extends Term { override def toString = "PROGRAM: " + terms.toString }

  case class Name (name : String) extends Term {
    override def toString = name
  }
}
