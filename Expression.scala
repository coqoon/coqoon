package dk.itu.sdg.javaparser

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

import scala.util.parsing.combinator.RegexParsers

// expression
// innerNewExpression
// newExpression
// parenthesizedExpression
// primaryExpression


trait Expression extends ImplicitConversions
{
  this: JavaParser =>

  def assignmentOp = "= += -= *= /= &= |= ^= %= <<= >>= >>>=".split("""\s+""").toList
  def infixOp = "|| && | ^ & == != < > <= >= << >> >>> + - * / %".split("""\s+""").toList
  def prefixOp = "++ -- ! ~ + -".split("""\s+""").toList
  def postfixOp = "++ --".split("""\s+""").toList

  def precedence = Array(
    Set(assignmentOp),
    Set("?"),
    Set("||"),
    Set("&&"),
    Set("|"),
    Set("^"),
    Set("&"),
    Set("==", "!="),
    Set("instanceof"),
    Set("<", "<=", ">", ">="),
    Set("<<", ">>", ">>>"),
    Set("+", "-"),
    Set("*", "/", "%")
  )
  
  //
  // expressions require extensive refactoring from the spec grammar
  // to capture precedence and  eliminate left-recursion
  // There are more interesting ways, but I want to get it basically working first
  // These productions were mostly adapted from these grammars:
  //
  //   http://www.habelitz.com/index.php?option=com_content&task=view&id=12&Itemid=8
  //   http://www.antlr.org/grammar/1090713067533/java15.g
  //

  // high level look

  def expression = assignmentExpression ^^ Expr
  def expressionList = rep1sep(expression, ",")
  def parExpression = "(" ~> expression <~ ")" ^^ ParExpr
  def arguments = "(" ~> repsep(expression, ",") <~ ")"

  def assignmentExpression: Parser[Any] = conditionalExpression ~ opt(assignmentOp ~ assignmentExpression) ^^ {
    case x~None => x
    case (x : AnyExpr)~Some((op : Key)~(y : AnyExpr)) => BinaryExpr(op, x, y)
  }

  def conditionalExpression: Parser[Any] = logicalOrExpression ~ opt("?" ~> assignmentExpression <~ ":" ~ conditionalExpression) ^^ {
    case x~None => x
    case x~Some(y) => new ~(x, y)
  }

  def logicalOrExpression: Parser[Any] = logicalAndExpression ~ rep("||" ~ logicalAndExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def logicalAndExpression: Parser[Any] = inclusiveOrExpression ~ rep("&&" ~ inclusiveOrExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def inclusiveOrExpression: Parser[Any] = exclusiveOrExpression ~ rep("|" ~ exclusiveOrExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def exclusiveOrExpression: Parser[Any] = andExpression ~ rep("^" ~ andExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def andExpression: Parser[Any] = equalityExpression ~ rep("&" ~ equalityExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }

  def equalityExpression: Parser[Any] = instanceOfExpression ~ rep(List("==", "!=") ~ instanceOfExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def instanceOfExpression: Parser[Any] = relationalExpression ~ opt("instanceof" ~> jtype) ^^ {
    case x~None => x
    case x~Some(y) => new ~(x, y)
  }
  def relationalExpression: Parser[Any] = shiftExpression ~ rep(List("<", ">", "<=", ">=") ~ shiftExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }

  def shiftExpression = additiveExpression ~ rep(List("<<", ">>", ">>>") ~ additiveExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def additiveExpression = multiplicativeExpression ~ rep(List("+", "-") ~ multiplicativeExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }
  def multiplicativeExpression = unaryExpression ~ rep(List("*", "/", "%") ~ unaryExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  }

  def listhelper (left : AnyExpr, x : List[Any]) : AnyExpr = {
    x match {
      case Nil => left
      case (op : Key)~(y : AnyExpr) :: rt => listhelper(BinaryExpr(op, left, y), rt)
      case (op : String)~(y : AnyExpr) :: rt => listhelper(BinaryExpr(Key(op), left, y), rt)
    }
  }

  def unaryExpression: Parser[Any] =
    ( "++" ~ unaryExpression
     | "--" ~ unaryExpression
     | "-" ~ unaryExpression
     | "+" ~ unaryExpression
     | unaryExpressionNotPlusOrMinus
   ) ^^ PrimaryExpr
  def unaryExpressionNotPlusOrMinus: Parser[Any] =
    ( "~" ~ unaryExpression
     | "!" ~ unaryExpression
     | "(" ~ jtype ~ ")" ~ unaryExpression
     | postfixExpression
   )

  // always starts with primary expression, then 0 or more of various things
  def postfixExpression = primaryExpression ~
    rep ( "." ~
          ( opt(genericTypeArgumentList) ~> id ~ opt(arguments) ^^ { //that's either call or field access (depending on arguments or no arguments)
            case x~None => x
            case x~Some(y) => new ~(x, y)
          }
           | "this"
           | "super" ~ arguments
           | "super" ~ "." ~ id ~ opt(arguments)
           | innerNewExpression
          )
         | bracesExpr
        ) ~ opt(List("++", "--")) ^^ {
      case x~List()~None => PostFixExpression(x)
      case x~None => PostFixExpression(x)
      case x => PostFixExpression(x)
    }

  def primaryExpression =
    ( parExpression
     | literal
     | newExpression
     | qualifiedIdExpression
     | genericTypeArgumentList ~
       ( "super" ~ (arguments | "." ~ id ~ arguments)
        | id ~ arguments
        | "this" ~ arguments
       )
     | "this" //~ opt(arguments) Hannes: I doubt that this is correct (the opt(arguments))
     | "super" ~ arguments //^^ Call(QualId(List("super")), _)
     | "super" ~ "." ~ id ~ opt(arguments)
     | basicType ~ rep(braces) ~ "." ~ "class"
     | "void" ~ "." ~ "class"
   ) ^^ PrimaryExpr

  def qualifiedIdExpression = qualifiedId ~
    opt ( bracesList ~ "." ~ "class"
         | arguments //a call!
         | "." ~ ( "class"
                  | genericTypeArgumentList ~ ("super" ~ opt("." ~ id) | id) ~ arguments //call with generics
                  | "this"
                  | "super" ~ arguments
                  | innerNewExpression
                 )
        ) ^^ {
      case x~Some(y : List[AnyExpr]) => Call(x, y)
      case x~None => x
      case x~Some(y) => new ~(x, y)
    }

  def newExpression = "new" ~>
    ( basicType ~ newArrayConstruction
     | opt(genericTypeArgumentList) ~> qualifiedTypeIdent ~
       (newArrayConstruction | arguments <~ opt(classBody))
    ) ^^ { case ty~(arg : List[AnyExpr]) => NewExpression(ty, arg)
           case x => NewExpr(x)
        }

  def innerNewExpression = "new" ~> opt(genericTypeArgumentList) ~> id ~ arguments <~ opt(classBody) ^^ {
    case ty~(arg : List[AnyExpr]) => NewExpression
    case x => NewExpr
  }

  def newArrayConstruction =
    ( bracesList ~ arrayInitializer
     | rep1(bracesExpr) ~ rep(braces)
    )

  def arrayInitializer: Parser[Any] = "{" ~> opt(rep1sep(variableInitializer, ",") <~ opt(",")) <~ "}"
  def variableInitializer =
    ( arrayInitializer
     | expression
    )

  //
  // types
  //

  def jtype: Parser[Any] =
    ( simpleType
     | objectType
    )
  def jtype0: Parser[Any] = objectType0
  def jtype1: Parser[Any] = objectType1
  def jtype2: Parser[Any] = objectType2

  def simpleType = basicType ~ rep(braces) ^^ { case x~List() => Primitive(x) ; case x~y => ArrayType(x, y.length) }
  def objectType = qualifiedTypeIdent <~ rep(braces)
  def objectType0 = qualifiedTypeIdent0 ~ rep(braces)
  def objectType1 = qualifiedTypeIdent1 ~ rep(braces)
  def objectType2 = qualifiedTypeIdent2 ~ rep(braces)
  def qualifiedTypeIdent = rep1sep(typeIdent, ".")
  def qualifiedTypeIdent0 = rep1sep(typeIdent0, ".")
  def qualifiedTypeIdent1 = rep1sep(typeIdent1, ".")
  def qualifiedTypeIdent2 = rep1sep(typeIdent2, ".")
  def typeIdent = id ~ opt(genericTypeArgumentList) ^^ {
    case id~None => id
    case id~so => new ~(id, so)
  }
  def typeIdent0 = id ~ genericTypeArgumentList0
  def typeIdent1 = id ~ genericTypeArgumentList0
  def typeIdent2 = id ~ genericTypeArgumentList00

  def genericTypeArgumentList = genericTypeArgumentList3 | genericTypeArgumentList2| genericTypeArgumentList1

  def genericTypeArgumentList00 = "<" ~ rep1sep(genericTypeArgument0, ",")
  def genericTypeArgumentList0 = "<" ~ rep1sep(genericTypeArgument, ",")
  def genericTypeArgumentList1 = "<" ~ rep1sep(genericTypeArgument, ",") ~ ">"
  def genericTypeArgumentList2 = "<" ~ rep1sep(genericTypeArgument1, ",") ~ ">>"
  def genericTypeArgumentList3 = "<" ~ rep1sep(genericTypeArgument2, ",") ~ ">>>"

  def genericTypeArgument0 =
    ( jtype0
     | "?" ~ opt(genericWildcardBoundType0)
    )
  def genericWildcardBoundType0 = ("extends" | "super") ~ jtype0

  def genericTypeArgument1 =
    ( jtype1
     | "?" ~ opt(genericWildcardBoundType1)
    )
  def genericWildcardBoundType1 = ("extends" | "super") ~ jtype1

  def genericTypeArgument2 =
    ( jtype2
     | "?" ~ opt(genericWildcardBoundType2)
    )
  def genericWildcardBoundType2 = ("extends" | "super") ~ jtype2

  def genericTypeArgument =
    ( jtype
     | "?" ~ opt(genericWildcardBoundType)
    )
  def genericWildcardBoundType = ("extends" | "super") ~ jtype

  def braces = "[" ~ "]"
  def bracesList = rep1(braces)
  def bracesExpr = "[" ~ expression ~ "]"

  def formalParameterList = "(" ~>
    opt ( rep1sep(formalParameter, ",") ~ opt("," ~> formalParameterVarArgDecl) ^^ {
      case x~None => x
      case x~Some(y) => new ~(x, y)
    }
         | formalParameterVarArgDecl
        ) <~ ")"
  def formalParameter = rep(localVariableModifier) ~> jtype ~ variableDeclaratorId ^^ {
    case jtype~id => FormalVariable(None, jtype, id) }
  def formalParameterVarArgDecl = rep(localVariableModifier) ~ jtype ~ "..." ~ variableDeclaratorId

  def binExpr(x: Any): Any = x

  def optl[T](p: => Parser[T]): Parser[List[T]] =
    p ^^ (x => List(x)) | success(List())
}

