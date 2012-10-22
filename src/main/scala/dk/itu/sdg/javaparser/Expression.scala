/*
 * originally developed by Paul Phillips (https://github.com/paulp/scala-lang-combinators)
 * adapted by Hannes Mehnert (https://github.com/hannesm/Kopitiam)
 */

package dk.itu.sdg.javaparser

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

// expression
// innerNewExpression
// newExpression
// parenthesizedExpression
// primaryExpression


trait Expression extends ImplicitConversions
{
  this: JavaParser =>

  def assignmentOp = "= += -= *= /= &= |= ^= %= <<= >>= >>>=".split("""\s+""").toList

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

  def expression = positioned(assignmentExpression ^^ Expr)
  def expressionList = rep1sep(expression, ",")
  def parExpression = "(" ~> expression <~ ")" ^^ ParExpr
  def arguments = "(" ~> repsep(expression, ",") <~ ")"

  def assignmentExpression: Parser[AnyExpr] = positioned(conditionalExpression ~ opt(assignmentOp ~ assignmentExpression) ^^ {
    case x~None => x
    case (x : AnyExpr)~Some((op : Key)~(y : AnyExpr)) => BinaryExpr(op, x, y)
  })

  def conditionalExpression: Parser[AnyExpr] = positioned(logicalOrExpression ~ opt("?" ~> assignmentExpression ~ ":" ~ conditionalExpression) ^^ {
    case x~None => x
    case (x : AnyExpr)~Some((y : AnyExpr)~":"~(z : AnyExpr)) => Conditional(ParExpr(x), y, Some(z))
  })

  def logicalOrExpression: Parser[AnyExpr] = positioned(logicalAndExpression ~ rep("||" ~ logicalAndExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def logicalAndExpression: Parser[AnyExpr] = positioned(inclusiveOrExpression ~ rep("&&" ~ inclusiveOrExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def inclusiveOrExpression: Parser[AnyExpr] = positioned(exclusiveOrExpression ~ rep("|" ~ exclusiveOrExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def exclusiveOrExpression: Parser[AnyExpr] = positioned(andExpression ~ rep("^" ~ andExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def andExpression: Parser[AnyExpr] = positioned(equalityExpression ~ rep("&" ~ equalityExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })

  def equalityExpression: Parser[AnyExpr] = positioned(instanceOfExpression ~ rep(List("==", "!=") ~ instanceOfExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def instanceOfExpression: Parser[AnyExpr] = positioned(relationalExpression ~ opt("instanceof" ~> jtype) ^^ {
    case x~None => x
    case x~Some(y) => InstanceOf(x, y)
  })
  def relationalExpression: Parser[AnyExpr] = positioned(shiftExpression ~ rep(List("<", ">", "<=", ">=") ~ shiftExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })

  def shiftExpression : Parser[AnyExpr] = positioned(additiveExpression ~ rep(List("<<", ">>", ">>>") ~ additiveExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def additiveExpression : Parser[AnyExpr] = positioned(multiplicativeExpression ~ rep(List("+", "-") ~ multiplicativeExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })
  def multiplicativeExpression : Parser[AnyExpr] = positioned(unaryExpression ~ rep(List("*", "/", "%") ~ unaryExpression) ^^ {
    case x~List() => x
    case (x : AnyExpr)~(y : List[Any]) => listhelper(x, y)
  })

  def listhelper (left : AnyExpr, x : List[Any]) : AnyExpr = {
    x match {
      case Nil => left
      case (op : Key)~(y : AnyExpr) :: rt =>
        val r = BinaryExpr(op, left, y)
        r.setPos(y.pos)
        listhelper(r, rt)
      case (op : String)~(y : AnyExpr) :: rt =>
        val r = BinaryExpr(Key(op), left, y)
        r.setPos(y.pos)
        listhelper(r, rt)
    }
  }

  def unaryExpression : Parser[AnyExpr] =
    positioned(
     ( "++" ~ unaryExpression ^^ UnaryExpr
     | "--" ~ unaryExpression ^^ UnaryExpr
     | "-" ~ unaryExpression ^^ UnaryExpr
     | "+" ~ unaryExpression ^^ UnaryExpr
     | unaryExpressionNotPlusOrMinus
   ) ^^ PrimaryExpr)
  def unaryExpressionNotPlusOrMinus : Parser[AnyExpr] =
    positioned(
    ( "~" ~ unaryExpression ^^ UnaryExpr
     | "!" ~ unaryExpression ^^ UnaryExpr
//     | "(" ~ jtype ~ ")" ~ unaryExpression
     | postfixExpr
   ))

  // always starts with primary expression, then 0 or more of various things
  def postfixExpr : Parser[AnyExpr] = positioned(primaryExpression ~
    rep ( "." ~
          ( opt(genericTypeArgumentList) ~> id ~ opt(arguments) ^^ { //that's either call or field access (depending on arguments or no arguments)
            case x~None => x
            case x~Some(y) => Call(QualId(List(x)), y)
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
      case x~List()~Some(k:Key) => PostExpr(k, x)
      case x => PostFixExpression(x)
    })

  def primaryExpression : Parser[AnyExpr] =
    positioned(
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
   ) ^^ PrimaryExpr)

  def qualifiedIdExpression : Parser[Term] = positioned(qualifiedId ~
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
      case x~Some(y) =>
        Console.println("got " + x + " and some y: " + y + "\nwhat should I do now? (dropping y at the moment)")
        x
    })

  def newExpression : Parser[AnyExpr] = positioned("new" ~>
    ( basicType ~ newArrayConstruction
     | opt(genericTypeArgumentList) ~> qualifiedTypeIdent ~
       (newArrayConstruction | arguments <~ opt(classBody))
    ) ^^ { case ty~(arg : List[AnyExpr]) => NewExpression(ty, arg)
           case x => NewExpr(x)
        })

  def innerNewExpression : Parser[Term] = positioned("new" ~> opt(genericTypeArgumentList) ~> id ~ arguments <~ opt(classBody) ^^ {
    case ty~(arg : List[AnyExpr]) => NewExpression(ty, arg)
    case x => NewExpr(x)
  })

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

