// package examples.parsing.lambda

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


trait Expression
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
	
	def assignmentExpression: Parser[Any] = conditionalExpression ~ opt(Pair(assignmentOp, "assignment") ~ assignmentExpression)
	def conditionalExpression: Parser[Any] = logicalOrExpression ~ opt("?" ~> assignmentExpression <~ ":" ~ conditionalExpression)
				// { case a ~ Some(b ~ c) => TernOp(a, b, c)
				//   case a ~ None => Expr(a)
				// }

	def logicalOrExpression: Parser[Any] = logicalAndExpression ~ rep("||" ~ logicalAndExpression) 
	def logicalAndExpression: Parser[Any] = inclusiveOrExpression ~ rep("&&" ~ inclusiveOrExpression) 
	def inclusiveOrExpression: Parser[Any] = exclusiveOrExpression ~ rep("|" ~ exclusiveOrExpression) 
	def exclusiveOrExpression: Parser[Any] = andExpression ~ rep("^" ~ andExpression) 
	def andExpression: Parser[Any] = equalityExpression ~ rep("&" ~ equalityExpression) 

	def equalityExpression: Parser[Any] = instanceOfExpression ~ rep(List("==", "!=") ~ instanceOfExpression) 
	def instanceOfExpression: Parser[Any] = relationalExpression ~ opt("instanceof" ~> jtype)	
	def relationalExpression: Parser[Any] = shiftExpression ~ rep(List("<", ">", "<=", ">=") ~ shiftExpression)	

	def shiftExpression = additiveExpression ~ rep(List("<<", ">>", ">>>") ~ additiveExpression) 
	def additiveExpression = multiplicativeExpression ~ rep(List("+", "-") ~ multiplicativeExpression) 
	def multiplicativeExpression = unaryExpression ~ rep(List("*", "/", "%") ~ unaryExpression) 
	def unaryExpression: Parser[Any] =
		( "++" ~ unaryExpression
		| "--" ~ unaryExpression
		| "-" ~ unaryExpression
		| "+" ~ unaryExpression
		| unaryExpressionNotPlusOrMinus
		)
	def unaryExpressionNotPlusOrMinus: Parser[Any] =
		( "~" ~ unaryExpression
		| "!" ~ unaryExpression
		| "(" ~ jtype ~ ")" ~ unaryExpression
		| postfixExpression
		)
		
	// always starts with primary expression, then 0 or more of various things
	def postfixExpression = primaryExpression ~
			rep ( "." ~
					( opt(genericTypeArgumentList) ~ id ~ opt(arguments)
					| "this"
					| "super" ~ arguments
					| "super" ~ "." ~ id ~ opt(arguments)
					| innerNewExpression
					)
				| bracesExpr
				) ~ 
			opt (List("++", "--"))
			
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
		| "this" ~ opt(arguments)
		| "super" ~ arguments
		| "super" ~ "." ~ id ~ opt(arguments)
		| basicType ~ rep(braces) ~ "." ~ "class"
		| "void" ~ "." ~ "class"
		) ^^ PrimaryExpr
		
	def qualifiedIdExpression = qualifiedId ~
			opt	( bracesList ~ "." ~ "class"
				| arguments
				| "." ~ ( "class"
						| genericTypeArgumentList ~ ("super" ~ opt("." ~ id) | id) ~ arguments
						| "this"
						| "super" ~ arguments
						| innerNewExpression
						)
				)

	def newExpression = "new" ~>
		( basicType ~ newArrayConstruction
		| opt(genericTypeArgumentList) ~ qualifiedTypeIdent ~
				(newArrayConstruction | arguments ~ opt(classBody))
		) ^^ NewExpr

	def innerNewExpression = "new" ~> opt(genericTypeArgumentList) ~ id ~ arguments ~ opt(classBody) ^^ NewExpr
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
	def objectType = qualifiedTypeIdent ~ rep(braces)
	def objectType0 = qualifiedTypeIdent0 ~ rep(braces)
	def objectType1 = qualifiedTypeIdent1 ~ rep(braces)
	def objectType2 = qualifiedTypeIdent2 ~ rep(braces)
	def qualifiedTypeIdent = rep1sep(typeIdent, ".")
	def qualifiedTypeIdent0 = rep1sep(typeIdent0, ".")
	def qualifiedTypeIdent1 = rep1sep(typeIdent1, ".")
	def qualifiedTypeIdent2 = rep1sep(typeIdent2, ".")
	def typeIdent = id ~ opt(genericTypeArgumentList)
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
			opt ( rep1sep(formalParameter, ",") ~ opt("," ~ formalParameterVarArgDecl)
				| formalParameterVarArgDecl
				) <~ ")"
	def formalParameter = rep(localVariableModifier) ~ jtype ~ variableDeclaratorId
	def formalParameterVarArgDecl = rep(localVariableModifier) ~ jtype ~ "..." ~ variableDeclaratorId
	
	// Transforming tokens into AST	
	/*
	def unExpr(e: Any): Any = e match {
		case BinOp(op, lvalue, rvalue)
				
		case (f @ (a~b) ~ None) => unExpr(f)
		case (f @ (a~b) ~ List()) => unExpr(f)
		case Expr(f) ~ None => f
		case Expr(f) ~ List() => f
		case Primary(f) ~ None => f
		case Primary(f) ~ List() => f
		case _ => e
	}
	*/

/*		
	def unExpr(e: ~[Any, Any]): ~[Any,Any] = e match {
		case f @ (a~b) ~ None => unExpr(f)
		case f @ (a~b) ~ List() => unExpr(f)
		case _ => e
	}
	
	*/
		
	// What if ~ made a list instead?
	/*
	implicit def twiddleToList(x: ~[Any, Any]): List[Any] = x match {
		case b ~ c => (twiddleToList(b) ::: twiddleToList(c))
		case _ => x
	}
	*/

	
	def binExpr(x: Any): Any = x
	

	// Flatten2 is called implicitly
	/*	
	def binExpr(x: Any): Any = x match {
		case hd ~ List() => binExpr(hd)
		case _ => x
	}
	
	def binExpr(lvalue: Any, rest: List[Any]): Any = rest match {
		case List() => lvalue
		case (op ~ rvalue) :: tail =>
			binExpr(new BinOp(op, lvalue, rvalue), tail)
		case x @ _ => { println("???: " + x); lvalue::rest }
	}
	*/
		
		/*
		
		case None => a
		case List() => a
		case Some(c ~ d) => new BinOp(c, a, d)
		case (c ~ d) :: List() => new BinOp(c, a, d)
		case (c ~ d) :: e => binExpr(new BinOp(c, a, d), e)
		case _ => new ~(a, b)
	} */
		
/*	
	def binExpr(e: ~[Any, Any]): Any = e match {
		case a ~ None => a
		case a ~ Some(b ~ c) => BinOp(b, a, c)
		
		case a ~ List() => a
		case a ~ (b :: c) => { 
			if (c isEmpty) binExpr(new ~(a,b))
			else if c match { 
			else { println("LONGMATCH"); val head = new ~(a,b); binExpr(new ~(head, c)) }
		}
			
		case _ => e
	}
	
	def simplifyExpr(e: Expr): Expr = e match {
		case Expr(Expr(f)) => simplifyExpr(Expr(f))
		case _ => e
	}
*/
}

