// package examples.parsing.lambda

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

import scala.util.parsing.combinator.RegexParsers

// put all the AST generation code in a subclass, where the grammar production accessors are overridden

// running all branches in an OR expression concurrently!
// but sometimes we are not indifferent to ordering, so need new combinator

// abstract out the expressions class and define the operator precedence levels there
// http://jparsec.codehaus.org/api/jfun/parsec/Expressions.html

// http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg02055.html

/**
 * Parser for an untyped lambda calculus
 *
 * @author Miles Sabin (adapted slightly by Adriaan Moors)
 */
trait JavaParser extends StdTokenParsers with ImplicitConversions with JavaTerms with Expression
{
  type Tokens = JavaLexer
  val lexical = new JavaLexer
  import lexical.{Keyword, NumericLit, StringLit, CharLit, Identifier}

  // lists of strings that we group into single parsers
  val keyword = """abstract continue for new switch
                   assert default if package synchronized
                   boolean do goto private this
                   break double implements protected throw
                   byte else import public throws
                   case enum instanceof return transient
                   catch extends int short try
                   char final interface static void
                   class finally long strictfp volatile
                   const float native super while""".split("""\s+""").toList

  def basicType = "byte short char int long float double boolean".split("""\s+""").toList
  def modifierWord = """public protected private static abstract final native
                        synchronized transient volatile strictfp""".split("""\s+""").toList
  def fieldModifierWord = """public protected private static
                             final transient volatile""".split("""\s+""").toList

  val operator = """= > < ! ~ ? : == <= >= != && || ++ -- + - * / & | ^ % << >> >>>
                    += -= *= /= &= |= ^= %= <<= >>= >>>=""".split("""\s+""").toList
  val separator = "( ) { } [ ] ; , .".split("""\s+""").toList  // separators, then operators

  // tell lexer about specials so they come back as Keyword tokens
  lexical.reserved ++= keyword
  lexical.reserved ++= """true false null""".split("""\s+""").toList  // boolean/null literals
  lexical.delimiters ++= operator
  lexical.delimiters ++= separator
  lexical.delimiters ++= List("...")  // not technically a keyword but easier this way

  // a list of strings is implicitly converted into a parser that tests for membership and
  // returns the member - or specify failure message with a Pair
  def key(s: String): Parser[Any] = accept(Keyword(s)) ^^^ Key(s)
  implicit def member(list: List[String]): Parser[Any] = {
    val errorMsg = list.foldLeft("one of [ ")(_ + ", " + _) + " ]"
    list.map(key).reduceRight[Parser[Any]](_|_) | failure(errorMsg)
  }
  // XXX enhancing 1-arg broke 2-arg
  implicit def member(x: Pair[List[String], String]): Parser[Any] =
    x._1.map(key).reduceRight[Parser[Any]](_|_) | failure(x._2)

  /** A parser which matches a character literal */
  def charLit: Parser[String] = elem("char literal", _.isInstanceOf[CharLit]) ^^ (_.chars)

  // standard tokens to transform into terms
  def id: Parser[Term] = ident ^^ Name // ((id: String) => Name(id))
  def JNum: Parser[Term] = numericLit ^^ Num   // (_.toInt) ^^ Lit
  def JString: Parser[Term] = stringLit ^^ Str
  def JChar: Parser[Term] = charLit ^^ Str

  //
  // paulp added productions of uncertain rightness
  //

  // XXX 8.3
  def fieldDeclaration = opt(fieldModifiers) ~ jtype ~ variableDeclarators <~ ";"
  // XXX 8.3.1
  def fieldModifiers = rep(fieldModifier)
  def fieldModifier = fieldModifierWord
  // XXX 8.4.1
  def localVariableModifier =
    ( "final"
     | annotation
    )
  // XXX 14.11
  def switchStatement = "switch" ~> "(" ~> expression <~ ")" ~ switchBlock
  def switchBlock = "{" ~> opt(switchBlockStatementGroups) ~ opt(switchLabels) <~ "}"
  def switchBlockStatementGroups = rep1(switchBlockStatementGroup)
  def switchBlockStatementGroup = switchLabels ~ rep(blockStatement)
  def switchLabels = rep1(switchLabel)
  def switchLabel =
    ( "case" ~> expression <~ ":"
     | "case" ~> id <~ ":"
     | "default" ~ ":"
    )

  //
  // the spec
  //

  // p585
  def qualifiedId = rep1sep(id, ".") ^^ QualId
  def qualifiedIdList = rep1sep(qualifiedId, ",")
  def literal = (JNum | JString | JChar | "true" | "false" | "null") ^^ Lit

  // p586-589 are in the expression trait

  // p590         
  def block = "{" ~> rep(blockStatement) <~ "}" ^^ Block
  def blockStatement =
    ( localVariableDeclaration <~ ";" ^^ AnyStatement
     | classOrInterfaceDeclaration ^^ AnyStatement
     | statement
    )

  def statement: Parser[AnyExpr] =
    ( block
     | "assert" ~ expression ~ opt(":" ~ expression) <~ ";" ^^ AnyStatement
     | "if" ~> parExpression ~ statement ~ opt("else" ~> statement) ^^ flatten3(Conditional)
     | "for" <~ "(" ~> forControl <~ ")" ~> statement ^^ For
     | "while" ~> parExpression ~ statement ^^ While
     | "do" ~> statement ~ "while" ~> parExpression <~ ";" ^^ DoWhile
     | "try" ~ block ~ (catches ~ jfinally | catches | jfinally) ^^ AnyStatement
     | "switch" ~ parExpression <~ "{" ~ switchBlockStatementGroups <~ "}" ^^ AnyStatement
     | "synchronized" ~ parExpression ~ block ^^ AnyStatement
     | "return" ~> opt(expression) <~ ";" ^^ Return
     | "throw" ~> expression <~ ";" ^^ AnyStatement
     | "break" ~> opt(id) <~ ";" ^^ AnyStatement
     | "continue" ~> opt(id) <~ ";" ^^ AnyStatement
     | expression <~ ";"
     | id ~ ":" ~ statement ^^ AnyStatement
     | ";" ^^ AnyStatement
    )

  def catches = rep1(catchClause ~ block)
  def catchClause = "catch" ~> "(" ~> formalParameter <~ ")"
  def jfinally = "finally" ~> block

  // p591
  def forControl =
    ( opt(forInit) <~ ";" ~ opt(expression) <~ ";" ~ opt(expressionList)
     | rep(localVariableModifier) ~ jtype ~ id ~ ":" ~ expression
    )
  def forInit =
    ( localVariableDeclaration
     | expressionList
    )

  //
  // annotations
  //

  def annotations = rep1(annotation)
  def annotation: Parser[Any] = "@" ~> qualifiedId ~ opt("(" ~> opt(id ~ "=") ~ elementValue <~ ")")
  def elementValue: Parser[Any] =
    ( conditionalExpression
     | annotation
     | elementValueArrayInitializer
    )
  def elementValueArrayInitializer = "{" ~> rep1sep(elementValue, ",") ~ opt(",") <~ "}"

  def localVariableDeclaration = rep(localVariableModifier) ~> jtype ~ variableDeclarators
  def modifier =
    ( annotation
     | Pair(modifierWord, "modifier")
    ) ^^ Modifier

  // p592
  def variableDeclarator = id <~ rep(braces) ~> opt("=" ~> variableInitializer)
  def constantDeclarator = id ~ constantDeclaratorRest

  def variableDeclarators = rep1sep(variableDeclarator, ",")
  def variableDeclaratorsRest = rep("," ~> variableDeclarator)
  def constantDeclaratorsRest = rep("," ~> constantDeclarator)
  def variableDeclaratorRest = rep(braces) ~ opt("=" ~ variableInitializer)
  def constantDeclaratorRest = rep(braces) ~ "=" ~ variableInitializer
  def variableDeclaratorId = id <~ rep(braces)  // see 14.20 for "correct" version
  def compilationUnit = opt(opt(annotations) <~ "package" ~> qualifiedId <~ ";") ~>
    rep(importDeclaration) ~ rep(typeDeclaration)
  def importDeclaration = "import" ~> optb("static") ~ qualifiedId ~ optb("." ~> "*") <~ ";" ^^ Import
  def typeDeclaration =
    ( classOrInterfaceDeclaration
     | ";"
    )

  def classOrInterfaceDeclaration = rep(modifier) ~ (classDeclaration | interfaceDeclaration)
  def classDeclaration: Parser[Any] =
    ( normalClassDeclaration
     | enumDeclaration
    )

  // p593
  def normalClassDeclaration = "class" ~> id ~ opt(typeParameters) ~ opt("extends" ~> jtype) ~
    opt("implements" ~> typeList) ~ classBody ^^ flatten5(JClass)
  def typeParameters = "<" ~> rep1sep(typeParameter, ",") <~ ">"
  def typeParameter = id ~ opt("extends" ~> bound)
  def bound = rep1sep(jtype, "&")

  def enumDeclaration = "enum" ~> id ~ opt("implements" ~> typeList) ~ enumBody
  def enumBody = rep(opt(enumConstants) <~ opt(",") ~ opt(enumBodyDeclarations))
  def enumConstants = rep1sep(enumConstant, ",")
  def enumConstant = annotations ~ id ~ opt(arguments) ~ opt(classBody)
  def enumBodyDeclarations = ";" ~ rep(classBodyDeclaration)
  def interfaceDeclaration: Parser[Any] =
    ( normalInterfaceDeclaration
     | annotationTypeDeclaration
    )
  def normalInterfaceDeclaration = "interface" ~> id ~ opt(typeParameters) ~ opt("extends" ~> typeList) ~ interfaceBody ^^ flatten4(JInterface)
  def typeList = rep1sep(jtype, ",")
  def annotationTypeDeclaration = "@interface" ~> id ~ annotationTypeBody
  def annotationTypeBody = rep(opt(annotationTypeElementDeclarations))

  // p594
  def annotationTypeElementDeclarations = rep1(annotationTypeElementDeclaration)
  def annotationTypeElementDeclaration = rep(modifier) ~ annotationTypeElementRest
  def annotationTypeElementRest: Parser[Any] =
    ( jtype ~ id ~ annotationMethodOrConstantRest
     | classDeclaration
     | interfaceDeclaration
     | enumDeclaration
     | annotationTypeDeclaration
    )
  def annotationMethodOrConstantRest =
    ( annotationMethodRest
     | annotationConstantRest
    )
  def annotationMethodRest = "(" ~> ")" ~> opt(defaultValue)
  def annotationConstantRest = variableDeclarators
  def defaultValue = "default" ~> elementValue

  def classBody = "{" ~> rep(classBodyDeclaration) <~ "}"
  def interfaceBody = "{" ~> rep(interfaceBodyDeclaration) <~ "}"
  def classBodyDeclaration =
    ( ";"
     | opt("static") ~ block
     | rep(modifier) ~ memberDecl
    )
  def memberDecl =
    ( genericMethodOrConstructorDecl
     | methodOrFieldDecl
     | "void" ~> id ~ voidMethodDeclaratorRest
     | id ~ constructorDeclaratorRest
     | interfaceDeclaration
     | classDeclaration
    )

  // p595
  def genericMethodOrConstructorDecl = typeParameters ~ genericMethodOrConstructorRest
  def genericMethodOrConstructorRest =
    ( (jtype | "void") ~ id ~ methodDeclaratorRest
     | id ~ constructorDeclaratorRest
    )

  def methodOrFieldDecl = jtype ~ id ~ methodOrFieldRest ^^ {
    case (jtype~id)~MethodDeclarator(parameters, throws, body) => MethodDeclaration(id, jtype, parameters, throws, body)
    case (jtype~id)~initializer => FieldDeclaration(id, jtype, initializer)
  }
  def methodOrFieldRest =
    ( methodDeclaratorRest
     | variableDeclaratorRest
    )
  def interfaceBodyDeclaration =
    ( ";"
     | rep(modifier) ~ interfaceMemberDecl
    )
  def interfaceMemberDecl =
    ( interfaceMethodOrFieldDecl
     | interfaceGenericMethodDecl
     | "void" ~ id ~ voidInterfaceMethodDeclaratorRest
     | interfaceDeclaration
     | classDeclaration
    )
  def interfaceMethodOrFieldDecl = jtype ~ id ~ interfaceMethodOrFieldRest ^^ {
    case (jtype~id)~MethodDeclarator(parameters, throws, body) => MethodDeclaration(id, jtype, parameters, throws, body)
    case (jtype~id)~initializer => FieldDeclaration(id, jtype, initializer)
  }
  def interfaceMethodOrFieldRest =
    ( constantDeclaratorRest <~ ";"
     | interfaceMethodDeclaratorRest
    )
  def methodDeclaratorRest = formalParameterList ~ rep(braces) ~ throwsClause ~ (methodBody | ";") ^^ { case ((arg~br)~throws)~body => MethodDeclarator(arg, throws, body) }
  def voidMethodDeclaratorRest = formalParameterList ~ throwsClause ~ (methodBody | ";")
  def interfaceMethodDeclaratorRest = formalParameterList ~ rep(braces) ~ throwsClause ~ ";" ^^ { case ((arg~br)~throws)~";" => MethodDeclarator(arg, throws, None) }
  def interfaceGenericMethodDecl = typeParameters ~ (jtype | "void") ~ id ~ interfaceMethodDeclaratorRest
  def voidInterfaceMethodDeclaratorRest = formalParameterList ~ throwsClause <~ ";"

  def throwsClause = optl("throws" ~> qualifiedIdList) ^^ { x => x.map(Throws) }

  // p596
  def constructorDeclaratorRest = formalParameterList ~ throwsClause ~ methodBody
  def methodBody = block

  //
  // INTERFACE
  //
  def program = rep(id|JNum|JString|keyword|operator|separator) ^^ Program

  //
  // DEBUGGING
  //

  // option parser that results in 0-or-1 length list
  //def optl[T](p: => Parser[T]): Parser[List[T]] =
  //  p ^^ (x => List(x)) | success(List())

  // option parser that results in boolean
  def optb[T](p: => Parser[T]): Parser[Boolean] =
    p ^^ (x => true) | success(false)


  // always suceed and consume a token
  def eatMe = elem("eatMe", { x => true })
}
