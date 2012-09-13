package dk.itu.sdg.javaparser

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import dk.itu.sdg.parsing.LengthPositionParsers

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
 *
 * adapted by Paul Phillips (https://github.com/paulp/scala-lang-combinators)
 *
 * adapted by Hannes Mehnert (https://github.com/hannesm/Kopitiam)
 *
 */

trait JavaParser extends StdTokenParsers with ImplicitConversions with JavaTerms with Expression with LengthPositionParsers
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

  //some hack I need for integration of specifications - hannes
  def anything () : Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val pos = in.offset
      val src = in.source
      var myin = in
      while (! (myin.first.chars == "%" && myin.rest.first.chars == ">"))
     	myin = myin.drop(1)
      val anyt = src.subSequence(pos, src.toString.indexOf("%>", pos)).toString
      //Console.println("finished with while! " + anyt)
      Success(anyt, myin)
    }
  }

  def specStmt : Parser[SpecStmt] = ("<" ~ "%") ~> anything <~ ("%" ~ ">") ^^ SpecStmt


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
    ( localVariableDeclaration <~ ";" ^^ LocalVar
     | classOrInterfaceDeclaration ^^ AnyStatement
     | statement
    )

  def statement: Parser[AnyExpr] =
    lengthPositioned(
     ( specStmt
     | block
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

  def localVariableDeclaration = rep(localVariableModifier) ~ jtype ~ variableDeclarators
  def modifier =
    ( annotation
     | Pair(modifierWord, "modifier")
    ) ^^ Modifier

  // p592
  def variableDeclarator = id ~ rep(braces) ~ opt("=" ~> variableInitializer)
  def constantDeclarator = id ~ constantDeclaratorRest

  def variableDeclarators = rep1sep(variableDeclarator, ",")
  def variableDeclaratorsRest = rep("," ~> variableDeclarator)
  def constantDeclaratorsRest = rep("," ~> constantDeclarator)
  def variableDeclaratorRest = rep(braces) ~ opt("=" ~ variableInitializer)
  def constantDeclaratorRest = rep(braces) ~ "=" ~ variableInitializer
  def variableDeclaratorId = id <~ rep(braces)  // see 14.20 for "correct" version
  def compilationUnit = opt(opt(annotations) <~ "package" ~> qualifiedId <~ ";") ~>
    rep(importDeclaration) ~ rep(typeDeclaration) //that's the main entry!
  def importDeclaration = "import" ~> optb("static") ~ qualifiedId ~ optb("." ~> "*") <~ ";" ^^ Import
  def typeDeclaration =
    ( classOrInterfaceDeclaration
     | ";"
    )

  def classOrInterfaceDeclaration = rep(modifier) ~ (classDeclaration | interfaceDeclaration) ^^ {
    case Nil ~ classOrInterface => classOrInterface
    case modifiers ~ classOrInterface => SomethingWithModifiers(modifiers, classOrInterface)
  }
  def classDeclaration: Parser[Any] =
    ( normalClassDeclaration
     | enumDeclaration
    )

  // p593
  def normalClassDeclaration = lengthPositioned("class" ~> id ~ opt(typeParameters) ~ opt("extends" ~> jtype) ~
    opt("implements" ~> typeList) ~ classBody ^^ flatten5(JClass))
  def typeParameters = "<" ~> rep1sep(typeParameter, ",") <~ ">"
  def typeParameter = id ~ opt("extends" ~> bound)
  def bound = rep1sep(jtype, "&")

  def enumDeclaration = "enum" ~> id ~ opt("implements" ~> typeList) ~ enumBody
  def enumBody = "{" ~> (opt(enumConstants) <~ opt(",")) ~ opt(enumBodyDeclarations) <~ "}"
  def enumConstants = rep1sep(enumConstant, ",")
  def enumConstant = opt(annotations) ~ id ~ opt(arguments) ~ opt(classBody)
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
     | rep(modifier) ~ memberDecl ^^ BodyDeclaration
    )
  def memberDecl =
    ( specStmt
     | genericMethodOrConstructorDecl
     | methodOrFieldDecl
     | lengthPositioned("void" ~> id ~ voidMethodDeclaratorRest ^^ { case id~MethodDeclarator(parameters, throws, body) => MethodDeclaration(id, "void", parameters, throws, body) })
     | lengthPositioned(id ~ constructorDeclaratorRest ^^ { case id~MethodDeclarator(parameters, throws, body) => ConstructorDeclaration(id, parameters, throws, body) })
     | interfaceDeclaration
     | classDeclaration
    )

  // p595
  def genericMethodOrConstructorDecl = typeParameters ~ genericMethodOrConstructorRest
  def genericMethodOrConstructorRest =
    ( (jtype | "void") ~ id ~ methodDeclaratorRest
     | id ~ constructorDeclaratorRest
    )

  def methodOrFieldDecl = jtype ~ rep1sep(id ~ methodOrFieldRest, ",") ^^ {
    case jtype~(xs:List[Any]) =>
      if (xs.length == 1)
        xs(0) match {
          case id~(x@MethodDeclarator(parameters, throws, body)) =>
            val r = MethodDeclaration(id, jtype, parameters, throws, body)
            r.setPos(x.pos)
            r
          case id~initializer => FieldDeclaration(id, jtype, initializer)
        }
      else
        xs.map(x => x match {
          case id~initializer => FieldDeclaration(id, jtype, initializer)
        })
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
    ( specStmt
     | interfaceMethodOrFieldDecl
     | interfaceGenericMethodDecl
     | lengthPositioned("void" ~> id ~ voidInterfaceMethodDeclaratorRest ^^ {
       case (id~MethodDeclarator(parameters, throws, body)) => MethodDeclaration(id, "void", parameters, throws, body)
     })
     | interfaceDeclaration
     | classDeclaration
    )
  def interfaceMethodOrFieldDecl = lengthPositioned(jtype ~ id ~ interfaceMethodOrFieldRest ^^ {
    case (jtype~id)~MethodDeclarator(parameters, throws, body) => MethodDeclaration(id, jtype, parameters, throws, body)
    case (jtype~id)~initializer => FieldDeclaration(id, jtype, initializer)
  })
  def interfaceMethodOrFieldRest =
    ( constantDeclaratorRest <~ ";"
     | interfaceMethodDeclaratorRest
    )
  def methodDeclaratorRest = lengthPositioned(formalParameterList ~ rep(braces) ~ throwsClause ~ (methodBody | ";") ^^ { case ((arg~br)~throws)~body => MethodDeclarator(arg, throws, body) })
  def voidMethodDeclaratorRest = lengthPositioned(formalParameterList ~ throwsClause ~ (methodBody | ";") ^^ { case (arg~throws)~body => MethodDeclarator(arg, throws, body) })
  def interfaceMethodDeclaratorRest = lengthPositioned(formalParameterList ~ rep(braces) ~ throwsClause ~ ";" ^^ { case ((arg~br)~throws)~";" => MethodDeclarator(arg, throws, None) })
  def interfaceGenericMethodDecl = typeParameters ~ (jtype | "void") ~ id ~ interfaceMethodDeclaratorRest
  def voidInterfaceMethodDeclaratorRest = lengthPositioned(formalParameterList ~ rep(braces) ~ throwsClause <~ ";" ^^ { case (arg~br)~throws => MethodDeclarator(arg, throws, None) })

  def throwsClause = optl("throws" ~> qualifiedIdList) ^^ { x => x.map(Throws) }

  // p596
  def constructorDeclaratorRest = lengthPositioned(formalParameterList ~ throwsClause ~ methodBody ^^ MethodDeclarator)
  def methodBody = block

  // option parser that results in boolean
  def optb[T](p: => Parser[T]): Parser[Boolean] =
    p ^^ (x => true) | success(false)
}
