/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.coqparser

import scala.util.parsing.input.Positional

trait VernacularRegion extends Positional with Product {
  val outline = false
  def outlineName = "<no name>"
  
  def getOutline = subRegions.filter(_.outline)  
  
  lazy val subRegions : Stream[VernacularRegion] = subRegions(productIterator.toStream)
  private def subRegions (fields : Stream[Any]) : Stream[VernacularRegion] =
    fields match {
      case (field : VernacularRegion) #:: rest => field #:: subRegions(rest)
      case (field : Traversable[VernacularRegion]) #:: rest => field.toStream ++ subRegions(rest)
      case _ #:: rest => subRegions(rest)
      case Stream.Empty => Stream.Empty
    }
  
  lazy val hasSubRegions : Boolean = subRegions.length > 0
}

trait GallinaSyntax { 
  sealed abstract class Name extends VernacularRegion
  case class StringName (name : String) extends Name
  case class UnderscoreName () extends Name 
  
  sealed abstract class Binder extends VernacularRegion
  case class NameBinder (name : Name) extends Binder
  case class TypedNameBinder (names: List[Name], `type` : Term) extends Binder
  case class ColonEqualBinder (name : Name, `type` : Option[Term], binding : Term) extends Binder
  
  sealed abstract class Arg extends VernacularRegion
  case class SimpleArg (term : Term) extends Arg
  case class NamedArg (name : Name, term : Term) extends Arg
  
  case class MatchItem(term : Term, as : Option[Name], in : Option[Term]) extends VernacularRegion
  case class MatchEquation(patterns : List[Pattern], rhs : Term) extends VernacularRegion
  
  sealed abstract class Pattern extends VernacularRegion
  case class MultPattern (patterns : List[Pattern]) extends Pattern
  case class AsPattern (pattern : Pattern, name : StringName) extends Pattern
  case class ScopePattern (pattern : Pattern, scope : StringName) extends Pattern
  case class IdentPattern (id : QualId, patterns : List[Pattern]) extends Pattern
  case class DontCarePattern () extends Pattern // underscore
  case class NumericPattern (num : Int) extends Pattern
  case class OrPattern (patterns : List[Pattern]) extends Pattern
  
  case class FixBody (
    ident : StringName,
    binders : List[Binder],
    annotation : Option[StringName],
    `type` : Option[Term],
    rhs : Term
  ) extends VernacularRegion {
    override val outline = true
    override def outlineName = ident.name
  }
  
  case class CofixBody (
    ident : StringName,
    binders : List[Binder],
    `type` : Option[Term],
    rhs : Term
  ) extends VernacularRegion {
    override val outline = true
    override def outlineName = ident.name
  }
  
  case class DepRetType (name : Option[Name], returnType : Term) extends VernacularRegion
  
  sealed abstract class Term extends VernacularRegion
  case class Forall (binders : List[Binder], term : Term) extends Term
  case class Fun (binders : List[Binder], body : Term) extends Term
  /* more */
  case class Ascription(term : Term, `type` : Term) extends Term
  case class Arrow (from : Term, to : Term) extends Term
  case class Application (op : Term, args : List[Arg]) extends Term
  /* more */
  case class QualId (path : List[StringName]) extends Term

  case class Fix (body: FixBody) extends Term
  case class FixFor (bodies : List[FixBody], `for` : StringName) extends Term
  
  case class Cofix (body : CofixBody) extends Term
  case class CofixFor (bodies : List[CofixBody], `for` : StringName) extends Term
  
  case class Let (name : StringName, binders : List[Binder], `type` : Option[Term], is : Term, body : Term) extends Term
  case class LetFix (fixBody : FixBody, body : Term) extends Term
  case class LetCofix (cofixBody : CofixBody, body : Term) extends Term
  case class LetDepRet(names : List[Name], depRet : Option[DepRetType], is : Term, body : Term) extends Term
  
  case class IfThenElse (condition : Term, depRetType : Option[DepRetType], `then` : Term, `else` : Term) extends Term
  
  case class NoImplicits (id : QualId, terms : List[Term]) extends Term
  
  sealed abstract class Sort extends Term
  // These are not case objects because they extend Positional
  case class Prop () extends Sort
  case class Set () extends Sort
  case class Type () extends Sort
  
  case class Scope (term : Term, name : String) extends Term
  
  case class Match (items : List[MatchItem], returnType : Option[Term], body : List[MatchEquation]) extends Term
  
  case class Num (value : Int) extends Term
  case class Inferrable () extends Term /* this is _ */
  
}

trait VernacularSyntax extends GallinaSyntax {
  /*The Vernacular*/
  sealed abstract class Sentence extends VernacularRegion {
    override val outline = true
  }
  
  case class AssumptionSentence (keyword : StringName, assumptions : List[Assumption]) extends Sentence {
    override def outlineName = keyword.name
  }
  case class DefinitionSentence (keyword : StringName, ident : StringName, binders : List[Binder], `type` : Option[Term], body : Term) extends Sentence {
    override def outlineName = keyword.name + " " +ident.name
  }
  case class InductiveSentence (keyword : StringName, bodies : List[IndBody]) extends Sentence {
    override def outlineName = keyword.name + " " + bodies.map(_.name.name).mkString(", ")
  }
  case class FixpointSentence (fixpoints : List[FixBody]) extends Sentence {
    override def outlineName = "Fixpoint " + fixpoints.map(_.ident .name).mkString(", ")
  }
  case class CofixpointSentence (cofixpoints : List[CofixBody]) extends Sentence {
    override def outlineName = "Cofixpoint " + cofixpoints.map(_.ident .name).mkString(", ")
  }
  case class AssertionSentence (keyword : StringName, name : StringName, binders : List[Binder], `type` : Term) extends Sentence {
    override def outlineName = keyword.name + " " + name.name
  }
  case class Proof (/* TODO : Represent proof body */) extends Sentence {
    override def outlineName = "Proof"
  }
  
  case class Assumption (names : List[StringName], `type` : Term) extends VernacularRegion
  case class IndBody (name : StringName, binders : List[Binder], `type` : Term, constructors : List[ConstructorDef]) extends VernacularRegion
  case class ConstructorDef (name : StringName, binders : List[Binder], `type` : Option[Term]) extends VernacularRegion
  
  
  /* This representation of modules is a bare minimum - it throws out useful information! */
  case class Module (name : StringName, contents : List[VernacularRegion]) extends VernacularRegion {
    override val outline = true
    override def outlineName = "Module " + name.name
  }
  
  case class VernacularDocument (contents : List[VernacularRegion]) extends VernacularRegion
}

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers, Parsers}
import scala.util.parsing.combinator.token.Tokens
//import scala.util.parsing.combinator.RegexParsers


trait VernacularReserved {
  // Not technically reserved words, but they work as such. Used to start top-level forms.
  val keyword = """Axiom Conjecture Parameter Parameters Variable Variables Hypothesis
                   Hypotheses Definition Example Inductive CoInductive Fixpoint CoFixpoint
                   Program Goal Let Remark Fact Corollary Proposition Lemma Theorem Tactic
                   Ltac Notation Infix Add Record Section Module Require Import Export Open
                   Proof End Qed Admitted Save Defined Print Eval Check Hint""".split("""\s+""").toList

  val operator = List("!", "%", "&", "&&", "(", "()", ")",
                      "*", "+", "++", ",", "-", "->", ".",
                      ".(", "..", "/", "/\\", ":", "::", ":<", "//\\\\",
                      ":=", ":>", ";", "<", "<-", "<->", "<:",
                      "<=", "<>", "=", "=>", "=_D", ">", ">->",
                      ">=", "?", "?=", "@", "[", "\\/", "]",
                      "^", "{", "|", "|-", "||", "}", "~", "\\", "·=·", "'")

  // The reserved words as listed in the reference manual
  val keywords = List("_", "as", "at", "cofix", "else", "end",
                      "exists", "exists2", "fix", "for", "forall", "fun",
                      "if", "IF", "in", "let", "match", "mod",
                      "Prop", "return", "Set", "then", "Type", "using",
                      "where", "with")
}


trait CoqTokens extends Tokens {
  case class Ident (chars : String) extends Token
  case class AccessIdent (chars : String) extends Token
  case class Delim(chars : String) extends Token //the "special tokens" in the Coq reference
  case class Num (chars : String) extends Token
  case class StringLit (chars : String) extends Token
  case class Comment (chars: String) extends Token
  case class Keyword (chars : String) extends Token
}

class VernacularLexer extends Lexical with VernacularReserved with CoqTokens with RegexParsers { //with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh

  type Tokens <: CoqTokens
  override type Elem = Char

  def whitespace = """[\t\n\ ]*""".r
  
  def ident : Parser[Token] = """[\p{L}_][\p{L}_0-9']*""".r ^^ processIdent// TODO: unicode-id-part from Coq reference
  private def processIdent(name : String) : Token =
	if (keywords contains name)
	  Keyword(name)
	else
      Ident(name)
  
  def accessIdent : Parser[Token] = """\.[\p{L}_][\p{L}_0-9']*""".r ^^ AccessIdent
  
  def num : Parser[Token] = """-?\d+""".r ^^ Num
  
  def string : Parser[Token] = '"'~>inString ^^ {chars => StringLit(chars.mkString)}
  private def inString : Parser[List[Char]] =
    ( '"' ~ '"' ~ inString ^^ {case '"'~'"'~rest => '"' :: rest}
    | chrExcept(EofCh, '"') ~ inString ^^ {case ch~rest => ch :: rest}
    | '"' ^^^ Nil
    | failure("String not properly terminated")
    )
    
  def comment : Parser[Token] =
	('('~'*')~>commentContents ^^ {chars => Comment("(*" + chars.mkString)}
  private def commentContents : Parser[List[Char]] =
	( '('~'*'~commentContents~commentContents ^^ {case '('~'*'~nested~rest => '(' :: '*' :: (nested ++ rest)}
	| '*'~')' ^^^ List('*', ')')
	| chrExcept(EofCh)~commentContents ^^ {case char~contents => char :: contents}
	| failure("Comment not finished")
    )
  
  // Based on technique from scala/util/parsing/combinator/lexical/StdLexical.scala
  private lazy val _delim : Parser[Token] = {
	def parseDelim(s : String): Parser[Token] = accept(s.toList) ^^ {x => Delim(x mkString)}
	operator.sortWith(_<_).map(parseDelim).foldRight(failure("no matching special token"): Parser[Token]) {
		(x, y) => y | x
	}
  }
  def delim : Parser[Token] = _delim
  
  def token = ident | accessIdent | num | string | comment | delim
}

object TestLexer extends VernacularLexer with Application {
  def test () : Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new Scanner(input)
      var result = collection.mutable.ListBuffer[Token]()
      while (!scan.atEnd) {
        result += scan.first
        scan = scan.rest
      }
      println(result.toList)
      test()
    }
  }
  test()
}


trait VernacularParser extends TokenParsers  with VernacularSyntax {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer
  
  
  /* Parsers for particular tokens */
  def ident = elem("identifier", _.isInstanceOf[lexical.Ident])
    
  def ident(str : String) = elem("identifier " + str, {
    case lexical.Ident(name) if name == str => true
    case _ => false
  })
  
  def accessIdent = elem("access identifier", _.isInstanceOf[lexical.AccessIdent])

  def keyword(str : String) = elem("keyword " + str, {
    case lexical.Keyword(name) if name == str => true
    case _ => false
  })
  
  def delim = elem("delimiter", _.isInstanceOf[lexical.Delim])
  
  def delim(str : String) = elem("delimiter " + str, {
    case lexical.Delim(name) if name == str => true
    case _ => false
  })
  
  def num : Parser[Int] = elem("number", _.isInstanceOf[lexical.Num]) ^^ {
    case lexical.Num(digits) => digits.toInt
  }
  
  def string = elem("string", _.isInstanceOf[lexical.StringLit])
  
  /* Utility parsers */
  def inParens[T](p : Parser[T]) : Parser[T] = delim("(")~>(p<~delim(")"))
  
  /* Parsers for Gallina */
  
  def term : Parser[Term] =
    positioned(
      ( forall
      | fun
      | fix
      | cofix
      | let
      | ifElse
      | sort
      | num ^^ Num
      | noImplicits
      | qualid
      | patternMatch
      | inParens(term) //not present in spec but seems necessary
      )~opt(
        delim(":")~term
      | delim("->")~term
      | rep1(arg)
      | delim("%")~ident
      ) ^^ {
        case t~None => t
        case t~Some(lexical.Delim(":")~(t2 : Term)) => Ascription(t, t2)
        case t~Some(lexical.Delim("->")~(t2 : Term)) => Arrow(t, t2)
        case t~Some(args : List[Arg]) => Application(t, args)
        case t~Some(lexical.Delim("%")~lexical.Ident(scope)) => Scope(t, scope)
      }
    )
  
  def forall : Parser[Term] = keyword("forall")~rep1(binder)~delim(",")~term ^^ {
    case kwd~binds~comma~body => Forall(binds, body)
  }
  
  def fix : Parser[Term] =
    keyword("fix")~>(fixBody | rep1sep(fixBody, keyword("with"))~keyword("for")~ident) ^^ {
      case (bodies : List[FixBody])~_~(id : lexical.Ident) =>
        FixFor(bodies, StringName(id.chars))
      case (fbody: FixBody) =>
        Fix(fbody)
    }
  
  def fixBody : Parser[FixBody] =
    ident~rep1(binder)~
    opt((delim("{")~keyword("struct"))~>(ident<~delim("}")))~
    opt(delim(":")~>term)~
    (delim(":=")~>term) ^^ {
    case id~binders~annot~ascr~rhs =>
      FixBody(StringName(id.chars), binders, annot.map(name=>StringName(name.chars)), ascr, rhs)
  }
  
  def cofix : Parser[Term] =
    keyword("cofix")~>(cofixBody | rep1sep(cofixBody, keyword("with"))~keyword("for")~ident) ^^ {
      case (bodies : List[CofixBody])~_~(id: lexical.Ident) =>
        CofixFor(bodies, StringName(id.chars))
      case (cfbody: CofixBody) =>
        Cofix(cfbody)
    }
  
  def cofixBody : Parser[CofixBody] =
    ident~rep(binder)~opt(delim(":")~>term)~(delim(":=")~>term) ^^ {
      case id~binders~ascr~rhs =>
        CofixBody(StringName(id.chars), binders, ascr, rhs)
    }
  
  def let : Parser[Term] =
    keyword("let")~>(
      ident~rep(binder)~opt(delim(":")~term)~(delim(":=")~>term)~(keyword("in")~>term)
    | (keyword("fix")~>fixBody)~(keyword("in")~>term)
    | (keyword("cofix")~>cofixBody)~(keyword("in")~>term)
    | inParens(repsep(name, delim(",")))~opt(depRetType)~(delim(":=")~>term)~(keyword("in")~>term)
    ) ^^ {
      case (id : lexical.Ident)~(binders : List[Binder])~(ascr : Option[Term])~(is : Term)~(body : Term) =>
        Let(StringName(id.chars), binders, ascr, is, body)
      case (fb : FixBody)~(body : Term) => LetFix(fb, body)
      case (cfb : CofixBody)~(body : Term) => LetCofix(cfb, body)
      case (names : List[Name])~(depRet : Option[DepRetType])~(is : Term)~(body : Term) =>
        LetDepRet(names, depRet, is, body)
    }
  
  def noImplicits : Parser[Term] =
    delim("@")~>qualid~rep(term) ^^ {
      case name~args => NoImplicits(name, args)
    }
    
  def ifElse : Parser[Term] =
    (keyword("if")~>term~opt(depRetType))~
    (keyword("then")~>term)~
    (keyword("else")~>term) ^^ {
      case cond~depRet~thn~els => IfThenElse(cond, depRet, thn, els) 
    }
  
  def binder : Parser[Binder] =
    positioned(
      name ^^ NameBinder
    | inParens(rep1(name)~delim(":")~term) ^^ {
        case names~colon~typ => TypedNameBinder(names, typ)
      }
    | inParens(name~opt(delim(":")~>term)~delim(":=")~term) ^^ {
        case name~typ~colonEqual~binding => ColonEqualBinder(name, typ, binding)
      }
    )
  
  def fun : Parser[Term] = (keyword("fun")~>rep1(binder))~(delim("=>")~>term) ^^ {
    case binders~body => Fun(binders, body)
  }

  def name : Parser[Name] =
    positioned(
      ident ^^ {case lexical.Ident(id) => StringName(id)}
    | delim("_") ^^^ UnderscoreName()
    )
  
  def qualid : Parser[QualId] = ident~rep(accessIdent) ^^ {
    case id~rest => QualId(StringName(id.chars) :: rest.map({(aID) => StringName(aID.chars)}))
  }
  
  def depRetType : Parser[DepRetType] =
    opt(keyword("as")~>name)~(keyword("return")~>term) ^^ {
      case name~ret => DepRetType(name, ret) 
    }
  
  def patternMatch : Parser[Term] = keyword("match")~>(
    (((rep1sep(matchItem, delim(","))~opt(keyword("return")~>term))<~ keyword("with"))~//missing return type
     opt(delim("|")) ~ repsep(matchEquation, delim("|")))<~keyword("end")
  ) ^^ {
    case items~retn~_~equations => Match(items, retn, equations)
  }
  
  def matchItem : Parser[MatchItem] =
    positioned(term~opt(keyword("as")~>name)~opt(keyword("in")~>term) ^^ {
      case t~as~in => MatchItem(t, as, in)
    })
  
  def matchEquation : Parser[MatchEquation] =
    positioned(
      rep1sep(rep1sep(pattern, delim(",")), delim("|"))~delim("=>")~term ^^ {
        case patterns~arrow~result => MatchEquation(patterns map MultPattern, result)
      }
    )
  
  def pattern : Parser[Pattern] =
    positioned(
      ( qualid~rep(pattern) ^^ {
          case id~Nil => IdentPattern(id, Nil) 
          case id~patterns => IdentPattern(id, patterns)
        }
      | num ^^ NumericPattern
      | keyword("_") ^^^ DontCarePattern()
      | inParens(rep1sep(rep1sep(pattern, delim("|")), delim(","))) ^^ {
          case (pattern :: Nil) :: Nil => pattern
          case patterns :: nil => OrPattern(patterns)
          case orPatterns => MultPattern(orPatterns map OrPattern)
        }
      )~opt(
        keyword("as")~ident
      | delim("%")~>ident
      ) ^^ {
        case pattern~Some(as~(id : lexical.Ident)) => AsPattern(pattern, StringName(id.chars))
        case pattern~Some(scope : lexical.Ident) => ScopePattern(pattern, StringName(scope.chars))
        case pattern~None => pattern
      }
    )
  
  def sort : Parser[Sort] = propSort | setSort | typeSort  
  def propSort : Parser[Prop] = keyword("Prop") ^^^ Prop()
  def setSort : Parser[Set] = keyword("Set") ^^^ Set()
  def typeSort : Parser[Type] = keyword("Type") ^^^ Type()
  
  def arg : Parser[Arg] = (term | inParens(name~delim(":=")~term)) ^^ {
    case t : Term => SimpleArg(t)
    case (name : Name)~colonEquals~(t : Term) => NamedArg(name, t)
  }
  
  /* Parsers for Vernacular */
  def sentence : Parser[Sentence] = positioned(
    assumption
  | definition
  | inductive
  | fixpoint
  | cofixpoint
  | assertion
  | proof
  )
  
  def dot : Parser[lexical.Token] = elem("period", (a : lexical.Token) => a match {
      case lexical.Delim(".") => true
      case _ => false
    })
  
  def assumption : Parser[Sentence] =
    assumptionKeyword~(assum | rep(inParens(assum)))<~dot ^^ {
      case kwd~(assum : Assumption) => AssumptionSentence(kwd, List(assum))
      case kwd~(assums : List[Assumption]) => AssumptionSentence(kwd, assums)
    }
  
  def assumptionKeyword : Parser[StringName] = positioned(
  ( ident("Axiom")
  | ident("Conjecture")
  | ident("Parameter")
  | ident("Parameters")
  | ident("Variable")
  | ident("Variables")
  | ident("Hypothesis")
  | ident("Hypotheses")
  ) ^^ {case id => StringName(id.chars)})
  
  def assum : Parser[Assumption] = rep1(ident)~delim(":")~term ^^ {
    case ids~_~t => Assumption(ids.map(id=>StringName(id.chars)), t)
  }
  
  def definition : Parser[Sentence] =
    defnKeyword~ident~rep(binder)~opt(delim(":")~>term)~(delim(":=")~>term)<~dot ^^ {
      case kwd~id~binders~typ~body =>
        DefinitionSentence(kwd, StringName(id.chars), binders, typ, body)
    }
  
  def defnKeyword : Parser[StringName] =
    (ident("Definition") | ident("Let")) ^^ {id => StringName(id.chars)}
  
  def inductive : Parser[Sentence] =
    inductiveKeyword~rep1sep(indBody, keyword("with"))<~dot ^^ {
      case kwd~bodies => InductiveSentence(kwd, bodies)
    }
  
  def inductiveKeyword = (ident("Inductive") | ident("Coinductive")) ^^ {
      id => StringName(id.chars)
    }
  
  def indBody : Parser[IndBody] =
    ident~rep(binder)~
    (delim(":")~>term)~
    (delim(":=")~>opt(delim("|"))~>rep1sep(constructorDef, delim("|"))) ^^ {
     case id~binders~typ~constructors => IndBody(StringName(id.chars), binders, typ, constructors)
    }
  
  def constructorDef : Parser[ConstructorDef] =
    ident~rep(binder)~opt(delim(":")~>term) ^^ {
     case id~binders~typ => ConstructorDef(StringName(id.chars), binders, typ)
    }
  
  def fixpoint : Parser[FixpointSentence] =
    ident("Fixpoint")~>(rep1sep(positioned(fixBody), ident("with"))<~dot) ^^ FixpointSentence
    
  def cofixpoint : Parser[CofixpointSentence] =
    ident("Cofixpoint")~>(rep1sep(positioned(cofixBody), ident("with"))<~dot) ^^ CofixpointSentence
    
  def assertion : Parser[AssertionSentence] =
    assertionKeyword~ident~rep(binder)~(delim(":")~>term)<~dot ^^ {
      case kwd~id~binders~typ => AssertionSentence(kwd, StringName(id.chars), binders, typ)
    }
  
  def assertionKeyword : Parser[StringName] =
    positioned(
      ( ident("Theorem")
      | ident("Lemma")
      | ident("Remark")
      | ident("Fact")
      | ident("Corollary")
      | ident("Proposition")
      | ident("Definition")
      | ident("Example")
      ) ^^ {kwd => StringName(kwd.chars)}
    )
  
  def proof : Parser[Sentence] = 
    ident("Proof")~dot~proofBody~(ident("Qed") | ident("Defined") | ident("Admitted")) ^^ {case _ => Proof()}
  
  def proofBody : Parser[Any] =
    elem("tactic", {
      case lexical.Ident(name) if name != "Qed" && name != "Defined" && name != "Admitted" => true
      case _ => false
    })~rep(ident | delim)~dot
    
    
  /* Modules - just a start.*/
  def module : Parser[Module] = for {
    moduleName <- ident("Module")~>(ident<~dot);
    body <- top;
    _ <- ident("End")~>(ident(moduleName.chars)<~dot)
  } yield Module(StringName(moduleName.chars), body)
  
  /* Valid Vernacular syntax*/
  def top = rep(sentence | module)
  
  def parseString (input : String) : ParseResult[VernacularDocument] = {
    import scala.util.parsing.input.CharSequenceReader
    phrase(top)(new lexical.Scanner(input)) map VernacularDocument
  }
}

object TestParser extends VernacularParser with Application {
  
  import scala.util.parsing.input.Reader
  def parse (in : Reader[Char]) : Unit = {
    val p = phrase(top)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => Console.println("Parse Success: " + x)
      case _ => Console.println("Parse Fail " + p)
    }
  }

  def test () : Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new lexical.Scanner(input)
      var lexResult = collection.mutable.ListBuffer[lexical.Token]()
      while (!scan.atEnd) {
        lexResult += scan.first
        scan = scan.rest
      }
      print("Lexer: ")
      println(lexResult.toList)
      
      import scala.util.parsing.input.CharSequenceReader
      parse(new CharSequenceReader(input))
      test()
    }
  }
  test()
}


/*
trait VernacularParser extends TokenParsers with ImplicitConversions with VernacularReserved {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer

  import lexical.{Ident, StringLit, Num, Keyword, Delim}

  def ident = elem("identifier", _.isInstanceOf[Ident])
    
  def ident(str : String) = elem("identifier " + str, {
    case Ident(name) if name == str => true
    case _ => false
  })

  def keyword(str : String) = elem("keyword " + str, {
    case Keyword(name) if name == str => true
    case _ => false
  })
  
  def delim(str : String) = elem("delimiter " + str, {
    case Delim(name) if name == str => true
    case _ => false
  })
  
  def string = elem("string", _.isInstanceOf[StringLit])

  def top = rep1(sentence)

  def sentence = toplevelcommand | "\n"

  //def comment = "(*" ~> rep1(nocomment) <~ "*)" ^^ VernacularComment
  //da (.|...) only valid in proof-mode?
  def toplevelcommand = commandfragment ~ (("." <~ ws) | ("." ~ "." ~ ("." <~ ws)))
  //what do we need here?
  // -> Module XXX <: PROGRAM -> CT
  // -> Definition...
  // -> Build_Class/_Program/_Interface/_Method
  // -> Build_spec (translate to pre/post)

  def commandfragment = assumption | definition | assertion | syntax | module | (end <~ ws) ~ ident | proof | sideeffect

  def term : Parser[Any] = ident ~ delim(".") ~ ident | ident | (delim("(") <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ (opt(ws) ~> ")") | string | numericLit ^^ { case x => x.toInt } | "::" | "_" | "," | "->" | "++" | "match" ~ rep(ws) ~ term ~ rep(ws) ~ "with" ~ rep(ws) ~ rep1sep(term, rep(ws)) ~ rep(ws) ~ "end" | "|" | "=>" | "*" | ">" | "=" | "-" | ";" | "fun" | (":" <~ rep(ws)) ~ term | "?=" | "%" ~ term | ">=" | "<-" | ("[" <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ "]" | "/" | "\\" | "!" | "·=·" | "/\\" | "in" | "forall" | "{" ~ rep1sep(term, rep(ws)) ~ "}" | "as" | "@" | ":=" | "'" | "|-" | "()" | "//\\\\" | sort

  def sort = ident("Prop") | ident("Set") | ident("Type")

  def assumption = assumptionStart ~ assRest

  def assRest = rep1(ident) ~ delim(":") ~ ident //term

  def assumptionStart =
    ( ident("Axiom")
    | ident("Conjecture")
    | ident("Parameter")
    | ident("Parameters")
    | ident("Variable")
    | ident("Variables")
    | ident("Hypothesis")
    | ident("Hypotheses")
    )

  def name = ( ident | keyword("_") )

  def binders = rep1(binder)

  def binder = (
    name
    | delim("(") ~ name ~ delim(":") ~ term ~ delim(")")
    | delim("(") ~ name ~ opt(rep(ws) ~ ":" ~ term) ~ rep(ws) ~ delim(":=") ~ rep(ws) ~ term
  )

  def definition = (definitionStart <~ ws) ~ ident ~ rep(ws) ~ opt(binders) ~ opt(opt(ws) ~ ":" ~ rep(ws) ~ term) ~ ws ~ ":=" ~ rep(ws) ~ rep1sep(term, rep(ws))

  def definitionStart = (
    "Definition"
    | "Example"
    | "Inductive"
    | "CoInductive"
    | "Fixpoint"
    | "CoFixpoint"
    | "Program"
    | "Goal"
    | "Let"
  )

  def assertion = (assertionStart <~ ws) ~ (ident <~ rep(ws)) ~ (":" <~ rep(ws)) ~ rep1sep(term, rep(ws)) ~ ("." <~ rep1(ws)) ~ opt(proofStart) ~ proofBody

  def assertionStart = (
    "Remark"
    | "Fact"
    | "Corollary"
    | "Proposition"
    | "Lemma"
    | "Theorem"
  )

  def syntax = (syntaxStart <~ ws) ~ rep1sep(term, rep(ws))

  def syntaxStart = (
    "Tactic"
    | "Ltac"
    | "Notation"
    | "Infix"
    | "Add"
    | "Record"
    | "Hint"
  )

  def module = moduleStart

  def moduleStart = (
    ("Section" <~ ws) ~ ident
    | ("Module" <~ ws) ~ ("Import" <~ ws) ~ (ident <~ ws) ~ (":=" <~ ws) ~ rep1sep(ident, rep(ws))
    | ("Module" <~ ws) ~ (ident <~ ws) ~ ("<:" <~ ws) ~ rep1sep(ident, rep(ws))
    | ("Require" <~ ws) ~ (("Import" | "Export") <~ ws) ~ ident
    | ("Import" <~ ws) ~ ident
    | ("Open" <~ ws) ~ (ident <~ ws) ~ ident
    )

  def proofStart = "Proof" ~ ("." <~ rep1(ws))
  def proofBody = rep(tactics) ~ end
  def proof = proofStart ~ proofBody

  def tactics = rep1sep(term, rep(ws)) ~ (("." | ";") <~ rep1(ws))

  def end = "End" | "Qed" | "Admitted" | "Save" | "Defined"

  def sideeffect = sideeffectstart ~ rep(ws) ~ rep1sep(term, rep(ws))
  def sideeffectstart = "Print" | "Eval" | "Check"
}*/

object VernacularDefinitions {
  import scala.collection.mutable.HashMap
  val defs = new HashMap[String, Object]()
}

object ParseV extends VernacularParser {
  import scala.util.parsing.input.Reader
  def parse (in : Reader[Char]) : Unit = {
    val p = phrase(top)(new lexical.Scanner(in))
    p match {
      case Success(x @ _,_) => //Console.println("success: " + x)
      case _ => Console.println("Fail " + p)
    }
  }
}

/*
object Main extends Application {
  import java.io.{FileInputStream,InputStreamReader,File}
  import scala.util.parsing.input.StreamReader

  override def main (args : Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    ParseV.parse(StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")))
  }
}
*/

