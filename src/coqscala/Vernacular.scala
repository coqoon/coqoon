/*
trait VernacularRegion { }
case class VernacularDeclaration () extends VernacularRegion { }
case class VernacularDefinition () extends VernacularRegion { }
case class VernacularSyntax () extends VernacularRegion { }
case class VernacularModule () extends VernacularRegion { }
case class VernacularSentence (data : List[String]) extends VernacularRegion { }
case class VernacularNamespace (head : String, tail : String) extends VernacularRegion { }

trait SExpression { }
trait Atom extends SExpression
case class SAtom (x : String) extends Atom
case class NAtom (x : Int) extends Atom
case class SList (x : List[SExpression]) extends SExpression

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
//import scala.util.parsing.combinator.RegexParsers

class VernacularLexer extends StdLexical { //with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh

  override def whitespace : Parser[Any] = rep('(' ~ '*' ~ comment)
  override def comment : Parser[Any] = (
    '*' ~ ')' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment
  )
}

trait VernacularParser extends StdTokenParsers with ImplicitConversions {
  val lexical = new VernacularLexer
  type Tokens = VernacularLexer

  lexical.delimiters ++= ".; ;\n;\t;\r".split(";").toList
  import lexical.{Identifier,StringLit,NumericLit}

  val keyword = """Axiom Conjecture Parameter Parameters Variable Variables Hypothesis
                   Hypotheses Definition Example Inductive CoInductive Fixpoint CoFixpoint
                   Program Goal Let Remark Fact Corollary Proposition Lemma Theorem Tactic
                   Ltac Notation Infix Add Record Section Module Require Import Export Open
                   Proof End Qed Admitted Save Defined Print Eval Check Hint""".split("""\s+""").toList
  lexical.reserved ++= keyword

  val operator = List("!", "%", "&", "&&", "(", "()", ")",
                      "*", "+", "++", ",", "-", "->", ".",
                      ".(", "..", "/", "/\\", ":", "::", ":<", "//\\\\",
                      ":=", ":>", ";", "<", "<-", "<->", "<:",
                      "<=", "<>", "=", "=>", "=_D", ">", ">->",
                      ">=", "?", "?=", "@", "[", "\\/", "]",
                      "^", "{", "|", "|-", "||", "}", "~", "\\", "·=·", "'")

  lexical.delimiters ++= operator

  val keywords = List("_", "as", "at", "cofix", "else", "end",
                      "exists", "exists2", "fix", "for", "forall", "fun",
                      "if", "IF", "in", "let", "match", "mod",
                      "Prop", "return", "Set", "then", "Type", "using",
                      "where", "with")
  
  lexical.reserved ++= keywords

  def string = stringLit ^^ { case x => "\"" + x + "\"" }

  def ws = " " | "\n" | "\t" | "\r"

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

  def commandfragment = assumption | definition | assertion | syntax | module | (end <~ ws) ~ ident | proof

  def term : Parser[Any] = ident ~ "." ~ ident | ident | ("(" <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ (opt(ws) ~> ")") | string | numericLit ^^ { case x => x.toInt } | "::" | "_" | "," | "->" | "++" | "match" | "with" | "|" | "=>" | "*" | ">" | "=" | "end" | "-" | ";" | "fun" | (":" <~ rep(ws)) ~ term | "?=" | "%" ~ term | ">=" | "<-" | ("[" <~ opt(ws)) ~ rep1sep(term, rep(ws)) ~ "]" | "/" | "\\" | "!" | "·=·" | "/\\" | "in" | "forall" | "{" ~ rep1sep(term, rep(ws)) ~ "}" | "as" | "@" | ":=" | "'" | "|-" | "()" | "//\\\\"

  def assumption = assumptionStart ~ assRest

  def assRest = rep1(ident) ~ ":" ~ ident //term

  def assumptionStart = (
    "Axiom"
    | "Conjecture"
    | "Parameter"
    | "Parameters"
    | "Variable"
    | "Variables"
    | "Hypothesis"
    | "Hypotheses"
    )

  def definition = (definitionStart <~ ws) ~ rep1sep(ident, rep1(ws)) ~ ((ws ~ ":=" ~ rep(ws)) ~> rep1sep(term, rep(ws))) ^^ {
    case "Definition"~id~t =>
      //Console.println("found definition " + id + " to " + t);
      val s = transform(t)
      Console.println(" " + id(0) + " ++> transformed to " + s)
      assert(id.length == 1)
      if (s.length == 1) {
        val c = transformCode(s(0))
        Console.println("     ++++++>>>> " + c)
        VernacularDefinitions.defs += id(0) -> s(0)
      } else
        if (id(0) == "Spec")
          Console.println("spec definition" + "TM.add (TClass name) (SM.add method (arg, spec))")
        else {
        val p =
        s(0) match {
          case SAtom("Build_Method") => //args, lvar, body, return
            assert(s(1).isInstanceOf[SList])
            val args = s(1).asInstanceOf[SList].x.filterNot(x => x == SAtom("::")).dropRight(1).map("JArgument(" + _ + ")")
            "JMethodDefinition(" + id(0) + ", " + args + " body " + s(3) + "ret " + s(4) //VernacularDefinitions.defs(s(3).x) ++ JReturn(JVariableAccess(s(4) (get last, unescape)))"
          case SAtom("Build_Class") => //super, fields, methods
            "JClassDefinition(" + id(0) + ", empty, " + s(1) + ", collect(" + s(2) + "," + s(3) + "), None)"
          case SAtom("Build_Program") => //class, interfaces
            "Program, well, dunno yet"
          case SAtom("Build_spec") => "arg?, (anonfun => (pre), (post))"
          case x => "dunno: " + x
        }
        Console.println(p)
      }
      new ~(id, t)
    case a => a
  }

  def transform (x : Any) : List[SExpression] = {
    x match {
      case "("~(xs:List[Any])~")" => List(SList(xs.map(transform).flatten))
      case (xs:List[Any]) =>
        if (xs.length == 1)
          transform(xs(0))
        else
          xs.map(transform).flatten
      case (x:String) => List(SAtom(x))
      case (x:Int) => List(NAtom(x))
      case x~"."~y => List(SAtom(x + "." + y))
      case "%"~y => List(SAtom("%" + y))
      case ":"~y => List(SAtom(":" + y))
      case x =>
        Console.println("dunno about (class:" + x.asInstanceOf[AnyRef].getClass + ") " + x)
        List(SList(List[SExpression]()))
    }
  }

  def transformE (c : SExpression) : JExpression = {
    transformCode(c).asInstanceOf[JExpression]
  }

  def transformS (c : JExpression) : String = {
    val v = transformCode(c)
    assert(v.isInstanceOf[JVariableAccess])
    v.asInstanceOf[JVariableAccess].variable
  }

  def transformCode (c : SExpression) : JBodyStatement = {
    c match {
      case SList(xs) => xs(0) match {
        case SAtom("cif") =>
          assert(xs.length == 4)
          JConditional(transformE(xs(1)),
                       transformCode(xs(2)),
                       transformCode(xs(3)))
        case SAtom("egt") =>
          assert(xs.length == 3)
          JBinaryExpression(">", transformE(xs(1)), transformE(xs(2)))
        case SAtom("eminus")
          assert(xs.length == 3)
          JBinaryExpression("-", transformE(xs(1)), transformE(xs(2)))
        case SAtom("etimes")
          assert(xs.length == 3)
          JBinaryExpression("*", transformE(xs(1)), transformE(xs(2)))
        case SAtom("cseq") =>
          assert(xs.length == 3)
          JBlock(transformCode(xs(1)), transformCode(xs(2)))
        case SAtom("ccall") =>
          assert(xs.length == 6)
          //ccall ret var meth arglist class
          //-> var is of type class
          val va = transformS(xs(2))
          val fu = transformS(xs(3))
          assert(xs(4).isInstanceOf[SList])
          val as = xs(4).asInstanceOf[SList].x.filterNot(x => x == SAtom("::")).dropRight(1).map(transformE)
          val re = transformS(xs(1))
          JBlock(List(JAssignment(re, JCall(va, fu, as))))
        case SAtom("cassign") =>
          assert(xs.length == 3)
          JAssignment(transformS(xs(1)), transformE(xs(2)))
        case SAtom("var_expr") =>
          assert(xs.length == 2)
          JVariableAccess(transformS(xs(1)))
        //cread, cwrite
        //calloc
      }
      case NAtom(x) => JLiteral(x.toString)
      case SAtom(x) =>
        if (x(0) == '\"')
          JVariableAccess(x.dropRight(1).drop(1))
  }

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

  def sideeffects = "Print" | "Eval" | "Check"
}

object VernacularDefinitions {
  import scala.collection.mutable.HashMap
  val defs = new HashMap[String, SExpression]()
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

object Main extends Application {
  import java.io.{FileInputStream,InputStreamReader,File}
  import scala.util.parsing.input.StreamReader

  override def main (args : Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    ParseV.parse(StreamReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8")))
  }
}
*/