/*
 * developed by Paul Phillips (https://github.com/paulp/scala-lang-combinators)
 *
 * adapted by Hannes Mehnert (https://github.com/hannesm/Kopitiam)
 */

package dk.itu.sdg.javaparser

import scala.util.parsing.combinator.lexical.StdLexical

import scala.util.matching.Regex
import scala.util.parsing.input._
import scala.util.parsing.combinator.syntactical._
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.CharArrayReader.EofCh

import java.util.regex.Pattern
import java.lang.Character

class JavaLexer extends StdLexical with MyScanner
{
  // regex helper fragments
  val exponentPart = """([eE][+-]?[0-9]+)"""
  val floatType = """[dDfF]"""

  // outsourcing - it's the future
  val javaLetter = elem("letter", { Character.isJavaIdentifierStart(_) } )
  val javaLetterOrDigit = elem("letter or digit", { Character.isJavaIdentifierPart(_) } )

  def intLiteral =
    ( """0[xX][0-9a-fA-F]+[lL]?""".r  // hex
     | """0[0-7]+[lL]?""".r  // octal
     | """(0|[1-9][0-9]*[lL]?)""".r  // decimal integer
    )

  def floatLiteral =
    ( ("""([0-9]+\.[0-9]*|\.[0-9]+)""" +
       exponentPart + "?" + floatType + "?").r // decimal cases 1-2
     | ("[0-9]+" + exponentPart + "?" + floatType).r // decimal case 3
     | ("[0-9]+" + exponentPart + floatType + "?").r // decimal case 4
    )

    //XXX: hannes: revert to stringLiteral here!
  //def stringLiteral =
  //  '\"' ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ '\"'

  def stringLiteral = '\"' ~> sLit <~ '\"'

  implicit def sLit(): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      var j = offset
      while (j < source.length && '\"' != source.charAt(j)) {
        if (source.charAt(j) == '\\') j += 1
        j += 1
      }
      Success(source.subSequence(offset, j).toString.replace("\\\"", "\""), in.drop(j - offset))
    }
  }

  //def stringLiteral =
    //'\"' ~> rep(chrExcept('\"', '\n', EofCh)) <~ '\"'
  //  '\"' ~> """(\"|[^"\n])*""".r <~ '\"' // XXX

  def charLiteral =
    """'[^\'\\]'""".r

  /*
   * StdLexical overrides
   */

  override def token: Parser[Token] =
    ( javaLetter ~ rep(javaLetterOrDigit) ^^ mkList ^^ { x => processIdent(x mkString "") }
     | floatLiteral ^^ NumericLit
     | intLiteral ^^ NumericLit
     | stringLiteral ^^ StringLit
// { case (x : List[Char]) => StringLit(x.map(_.toString).reduceLeft(_ + _)) }
     | charLiteral ^^ CharLit
     | EofCh ^^^ EOF
     //| '\"' ~> failure("unclosed quote")
     | delim
     | failure("illegal character")
    )

    override protected def processIdent(name: String) = {
      // Console.println(name)
      if (reserved contains name)
        Keyword(name)
      else
        Identifier(name)
    }

  /*
   * RegexParser code - doesn't work to trait it in
   */
  protected val whiteSpace = """\s+""".r
  def skipWhitespace = whiteSpace.toString.length > 0

  protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset

  /** A parser that matches a regex string */
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(source.subSequence(start, start + matched.end).toString,
                  in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }

  // tokens we need beyond the standard
  case class CharLit(chars: String) extends Token {
    override def toString = "'" + chars + "'"
  }
}

object JavaLexer extends JavaLexer { }

import scala.util.parsing.combinator.lexical.Scanners
trait MyScanner extends Scanners {
class Scanner(in: Reader[Char]) extends Reader[Token] {
  /** Convenience constructor (makes a character reader out of the given string) */
  def this(in: String) = this(new CharArrayReader(in.toCharArray()))
  private val (tok, rest1, rest2) = whitespace(in) match {
    case Success(_, in1) =>
      token(in1) match {
        case Success(tok, in2) => (tok, in1, in2)
        case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
      }
    case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
  }
    private def skip(in: Reader[Char]) = if (in.atEnd) in else in.rest
    override def source: java.lang.CharSequence = in.source
    override def offset: Int = in.offset
    def first = tok
    def rest = new Scanner(rest2)
    def pos = rest1.pos
    def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
    def inp : Reader[Char] = in
}
}
