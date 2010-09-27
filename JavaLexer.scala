package dk.itu.sdg.javaparser

import scala.util.parsing.combinator.lexical.StdLexical

import scala.util.matching.Regex
import scala.util.parsing.input._
import scala.util.parsing.syntax._
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.CharArrayReader.EofCh

import java.util.regex.Pattern
import java.lang.Character

class JavaLexer extends StdLexical
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

  def stringLiteral =
    // '"' ~> rep(chrExcept('"', '\n', EofCh)) <~ '"'
    '\"' ~> """[^\"\n]*""".r <~ '\"' // XXX

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
     | charLiteral ^^ CharLit
     | EofCh ^^^ EOF
     | '\"' ~> failure("unclosed quote")
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
