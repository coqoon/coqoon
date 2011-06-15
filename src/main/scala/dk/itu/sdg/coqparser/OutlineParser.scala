package dk.itu.sdg.coqparser

import dk.itu.sdg.parsing._

import scala.annotation.tailrec

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers, Parsers}
import scala.util.parsing.combinator.token.Tokens

object OutlineVernacular {
  trait OutlineSentence extends VernacularRegion {
    override val outline = true
  }

  case class UnknownSentence (chars : String) extends OutlineSentence {
    override def outlineName = chars.split(":=")(0).replace("""\s+""", " ")
  }

  case class Definition (chars : String) extends OutlineSentence {
    override def outlineName = chars.split(":=")(0).replace("""\s+""", " ")
  }

  case class Assertion (chars : String, name : String) extends OutlineSentence {
    override def outlineName = chars.replace("""\s+""", " ")
  }

  case class Goal (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  case class Import (chars : String) extends OutlineSentence {
    override def outlineName = chars.take(60)
  }

  trait OutlineStructure extends VernacularRegion {
    override val outline = true
    val contents : List[VernacularRegion] = Nil
  }

  case class Module (name : String, override val contents : List[VernacularRegion]) extends OutlineStructure {
    override def toString = "Module " + name + contents.mkString("(", ",", ")")
    override def outlineName = "Module " + name
  }

  case class Section (name : String, override val contents : List[VernacularRegion]) extends OutlineStructure {
    override def toString = "Section " + name + contents.mkString("(", ",", ")")
    override def outlineName = "Section " + name
  }

  case class Document (override val contents : List[VernacularRegion]) extends OutlineStructure
}

object SentenceFinder {
  private val whitespace = Set(' ', '\r', '\n', '\t')
  
  def findCommands(script : String, offset : Int = 0) : Stream[(Int, Int)] = {
    if (offset >= script.length) Stream.Empty
    else if (whitespace contains script(offset)) findCommands(script, offset + 1)
    else {
      (for (end <- getCommand(script, offset))
       yield (offset, end - offset + 1) #:: findCommands(script, end + 1)) getOrElse Stream.Empty 
    }
  }
  
  @tailrec
  private def skipString(script : String, offset : Int) : Option[Int] = {
    if (offset >= script.length) None
    else {
      script(offset) match {
        case '\\' => skipString(script, offset + 2)
        case '"' => Some(offset + 1)
        case _ => skipString(script, offset + 1)
      }
    }
  }
  
  @tailrec
  private def skipComment(script : String, offset : Int, level : Int = 0) : Option[Int] =
    if (offset + 1 >= script.length) None
    else if (script(offset) == '*' && script(offset + 1) == ')') {
      if (level < 1) Some(offset + 1)
      else skipComment(script, offset, level - 1)
    } else if (script(offset) == '(' && script(offset + 1) == '*') {
      skipComment(script, offset + 1, level + 1)
    } else skipComment(script, offset + 1, level + 1)
   
  private def isCommandEnd(script : String, pos : Int) : Boolean =
    pos + 1 < script.length &&
    script(pos) == '.' &&
    (whitespace contains script(pos + 1))
  
  private def getCommand(script : String, ptr : Int) : Option[Int] = {
    if (ptr >= script.length) None // Overshot the end somehow
    else if (ptr == script.length - 1) { // Reached end of proof script
      if (script(ptr) == '.') Some(ptr)
      else None
    }
    else script(ptr) match {
      case '"' =>
        for {
          stringEnd <- skipString(script, ptr + 1)
          cmd <- getCommand(script, stringEnd)
        } yield cmd
      case '(' if script(ptr + 1) == '*' => 
        for {
          commentEnd <- skipComment(script, ptr)
          cmd <- getCommand(script, commentEnd)
        } yield cmd
      case '.' if whitespace contains script(ptr + 1) => Some(ptr)
      case _ => getCommand(script, ptr + 1)
    }
  }
}

trait SentenceParser extends Parsers {
  import OutlineVernacular._
  
  def sentence : Parser[VernacularRegion] = unknownSentence
  
  def unknownSentence : Parser[UnknownSentence] =
    rep(elem("character", {ch : Elem => true})) ^^ {
      case chars => UnknownSentence(chars.mkString)
    }
}

object TestSentences extends Application {
  def read(filename : String) = scala.io.Source.fromFile(filename).mkString
  
  def test() : Unit = {
    Console.print("filename> ")
    val input = Console.readLine
    if (input != "q") {
      val text = read(input)
      println(text)
      for ((pos, len) <- SentenceFinder.findCommands(text)) {
        val sentence = text.substring(pos, pos+len)
        println("Found [" + sentence + "]")
      }
      test()
    } else println("done")
  }
  test()
}
