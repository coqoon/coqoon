package dk.itu.sdg.parsing

import scala.util.parsing.combinator._

sealed trait LengthPosition {
  def hasPosition : Boolean

  def offset : Int
  def length : Int

  override def toString = "(@" + offset + ", len " + length + ")"

  def < (that: LengthPosition) =
    if (this.offset == that.offset)
      this.length < that.length
    else
      this.offset < that.offset

  def line () : Int = 0
  def column () : Int = 0
}

case object NoLengthPosition extends LengthPosition {
  def hasPosition = false
  def offset = 0
  def length = 0
  override def toString = "<no position>"
}

case class RegionPosition (offset : Int, length : Int) extends LengthPosition {
  def hasPosition = true
}

trait LengthPositional {
  var pos : LengthPosition = NoLengthPosition

  def setPos (off : Int, len : Int) : this.type= {
    if (this.pos == NoLengthPosition) {
      this.pos = RegionPosition(off, len)
      this
    }
    else this
  }

  def setPos (p : LengthPosition) : this.type= {
    Console.println("setting position to " + p)
    this.pos = p
    this
  }

  //Enable a hack to kill whitespace that the positioned method brings along
  def advancePosStart() : Boolean = advancePosStart(1)

  def advancePosStart(n : Int) : Boolean =
    this.pos match {
      case NoLengthPosition => false
      case RegionPosition(off, len) if len > 0 => {
        this.pos = RegionPosition(off + 1, len - 1)
        true
      }
      case _ => false
    }
}

trait LengthPositionParsers extends Parsers {
  def lengthPositioned[T <: LengthPositional] (p : Parser[T]) : Parser[T] =
    Parser { input =>
      p(input) match {
        case Success(t, rest) =>
          Console.println("input is: " + input.getClass)
          if (input.isInstanceOf[dk.itu.sdg.javaparser.JavaLexer.Scanner]) {
            val inpu = input.asInstanceOf[dk.itu.sdg.javaparser.JavaLexer.Scanner]
            val inputt = inpu.inp
            Console.println("off is: " + inputt.offset)
            Console.println("inputt is " + inputt.getClass)
            if (inputt.isInstanceOf[dk.itu.sdg.javaparser.CharArrayReaderX])
              Console.println(" mypos: " + inputt.asInstanceOf[dk.itu.sdg.javaparser.CharArrayReaderX].mypos)
          }
          Console.println("assigning length: " + input.offset + " (or: " + input.pos + ") rest: " + rest.offset + " (or: " + rest.pos + ") for t " + t + " and rest: " + rest + " (input: " + input + ")")
          Success(if (t.pos == NoLengthPosition) t.setPos(input.offset, rest.offset - input.offset) else t, rest)
        case ns : NoSuccess => ns
      }
    }
}
