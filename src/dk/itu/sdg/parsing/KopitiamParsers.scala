package dk.itu.sdg.parsing

import scala.util.parsing.combinator._

trait LengthPosition {
  def hasPosition : Boolean
  
  def offset : Int
  def length : Int
  
  override def toString = "(@" + offset + ", len " + length + ")"
  
  def < (that: LengthPosition) =
    if (this.offset == that.offset)
      this.length < that.length
    else
      this.offset < that.offset
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
}

trait LengthPositionParsers extends Parsers {
  def lengthPositioned[T <: LengthPositional] (p : Parser[T]) : Parser[T] =
    Parser { input =>
      p(input) match {
        case Success(t, rest) =>
          Success(if (t.pos == NoLengthPosition) t.setPos(input.offset, rest.offset - input.offset) else t, rest)
        case ns : NoSuccess => ns
      }
    }
}