package dk.itu.coqoon.ui.parsing

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
          Success(if (t.pos == NoLengthPosition) t.setPos(input.offset, rest.offset - input.offset) else t, rest)
        case ns : NoSuccess => ns
      }
    }
}
