// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import java.io.File

abstract class Position extends Ordered[Position] {
  val line: Int
  val col: Int
  // TODO: this could be an option to avoid the null
  val file: File

  def compare(that: Position): Int = {
    if (this.file == that.file) {
      val ld = this.line - that.line
      if (ld == 0) {
        this.col - that.col
      } else {
        ld
      }
    } else {
      if (this.file eq null) {
        -1
      } else if (that.file eq null) {
        +1
      } else {
        this.file.getPath.compare(that.file.getPath)
      }
    }
  }

  def fullString: String

  def isDefined: Boolean
}

object Position {
  def between(a: Position, b: Position): Position = {
    if (a.file == b.file) {
      if (a.line == b.line && a.col == b.col) {
        a
      } else {
        val (from, to) = if (a < b) (a, b) else (b, a)

        (from, to) match {
          case (p1: OffsetPosition, p2: OffsetPosition) =>
            RangePosition(p1.line, p1.col, p1.point, p2.line, p2.col, p2.point, p1.file)
          case (p1: RangePosition, p2: RangePosition) =>
            RangePosition(p1.lineFrom, p1.colFrom, p1.pointFrom, p2.lineTo, p2.colTo, p2.pointTo, p1.file)
          case (p1: OffsetPosition, p2: RangePosition) =>
            RangePosition(p1.line, p1.col, p1.point, p2.lineTo, p2.colTo, p2.pointTo, p1.file)
          case (p1: RangePosition, p2: OffsetPosition) =>
            RangePosition(p1.lineFrom, p1.colFrom, p1.pointFrom, p2.line, p2.col, p2.point, p1.file)
          case (a,b) =>
            a
        }
      }
    } else {
      a
    }
  }
}

abstract class DefinedPosition extends Position {
  override def toString: String = line + ":" + col
  override def fullString: String = file.getPath + ":" + line + ":" + col
  override def isDefined: Boolean = true

  def focusBegin: OffsetPosition
  def focusEnd: OffsetPosition
}

case class OffsetPosition(line: Int, col: Int, point: Int, file: File) extends DefinedPosition {
  def focusBegin: OffsetPosition = this
  def focusEnd: OffsetPosition = this
}

case class RangePosition(lineFrom: Int, colFrom: Int, pointFrom: Int,
  lineTo: Int, colTo: Int, pointTo: Int, file: File) extends DefinedPosition {

  def focusEnd: OffsetPosition = OffsetPosition(lineTo, colTo, pointTo, file)
  def focusBegin: OffsetPosition = OffsetPosition(lineFrom, colFrom, pointFrom, file)

  val line = lineFrom
  val col  = colFrom
}

case object NoPosition extends Position {
  val line = -1
  val col  = -1
  val file = null

  override def toString: String = "?:?"
  override def fullString: String = "?:?:?"
  override def isDefined: Boolean = false
}


trait Positioned {
  private[this] var _pos: Position = NoPosition

  def setPos(pos: Position): this.type = {
    _pos = pos
    this
  }

  def setPos(that: Positioned): this.type = {
    _pos = that.getPos
    this
  }

  def getPos: Position = {
    _pos
  }
}
