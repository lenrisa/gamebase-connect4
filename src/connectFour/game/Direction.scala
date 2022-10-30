package connectFour.game

import connectFour.logic.Point

sealed abstract class Direction {
  def toPoint : Point
}

case class Right() extends Direction  {
  def toPoint = Point(1,0)
}

case class Left() extends Direction  {
  def toPoint = Point(-1,0)
}

