package connectFour.game

sealed abstract class CellType {
  def opposite : CellType
  def ghost : CellType
  def isEmpty : Boolean
}
case class PlayerA() extends CellType {
  override def opposite: CellType = PlayerB()
  override def ghost: CellType = PlayerAGhost()
  override def isEmpty: Boolean = false
}

case class PlayerB() extends CellType {
  override def opposite: CellType = PlayerA()
  override def ghost: CellType = PlayerBGhost()
  override def isEmpty: Boolean = false
}

case class PlayerAGhost() extends CellType {
  override def opposite: CellType = PlayerBGhost()
  override def ghost: CellType = PlayerA()
  override def isEmpty: Boolean = true
}

case class PlayerBGhost() extends CellType {
  override def opposite: CellType = PlayerAGhost()
  override def ghost: CellType = PlayerB()
  override def isEmpty: Boolean = true
}

case class Empty() extends CellType {
  override def opposite: CellType = Empty()
  override def ghost: CellType = Empty()
  override def isEmpty: Boolean = true
}

case class TopRow() extends CellType {
  override def opposite: CellType = TopRow()
  override def ghost: CellType = TopRow()
  override def isEmpty: Boolean = true
}

