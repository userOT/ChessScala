package com.chess.items

object Colors {

  sealed trait CellColor extends Product with Serializable

  case object BlackCell extends CellColor

  case object WhiteCell extends CellColor

}
