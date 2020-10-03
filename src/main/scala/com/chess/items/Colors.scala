package com.chess.items

object Colors {

  sealed trait CellColor extends Product with Serializable

  case object Black extends CellColor

  case object White extends CellColor

}
