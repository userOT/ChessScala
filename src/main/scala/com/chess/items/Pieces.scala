package com.chess.items

object Pieces {

  sealed trait Piece {

    def validMove: Boolean

  }

  case object King extends Piece {

    override def validMove: Boolean = true
  }

  case object Pawn extends Piece {

    override def validMove: Boolean = true
  }

  case object Queen extends Piece {

    override def validMove: Boolean = true
  }

  case object Knight extends Piece {
    override def validMove: Boolean = true
  }

  case object Rook extends Piece {
    override def validMove: Boolean = true
  }

  case object Bishop extends Piece {
    override def validMove: Boolean = true
  }

}
