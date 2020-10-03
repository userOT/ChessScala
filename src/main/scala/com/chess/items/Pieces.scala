package com.chess.items

import com.chess.items.ChessBoard.Position

object Pieces {

  sealed trait Piece {

    def validMove(from: Position, to: Position): Boolean

  }

  case object King extends Piece {

    override def validMove(from: Position, to: Position): Boolean = {
      Math.abs(to.x - from.x) <= 1 && Math.abs(to.y - from.y) <= 1
    }
  }

  case object Pawn extends Piece {
    override def validMove(from: Position, to: Position): Boolean = {
      (to.y == from.y) && (to.x - from.x <= 2)
    }
  }

  case object Queen extends Piece {
    override def validMove(from: Position, to: Position): Boolean = Rook.validMove(from, to) || Bishop.validMove(from, to)
  }

  case object Knight extends Piece {
    override def validMove(from: Position, to: Position): Boolean = true
  }

  case object Rook extends Piece {
    override def validMove(from: Position, to: Position): Boolean =
      to.x == from.x || to.y == from.y
  }

  case object Bishop extends Piece {
    override def validMove(from: Position, to: Position): Boolean = Math.abs(to.x - from.x) == Math.abs(to.y - from.y)
  }

}
