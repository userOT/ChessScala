package com.chess.items

import com.chess.items.ChessBoard.Position

object Pieces {

  sealed trait Piece {

    def isValidMove(from: Position, to: Position): Boolean

  }

  case object King extends Piece {

    override def isValidMove(from: Position, to: Position): Boolean = {
      Math.abs(to.x - from.x) <= 1 && Math.abs(to.y - from.y) <= 1
    }
  }

  case object Pawn extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean = {
      (to.y == from.y) && ((to.x - from.x) <= 2)
    }
  }

  case object Queen extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean = Rook.isValidMove(from, to) || Bishop.isValidMove(from, to)
  }

  case object Knight extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean =
      (Math.abs(to.x - from.x) == 1 && Math.abs(to.y - from.y) == 2) || (Math.abs(to.x - from.x) == 2 && Math.abs(to.y - from.y) == 1)
  }

  case object Rook extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean =
      to.x == from.x || to.y == from.y
  }

  case object Bishop extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean = Math.abs(to.x - from.x) == Math.abs(to.y - from.y)
  }

}
