package com.chess.game

import com.chess.items.ChessBoard

case class ChessGame(private val moves: Seq[String]) {

  def play(): Unit  = {
    val board = ChessBoard()
    board.fillWithDefaultPiecePositions().print()
    board.makePlayersMoves(moves)
  }
}
