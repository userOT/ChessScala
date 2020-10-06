package com.chess.game


case class ChessGame(private val moves: Seq[String]) {

  def play(): Unit  = {
    new ChessGameService().startMoves(moves)
  }
}
