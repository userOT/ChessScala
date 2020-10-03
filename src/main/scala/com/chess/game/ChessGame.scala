package com.chess.game

import com.chess.items.{ChessBoard, WhitePlayer}
import com.chess.items.Pieces.Pawn

case class ChessGame(private val moves: Seq[String]) {

  def play(): Unit  = {
    val board = ChessBoard()
    board.fillWithDefaultPiecePositions().print()
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    board.makePlayersMoves(moves).print()
  }
}
