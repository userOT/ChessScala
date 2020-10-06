package com.chess

import com.chess.game.ChessGame
import com.chess.reader.MovesReader

object Game extends App {

  val checkMateMoves = MovesReader.read("src/resources/checkmate.txt")
  val chessGame = ChessGame(checkMateMoves)
  chessGame.play()

}
