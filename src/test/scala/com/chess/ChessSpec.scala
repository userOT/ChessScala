package com.chess

import com.chess.game.ChessGame
import com.chess.reader.MovesReader
import org.scalatest.FlatSpec

class ChessSpec extends FlatSpec {

  "start" should "create new game with 2 players and default board" in {
    val checkMateMoves = MovesReader.read("src/resources/checkmate.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }
}

