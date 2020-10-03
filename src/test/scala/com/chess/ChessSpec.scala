package com.chess

import com.chess.game.ChessGame
import com.chess.reader.MovesReader
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ChessSpec extends FlatSpec {

  "start" should "create new game with 2 players and default board" in {
    val checkMateMoves = MovesReader.read("src/resources/checkmate.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }

  it should "create new game with another moves" in {
    val checkMateMoves = MovesReader.read("src/resources/sample-moves.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }

  it should "create new game for not valid" in {
    val checkMateMoves = MovesReader.read("src/resources/sample-moves-invalid.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy (chessGame.play())
  }
}

