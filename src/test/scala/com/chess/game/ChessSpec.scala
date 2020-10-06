package com.chess.game

import com.chess.reader.MovesReader
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ChessSpec extends FlatSpec {

  "start" should "create new game with 2 players and default board with checkmate" in {
    val checkMateMoves = MovesReader.read("src/resources/checkmate.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }

  it should "create new game with another sample moves" in {
    val checkMateMoves = MovesReader.read("src/resources/sample-moves.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }

  it should "throw RuntimeException to invalid bishop move" in {
    val checkMateMoves = MovesReader.read("src/resources/bishop-moves.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy(chessGame.play())
  }

  it should "throw RuntimeException on invalid sample moves" in {
    val checkMateMoves = MovesReader.read("src/resources/sample-moves-invalid.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy(chessGame.play())
  }

  it should "Successfully pass validation for rook move" in {
    val checkMateMoves = MovesReader.read("src/resources/valid-rook-move.txt")
    val chessGame = ChessGame(checkMateMoves)
    chessGame.play()
  }

  it should "throw RuntimeException on invalid rook move" in {
    val checkMateMoves = MovesReader.read("src/resources/invalid-rook-move.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy(chessGame.play())
  }

  it should "throw RuntimeException on pawn invalid move on way" in {
    val checkMateMoves = MovesReader.read("src/resources/pawn-invalid-move.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy(chessGame.play())
  }

  it should "throw RuntimeException on king invalid move on way" in {
    val checkMateMoves = MovesReader.read("src/resources/king-invalid-move.txt")
    val chessGame = ChessGame(checkMateMoves)
    a[RuntimeException] shouldBe thrownBy(chessGame.play())
  }

}

