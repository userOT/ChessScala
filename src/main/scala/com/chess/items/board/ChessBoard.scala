package com.chess.items.board

import com.chess.items.Colors.CellColor
import com.chess.items.Pieces.Piece
import com.chess.items.Player
import com.chess.items.board.ChessBoard._

trait ChessBoard {

  def updateBoardWithMove(from: String, to: String, player: Player): ChessBoard

  def setEmptyCell(to: String, piece: Piece, player: Player): ChessBoard

  def checkKingDefeatMove(move: Move): Unit

  protected def cells: ChessCells

  def print(): Unit

  def createDefaultBoard: ChessBoard
}

object ChessBoard {

  type ChessCells = Array[Array[CellItem]]

  case class Position(x: Int, y: Int)

  case class Move(player: Player, from: String, to: String)

  val DefaultLetters = "abcdefgh"
  val DefaultNumbers = "87654321"

  def createClassicCellName(x: Int, y: Int): String = {
    if ((x > 7) || y > 7) null
    else s"${DefaultLetters.charAt(x)}${DefaultNumbers.charAt(y)}"
  }

  case class CellItem(classicPosition: String, color: CellColor, piece: Option[Piece], originalPosition: Position, player: Option[Player]) {

    override def toString: String = s"[$classicPosition -> $color -> $piece -> $player]"

  }

}