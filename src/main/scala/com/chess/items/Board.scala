package com.chess.items

import com.chess.items.Board.ChessCell
import com.chess.items.Colors.CellColor
import com.chess.items.Pieces.Piece

trait Board {

  def cell: ChessCell

  def print(): Unit

  def fill(): Board

  def changeCell(to: String, piece: Piece): Board

}

case class SimpleBoard(cell: ChessCell) extends Board {

  override def print(): Unit = {
    cell.foreach(rr => println(rr.mkString(", ") + "\n"))
  }

  override def fill(): Board = {
    val players = Players.players
    (for {
      pl <- players
      (piece, positions) <- pl.getDefaultPiecesPositions()
      position <- positions
    } yield {
      changeCell(position, piece)
    }).last
  }

  override def changeCell(to: String, piece: Piece): Board = {
    val present = for {
      l <- cell
      n <- l
    } yield if (n.position == to) Some(n) else None
    present.flatten.headOption match {
      case Some(value) =>
        val position = value.originalPosition
        cell(position.x)(position.y) = value.copy(value = Some(piece))
        SimpleBoard(cell)
      case None => throw new UnsupportedOperationException
    }
  }
}

object Board {

  type ChessCell = Array[Array[CellItem]]

  case class Position(x: Int, y: Int)

  val DefaultLetters = "abcdefgh"
  val DefaultNumbers = "87654321"

  case class CellItem(position: String, color: CellColor, value: Option[Piece], originalPosition: Position) {

    override def toString: String = s"[$position -> $color -> $value]"

  }

}