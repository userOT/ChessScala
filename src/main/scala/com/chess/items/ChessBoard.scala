package com.chess.items

import com.chess.items.ChessBoard.{CellItem, ChessCell, Position, plusName}
import com.chess.items.Colors.{Black, CellColor, White}
import com.chess.items.Pieces.Piece

trait ChessBoard {

  def makePlayersMoves(moves: Seq[String]): ChessBoard

  protected def cell: ChessCell

  def print(): Unit

  def fillWithDefaultPiecePositions(): ChessBoard

  def changeCell(to: String, piece: Piece): ChessBoard

  def createDefaultBoard: ChessBoard
}

case class SimpleChessBoard(cell: ChessCell) extends ChessBoard {

  override def print(): Unit = {
    cell.foreach(rr => println(rr.mkString(", ") + "\n"))
  }

  override def fillWithDefaultPiecePositions(): ChessBoard = {
    val players = Players.players
    (for {
      pl <- players
      (piece, positions) <- pl.getDefaultPiecesPositions()
      position <- positions
    } yield {
      changeCell(position, piece)
    }).last
  }

  override def changeCell(to: String, piece: Piece): ChessBoard = {
    val present = for {
      l <- cell
      n <- l
    } yield if (n.position == to) Some(n) else None
    present.flatten.headOption match {
      case Some(value) =>
        val position = value.originalPosition
        cell(position.x)(position.y) = value.copy(value = Some(piece))
        SimpleChessBoard(cell)
      case None => throw new UnsupportedOperationException
    }
  }

  override def createDefaultBoard: ChessBoard = {
    for {
      (x, xIndex) <- (1 to 8).toArray.zipWithIndex
      (y, yIndex) <- (1 to 8).toArray.zipWithIndex
    } yield {
      if ((x + y) % 2 == 0)
        cell(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), White, None, Position(xIndex, yIndex))
      else
        cell(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), Black, None, Position(xIndex, yIndex))
    }
    SimpleChessBoard(cell)
  }

  override def makePlayersMoves(moves: Seq[String]): ChessBoard = {

    this

  }
}

object ChessBoard {

  type ChessCell = Array[Array[CellItem]]

  case class Position(x: Int, y: Int)

  def apply(): ChessBoard = SimpleChessBoard( Array.ofDim[CellItem](8, 8)).createDefaultBoard
    .createDefaultBoard

  val DefaultLetters = "abcdefgh"
  val DefaultNumbers = "87654321"

  def plusName(x: Int, y: Int): String = {
    if ((x > 7) || y > 7) null
    else s"${DefaultLetters.charAt(x)}${DefaultNumbers.charAt(y)}"
  }

  case class CellItem(position: String, color: CellColor, value: Option[Piece], originalPosition: Position) {

    override def toString: String = s"[$position -> $color -> $value]"

  }

}