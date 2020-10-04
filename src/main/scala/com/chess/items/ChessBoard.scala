package com.chess.items

import com.chess.items.ChessBoard.{CellItem, ChessCell, Move, Position, plusName}
import com.chess.items.Colors.{BlackCell, CellColor, WhiteCell}
import com.chess.items.Pieces.{Piece, Rook}

trait ChessBoard {

  def makePlayersMoves(moves: Seq[String]): ChessBoard

  protected def cell: ChessCell

  def print(): Unit

  def fillWithDefaultPiecePositions(): ChessBoard

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
      (piece, positions) <- pl.getDefaultPiecesPositions
      position <- positions
    } yield {
      changeCell(position, piece, pl)
    }).last
  }

  override def createDefaultBoard: ChessBoard = {
    for {
      (x, xIndex) <- (1 to 8).toArray.zipWithIndex
      (y, yIndex) <- (1 to 8).toArray.zipWithIndex
    } yield {
      if ((x + y) % 2 == 0)
        cell(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), WhiteCell, None, Position(xIndex, yIndex), None)
      else
        cell(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), BlackCell, None, Position(xIndex, yIndex), None)
    }
    SimpleChessBoard(cell)
  }

  override def makePlayersMoves(moves: Seq[String]): ChessBoard = {
    val splitMoves = moves.map(mv => mv.splitAt(2)).zipWithIndex.map {
      case ((from, to), index) if index == 0 => Move(WhitePlayer, from, to)
      case ((from, to), index) if index % 2 != 0 => Move(BlackPlayer, from, to)
      case ((from, to), _) => Move(WhitePlayer, from, to)
    }
    splitMoves.map(move => {
      println(s"Player ${move.player} make move from ${move.from} to ${move.to}")
      updateBoardWithMove(move.from, move.to, move.player)
    }).last
  }

  def updateBoardWithMove(from: String, to: String, player: Player): ChessBoard = {
    (findCell(from), findCell(to)) match {
      case (Some(f), Some(_)) if f.piece.isEmpty => throw new UnsupportedOperationException(s"Can't move not existing piece $f")
      case (Some(f), Some(d)) =>
        validateMove(f.piece.get, f.originalPosition, d.originalPosition)
        cell(f.originalPosition.x)(f.originalPosition.y) = f.copy(piece = None, player = None)
        cell(d.originalPosition.x)(d.originalPosition.y) = d.copy(piece = f.piece, player = Some(player))
      case _ => throw new UnsupportedOperationException
    }
    SimpleChessBoard(cell)
  }

   def changeCell(to: String, piece: Piece, player: Player): ChessBoard = {
    val present = for {
      l <- cell
      n <- l
    } yield if (n.classicPosition == to) Some(n) else None
    present.flatten.headOption match {
      case Some(value) =>
        val position = value.originalPosition
        cell(position.x)(position.y) = value.copy(piece = Some(piece), player = Some(player))
        SimpleChessBoard(cell)
      case None => throw new UnsupportedOperationException
    }
  }

  private def checkPath(from: CellItem, to: CellItem, piece: Piece) = {
     if(piece == Rook) {
       val item = cell(from.originalPosition.x)(to.originalPosition.y)


     }
  }

  private def validateMove(piece: Piece, from: Position, to: Position) = {
    if (piece.isValidMove(from, to)) piece else throw new RuntimeException(s"Piece $piece can't go to cell $to from $from")
  }

  private def findCell(classicPosition: String): Option[CellItem] = {
    val present = for {
      l <- cell
      n <- l
      if n.classicPosition == classicPosition
    } yield n
    present.headOption
  }
}

object ChessBoard {

  type ChessCell = Array[Array[CellItem]]

  case class Position(x: Int, y: Int)

  case class Move(player: Player, from: String, to: String)

  def apply(): ChessBoard = SimpleChessBoard(Array.ofDim[CellItem](8, 8)).createDefaultBoard
    .createDefaultBoard

  val DefaultLetters = "abcdefgh"
  val DefaultNumbers = "87654321"

  def plusName(x: Int, y: Int): String = {
    if ((x > 7) || y > 7) null
    else s"${DefaultLetters.charAt(x)}${DefaultNumbers.charAt(y)}"
  }

  case class CellItem(classicPosition: String, color: CellColor, piece: Option[Piece], originalPosition: Position, player: Option[Player]) {

    override def toString: String = s"[$classicPosition -> $color -> $piece -> $player]"

  }

}