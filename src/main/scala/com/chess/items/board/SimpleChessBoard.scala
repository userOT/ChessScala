package com.chess.items.board

import com.chess.items.Colors.{BlackCell, CellColor, WhiteCell}
import com.chess.items.Pieces.{King, Piece}
import com.chess.items.Player
import com.chess.items.board.ChessBoard._
import com.chess.validator.MovesValidator.{validateExistingPiecesOnMoveWay, validatePieceMove}

case class SimpleChessBoard(cells: ChessCells) extends ChessBoard {

  override def print(): Unit = {
    cells.foreach(rr => println(rr.mkString(", ") + "\n"))
  }

  override def createDefaultBoard: ChessBoard = {
    val defaultRange = (1 to 8).toArray.zipWithIndex
    for {
      (x, xIndex) <- defaultRange
      (y, yIndex) <- defaultRange
    } yield {
      if ((x + y) % 2 == 0) setEmptyCellWithColor(xIndex, yIndex, WhiteCell)
      else setEmptyCellWithColor(xIndex, yIndex, BlackCell)
    }
    SimpleChessBoard(cells)
  }

  override def updateBoardWithMove(from: String, to: String, player: Player): ChessBoard = {
    (findCell(from), findCell(to)) match {
      case (Some(f), Some(_)) if f.piece.isEmpty => throw new UnsupportedOperationException(s"Can't move not existing piece $f")
      case (Some(f), Some(d)) => update(f, d, player)
      case _ => throw new UnsupportedOperationException("Moves cannot be empty")
    }
    SimpleChessBoard(cells)
  }

  override def setEmptyCell(to: String, piece: Piece, player: Player): ChessBoard = {
    val present = for {
      array <- cells
      nestedArray <- array
    } yield if (nestedArray.classicPosition == to) Some(nestedArray) else None
    present.flatten.headOption match {
      case Some(value) =>
        val position = value.originalPosition
        cells(position.x)(position.y) = value.copy(piece = Some(piece), player = Some(player))
        SimpleChessBoard(cells)
      case None => throw new UnsupportedOperationException("Target position not found")
    }
  }

  override def checkKingDefeatMove(move: Move): Unit = {
    val kingCell = findOpponentKingCell(move.player)
    val kingPosition = kingCell.originalPosition
    val lastMovePosition = findCell(move.to).head.originalPosition
    val lastMovePiece = findCell(move.to).head.piece.head
    if (lastMovePiece.isValidMove(lastMovePosition, kingPosition)) {
      println("King attack! Check! ")
    }
  }

  private def setEmptyCellWithColor(xIndex: Int, yIndex: Int, color: CellColor): ChessCells = {
    cells(xIndex)(yIndex) = CellItem(createClassicCellName(yIndex, xIndex), color, None, Position(xIndex, yIndex), None)
    cells
  }

  private def findOpponentKingCell(player: Player): CellItem = {
   for {
     array <- cells
     nestedArray <- array
     if nestedArray.piece.contains(King) && !nestedArray.player.contains(player)
    } yield nestedArray
  }.head

  private def update(from: CellItem, to: CellItem, player: Player): ChessCells = {
    validatePieceMove(from.piece.get, from.originalPosition, to.originalPosition)
    validateExistingPiecesOnMoveWay(from.originalPosition, to.originalPosition, from.piece.get, from.player.get)(cells)
    cells(from.originalPosition.x)(from.originalPosition.y) = from.copy(piece = None, player = None)
    cells(to.originalPosition.x)(to.originalPosition.y) = to.copy(piece = from.piece, player = Some(player))
    cells
  }


  private def findCell(classicPosition: String): Option[CellItem] = {
    val present = for {
      array <- cells
      nestedArray <- array
      if nestedArray.classicPosition == classicPosition
    } yield nestedArray
    present.headOption
  }
}