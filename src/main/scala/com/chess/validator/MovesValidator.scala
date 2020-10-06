package com.chess.validator

import com.chess.items.Pieces.{Pawn, Piece}
import com.chess.items.Player
import com.chess.items.board.ChessBoard.{ChessCells, Position}

object MovesValidator {

  def validateExistingPiecesOnMoveWay(from: Position, to: Position, piece: Piece, player: Player)(cell: ChessCells): Unit = {
    val moves = piece.positions(from, to)
    val cells = moves.map(r => cell(r.x)(r.y))

    if (cells.isEmpty) {
      println("***Knight is skipping the pieces. No way validation applied*** \n")
    }
    else {
      cells.tail.collectFirst {
        case cellItem if cellItem.player.contains(player) => cellItem
        case cellItem if (piece == Pawn) && (cellItem.player.nonEmpty && !cellItem.player.contains(player)) =>
          throw new RuntimeException(s"Pawn can't move forward, if the is another piece on way")
      } match {
        case Some(s) => throw new RuntimeException(s"Piece ${s.piece.get} is on the way of move to $to for player $player")
        case None => ()
      }
    }
  }

  def validatePieceMove(piece: Piece, from: Position, to: Position): Unit =
    if (piece.isValidMove(from, to)) println("Move is valid \n")  else throw new RuntimeException(s"Piece $piece can't go to cell $to from $from")


}
