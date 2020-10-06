package com.chess.game

import com.chess.items.{BlackPlayer, Players, WhitePlayer}
import com.chess.items.board.{ChessBoard, SimpleChessBoard}
import com.chess.items.board.ChessBoard.{CellItem, Move}

class ChessGameService {

  private val ChessBoard: ChessBoard = SimpleChessBoard(Array.ofDim[CellItem](8, 8))

  def startMoves(moves: Seq[String]): ChessBoard = {
    moves match {
      case Nil => throw new RuntimeException("Player moves cannot ne empty")
      case _ =>
        ChessBoard.createDefaultBoard
        fillBoardWithDefaultPiecePositions().print()
        startPlayersMoves(moves)
    }
  }

  private def fillBoardWithDefaultPiecePositions(): ChessBoard =
    (for {
      pl <- Players.players
      (piece, positions) <- pl.getDefaultPiecesPositions
      position <- positions
    } yield ChessBoard.setEmptyCell(position, piece, pl)).last

  private def divideMovesOnPlayersColors(moves: Seq[String]): Seq[Move] = {
    moves.map(mv => mv.splitAt(2)).zipWithIndex.map {
      case ((from, to), index) if index == 0 => Move(WhitePlayer, from, to)
      case ((from, to), index) if index % 2 != 0 => Move(BlackPlayer, from, to)
      case ((from, to), _) => Move(WhitePlayer, from, to)
    }
  }

  private def startPlayersMoves(moves: Seq[String]): ChessBoard = {
    val splitMoves = divideMovesOnPlayersColors(moves)
    splitMoves.map(move => {
      println(s"[ Player ${move.player} make move from ${move.from} to ${move.to} ]\n")
      val p = ChessBoard.updateBoardWithMove(move.from, move.to, move.player)
      ChessBoard.checkKingDefeatMove(move)
      p.print()
      p
    }).last
  }
  
}
