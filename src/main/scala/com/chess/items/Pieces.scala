package com.chess.items

import com.chess.items.board.ChessBoard.Position

object Pieces {

  sealed trait Piece {

    def isValidMove(from: Position, to: Position): Boolean

    def positions(from: Position, to: Position): Seq[Position]

  }

  case object King extends Piece {

    override def isValidMove(from: Position, to: Position): Boolean =
      Math.abs(to.x - from.x) <= 1 && Math.abs(to.y - from.y) <= 1

    override def positions(from: Position, to: Position): Seq[Position] = Queen.positions(from, to).take(2)
  }

  case object Pawn extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean =
      (to.y == from.y) && (Math.abs(to.x - from.x) <= 2)

    override def positions(from: Position, to: Position): Seq[Position] = {
      (from, to) match {
        case (Position(fromX, fromY), Position(toX, toY)) if fromX > toX && fromY == toY =>
          val xes = (toX to fromX).reverse.toList
          xes.map(x => Position(x, toY))
        case (Position(fromX, fromY), Position(toX, toY)) if fromX < toX && fromY == toY =>
          val xes = (fromX to toX).toList
          xes.map(x => Position(x, toY))
        case _ => Seq.empty
      }
    }
  }

  case object Queen extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean = Rook.isValidMove(from, to) || Bishop.isValidMove(from, to)

    override def positions(from: Position, to: Position): Seq[Position] =
      if (Bishop.positions(from, to).isEmpty) Rook.positions(from, to) else  Bishop.positions(from, to)
  }

  case object Knight extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean =
      (Math.abs(to.x - from.x) == 1 && Math.abs(to.y - from.y) == 2) || (Math.abs(to.x - from.x) == 2 && Math.abs(to.y - from.y) == 1)

    override def positions(from: Position, to: Position): Seq[Position] = Seq.empty
  }

  case object Rook extends Piece {
    override def isValidMove(from: Position, to: Position): Boolean = to.x == from.x || to.y == from.y

    override def positions(from: Position, to: Position): Seq[Position] = {
      (from, to) match {
        case (Position(fromX, fromY), Position(toX, toY)) if fromX > toX && fromY == toY =>
          val xes = (toX to fromX).reverse.toList
          xes.map(x => Position(x, toY))
        case (Position(fromX, fromY), Position(toX, toY)) if fromX == toX && fromY > toY =>
          val xes = (toY to fromY).reverse.toList
          xes.map(y => Position(fromX, y))
        case (Position(fromX, fromY), Position(toX, toY)) if fromX < toX && fromY == toY =>
          val xes = (fromX to toX).toList
          xes.map(x => Position(x, toY))
        case (Position(fromX, fromY), Position(toX, toY)) if fromX == toX && fromY < toY =>
          val xes = (fromY to toY).toList
          xes.map(y => Position(fromX, y))
        case _ => Seq.empty

      }
    }
  }

    case object Bishop extends Piece {

      override def isValidMove(from: Position, to: Position): Boolean = Math.abs(to.x - from.x) == Math.abs(to.y - from.y)

      override def positions(from: Position, to: Position): Seq[Position] = {
        (from, to) match {
          case (Position(fromX, fromY), Position(toX, toY)) if fromX < toX && fromY > toY =>
            val xes = (fromX to toX).toList
            val yes = (toY to fromY).reverse.toList
            xes.zip(yes).map { case (x, y) => Position(x, y) }
          case (Position(fromX, fromY), Position(toX, toY)) if fromX < toX && fromY < toY =>
            val xes = (fromX to toX).reverse.toList
            val yes = (fromY to toY).reverse.toList
            xes.zip(yes).map { case (x, y) => Position(x, y) }.reverse
          case (Position(fromX, fromY), Position(toX, toY)) if fromX > toX && fromY > toY =>
            val xes = (toX to fromX).toList
            val yes = (toY to fromY).toList
            xes.zip(yes).map { case (x, y) => Position(x, y) }.reverse
          case (Position(fromX, fromY), Position(toX, toY)) if fromX > toX && fromY < toY =>
            val xes = (toX to fromX).reverse.toList
            val yes = (fromY to toY).toList
            xes.zip(yes).map { case (x, y) => Position(x, y) }
          case _ => Seq.empty
        }
      }
    }
}
