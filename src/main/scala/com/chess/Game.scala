package com.chess

import com.chess.items.Board.{CellItem, DefaultLetters, DefaultNumbers, Position}
import com.chess.items.Colors.{Black, White}
import com.chess.items.Pieces.Pawn
import com.chess.items.SimpleBoard

object Game extends App {

  val emptyBoard = Array.ofDim[CellItem](8, 8)

  for {
    (x, xIndex) <- (1 to 8).toArray.zipWithIndex
    (y, yIndex) <- (1 to 8).toArray.zipWithIndex
  } yield {
    if ((x + y) % 2 == 0)
      emptyBoard(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), White, None, Position(xIndex, yIndex))
    else
      emptyBoard(xIndex)(yIndex) = CellItem(plusName(yIndex, xIndex), Black, None, Position(xIndex, yIndex))
  }

  val board = SimpleBoard(emptyBoard)

  board.fill().print()

  def plusName(x: Int, y: Int): String = {
    if ((x > 7) || y > 7) null
    else s"${DefaultLetters.charAt(x)}${DefaultNumbers.charAt(y)}"
  }


}
