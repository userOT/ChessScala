package com.chess.items

import com.chess.items.Board.DefaultLetters
import com.chess.items.Pieces.{Bishop, King, Knight, Pawn, Piece, Queen, Rook}

trait Player extends Product with Serializable {

  def move(pieces: Piece, from: String, to: String)

  def getDefaultPiecesPositions(): Map[Piece, Seq[String]]
}

case object BlackPlayer extends Player {

  override def move(pieces: Piece, from: String, to: String): Unit = ???

  override def getDefaultPiecesPositions(): Map[Piece, Seq[String]] =
    Map(
      Pawn -> DefaultLetters.map(_ + 2.toString),
      King -> Seq("e1"),
      Queen -> Seq("d8"),
      Bishop -> Seq("c8", "f8"),
      Rook -> Seq("a8", "h8"),
      Knight -> Seq("b8", "g8")
    )
}


case object WhitePlayer extends Player {

  override def move(pieces: Piece, from: String, to: String): Unit = ???

  override def getDefaultPiecesPositions(): Map[Piece, Seq[String]] =
    Map(
      Pawn -> DefaultLetters.map(_ + 7.toString),
      King -> Seq("e8"),
      Queen -> Seq("d1"),
      Bishop -> Seq("c1", "f1"),
      Rook -> Seq("a1", "h1"),
      Knight -> Seq("b1", "g1")
    )
}

object Players {

  def players = Seq(BlackPlayer, WhitePlayer)

}