package com.chess.reader

import scala.io.Source

object MovesReader {

  def read(movesPath: String): Seq[String] = Source.fromFile(movesPath).getLines.toList
}
