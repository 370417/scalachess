package chess

import scala.language.implicitConversions
import Pos.*
import bitboard.Bitboard.*
import chess.bitboard.Bitboard

class BishopTest extends ChessTest:

  "a bishop" should {

    val bishop = White - Bishop

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B



N B    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
        """
k B   x
     x
x   x
 x x
N B    P
 x x
PPPPPPPP
 NBQKBNR
"""
      )
    }

    "capture opponent pieces" in {
      val board = """
k B
     q
p

N B    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
        """
k B
     x
x   x
 x x
N B    P
 x x
PPPPPPPP
 NBQKBNR
"""
      )
    }
  }
