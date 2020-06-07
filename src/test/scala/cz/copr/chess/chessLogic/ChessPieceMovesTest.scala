package cz.copr.chess.chessLogic

import org.specs2.mutable.Specification

class ChessPieceMovesTest extends Specification {

  "GameTest canBishopMove" should {
    "bishop should be able to caputer pawn" in {
      val b = Bishop(Position.createPiecePosition(1, 1).get, White)
      val o = Pawn(Position.createPiecePosition(2, 2).get, Black, false)
      ChessPiece.canBishopMove(b, o) must beEqualTo(true)
    }
  }

  "GameTest canBishopMove" should {
    "bishop should be able to capture pawn" in {
      val b = Bishop(Position.createPiecePosition(2, 1).get, White)
      val o = Pawn(Position.createPiecePosition(5, 4).get, Black, false)
      ChessPiece.canBishopMove(b, o) must beEqualTo(true)
    }
  }


  "GameTest canBishopMove" should {
    "bishop should NOT be able to caputre pawn" in {
      val b = Bishop(Position.createPiecePosition(1, 1).get, White)
      val o = Pawn(Position.createPiecePosition(2, 1).get, Black, false)
      ChessPiece.canBishopMove(b, o) must beEqualTo(false)
    }
  }

  "GameTest canKingMove" should {
    "allow king to capture oponnents piece diagonally" in {
      val k = King(Position.createPiecePosition(5, 5).get, White, false)
      val q = Queen(Position.createPiecePosition(6, 6).get, Black)
      ChessPiece.canKingMove(k, q) must beEqualTo(true)
    }
  }
}
