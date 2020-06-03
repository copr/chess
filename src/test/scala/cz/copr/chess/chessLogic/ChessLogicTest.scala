package cz.copr.chess.chessLogic

import org.specs2.mutable.Specification

class ChessLogicTest extends Specification {

  "ChessLogic getAllPosibleMoves" should {
    "return some moves for a black team when board is in initial state" in {
      val chessBoard = ChessState.createInitialState

      ChessLogic.getAllPossibleMoves(chessBoard).nonEmpty
    }
  }
}
