package cz.copr.chess.chessClient

import cz.copr.chess.chessLogic.{ ChessState, Move }

trait ChessClient[F[_]] {
  def getMove(gameState: ChessState): F[Move]

  def inform(msg: String): F[Unit]
}

