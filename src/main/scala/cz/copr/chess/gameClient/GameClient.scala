package cz.copr.chess.gameClient

import cz.copr.chess.chessLogic.{ ChessState, Move }

sealed trait MoveError
case object IllegalMove

trait GameClient[F[_]] {
  def move(move: Move): F[ChessState.MoveResult]
}
