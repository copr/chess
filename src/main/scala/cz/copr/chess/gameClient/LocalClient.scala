package cz.copr.chess.gameClient

import cats.effect.Sync
import cats.implicits._
import cats.mtl.MonadState
import cz.copr.chess.chessLogic.{ ChessLogic, ChessState, Move }

object LocalClient {
  def apply[F[_] : Sync](implicit S: MonadState[F, ChessState]): GameClient[F] = (move: Move) => for {
    chessState <- S.get
    result = ChessLogic.move(move, chessState)
  } yield result
}

