package cz.copr.chess.game

import cats.effect.Sync
import cats.implicits._
import cats.mtl.MonadState
import cz.copr.chess.chessClient.ChessClient
import cz.copr.chess.chessLogic.{ ChessState, Ongoing, White }
import cz.copr.chess.gameClient.GameClient

class Game[F[_] : Sync](
  whitePlayer: ChessClient[F],
  blackPlayer: ChessClient[F],
  gameClient: GameClient[F])(implicit S: MonadState[F, ChessState])  {

  def playUntilTheEnd: F[Unit] = for {
    state    <- S.get
    newState <- turn(getCurrentPlayer(state))
    _        <- if (newState.gameResult != Ongoing) {
      ().pure[F]
    } else {
      playUntilTheEnd
    }
  } yield ()

  private def turn(player: ChessClient[F]): F[ChessState] = for {
    state    <- S.get
    move     <- player.getMove(state)
    gameResp <- gameClient.move(move)
    newState <- handleResp(player, gameResp)
    _        <- S.set(newState.switchTeam)
  } yield newState

  private def handleResp(player: ChessClient[F], gameResp: ChessState.MoveResult): F[ChessState] = {
    gameResp.fold(
      err => for {
        _   <- player.inform(err.toString)
        res <- turn(player)
      } yield res,
      _.pure[F]
    )
  }

  private def getCurrentPlayer(state: ChessState): ChessClient[F] =
    if (state.team == White) {
      whitePlayer
    } else {
      blackPlayer
    }
}