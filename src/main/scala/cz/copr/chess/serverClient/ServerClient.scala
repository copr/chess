package cz.copr.chess.serverClient

import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.ChessRepo.{ NewGameRequest, PlayRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import cz.copr.chess.server.{ GameMove, GameState, Player, RegistrationRequest }

// to je debilni jmeno? liberalnikonzervativni
trait ServerClient[F[_]] {
  def startGame(newGameRequest: NewGameRequest): F[GameId]

  def play(playRequest: PlayRequest): F[Unit]

  def getGameState(gameId: GameId): F[GameState]

  def register(registrationRequest: RegistrationRequest): F[PlayerId]
}
