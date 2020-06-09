package cz.copr.chess.serverClient

import cats.effect.Sync
import cats.implicits._
import cz.copr.chess.server.ChessRepo.{ NewGameRequest, PlayRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import cz.copr.chess.server.{ GameState, RegistrationRequest }
import org.http4s.client.Client
import org.http4s.{ Method, Request, Uri }

class HttpServerClient[F[_] : Sync](client: Client[F], chessServerUri: Uri) extends ServerClient[F] {
  def startGame(newGameRequest: NewGameRequest): F[GameId] = for {
    req    <- Request[F](Method.POST, chessServerUri / "start-game").withEntity(newGameRequest).pure[F]
    gameId <- client.expect[GameId](req)
  } yield gameId

  def play(playRequest: PlayRequest): F[Unit] = for {
    req <- Request[F](Method.POST, chessServerUri / "play").withEntity(playRequest).pure[F]
    _   <- client.expect[Unit](req)
  } yield ()

  def getGameState(gameId: GameId): F[GameState] = for {
    req   <- Request[F](Method.GET, chessServerUri / "game" / gameId.value).pure[F]
    state <- client.expect[GameState](req)
  } yield state

  def register(registrationRequest: RegistrationRequest): F[PlayerId] = for {
    req      <- Request[F](Method.POST, chessServerUri / "register").withEntity(registrationRequest).pure[F]
    playerId <- client.expect[PlayerId](req)
  } yield playerId
}
