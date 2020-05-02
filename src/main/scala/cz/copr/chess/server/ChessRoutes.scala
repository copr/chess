package cz.copr.chess.server

import cats.effect.Sync
import cats.implicits._
import cz.copr.chess.server.ChessRepo.{ MoveRequest, NewGameRequest, listEntityEncoder }
import cz.copr.chess.server.Player._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import RegistrationRequest.registrationRequestEntityDecoder


object ChessRoutes {

  def chessRoutes[F[_]: Sync : ChessRepo]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    val repo = ChessRepo[F]
    import dsl._
    HttpRoutes.of[F] {
      // vrat Id
      case req @ POST -> Root / "register" =>
        req
          .as[RegistrationRequest]
          .flatMap(player => repo.register(player).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(playerId) => Ok(playerId.value)
          }

      // kdo, s kym, varianta,
      case req @ POST -> Root / "start-game" =>
        req
          .as[NewGameRequest]
          .flatMap(newGame => repo.startGame(newGame).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(gameId) => Ok(gameId.value)
          }

      // vrat zacate hry
      case req @ GET -> Root / "check-games" =>
        req
          .as[PlayerId]
          .flatMap(playerId => repo.getGames(playerId).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(gameList) => Ok(gameList)
          }

      case req @ POST -> Root / "move" =>
        req
          .as[MoveRequest]
          .flatMap(moveRequest => repo.move(moveRequest).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(_) => Ok()
          }

    }
  }
}
