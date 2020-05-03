package cz.copr.chess.server

import cats.effect.Sync
import cats.implicits._
import cz.copr.chess.server.ChessRepo.{ FinishGameRequest, MoveRequest, NewGameRequest, listEntityEncoder }
import cz.copr.chess.server.Player._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import io.circe.syntax._
import RegistrationRequest.registrationRequestEntityDecoder
import io.circe.Json


object ChessRoutes {

  object PlayerIdVar {
    def unapply(str: String): Option[PlayerId] =
      playerIdDecoder.decodeJson(Json.fromString(str)).toOption
  }

  def chessRoutes[F[_]: Sync](repo: ChessRepo[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
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
      case GET -> Root / "check-games" / PlayerIdVar(playerId) =>
          repo
            .getGames(playerId)
            .attempt
            .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(gameList) => Ok(gameList)
          }

      case req @ POST -> Root / "play" =>
        req.attemptAs[MoveRequest].value.flatMap {
          case Right(moveRequest) => repo.move(moveRequest)
          case Left(_) => req.as[FinishGameRequest].flatMap(finishGameRequest => repo.finish(finishGameRequest))
        }
        .attempt
        .flatMap {
          case Left(t) => BadRequest(t.getMessage)
          case Right(gameList) => Ok(gameList)
        }


    }
  }
}
