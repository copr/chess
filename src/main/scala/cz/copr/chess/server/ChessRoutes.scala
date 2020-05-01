package cz.copr.chess.server

import cats.effect.Sync
import cats.implicits._
import cz.copr.chess.server.ChessRepo.{ MoveRequest, NewGameRequest, listEntityEncoder }
import cz.copr.chess.server.Player._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl


object ChessRoutes {

  def chessRoutes[F[_]: Sync](chessRepo: ChessRepo[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      // vrat Id
      case req @ POST -> Root / "register" =>
        req
          .as[Player]
          .flatMap(player => chessRepo.register(player).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(playerId) => Ok(playerId.value)
          }

      // kdo, s kym, varianta,
      case req @ POST -> Root / "start-game" =>
        req
          .as[NewGameRequest]
          .flatMap(newGame => chessRepo.startGame(newGame).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(gameId) => Ok(gameId.value)
          }

      // vrat zacate hry
      case req @ GET -> Root / "check-games" =>
        req
          .as[PlayerId]
          .flatMap(playerId => chessRepo.getGames(playerId).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(gameList) => Ok(gameList)
          }

      case req @ POST -> Root / "move" =>
        req
          .as[MoveRequest]
          .flatMap(moveRequest => chessRepo.move(moveRequest).attempt)
          .flatMap {
            case Left(t) => BadRequest(t.getMessage)
            case Right(_) => Ok()
          }

    }
  }
}
