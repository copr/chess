package cz.copr.chess.server

import cats.effect.{ IO, Sync }
import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.ChessRepo.{ MoveRequest, NewGameRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import io.circe.Encoder
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }

trait ChessRepo[F[_]] {
  def register(player: Player): F[PlayerId]

  def startGame(request: NewGameRequest): F[GameId]

  def getGames(playerId: PlayerId): F[List[GameState]]

  def move(moveRequest: MoveRequest): F[Unit]
}

object MemoryRepo extends ChessRepo[IO] {
  def register(player: Player): IO[PlayerId] = ???

  def startGame(request: NewGameRequest): IO[GameId] = ???

  def getGames(playerId: PlayerId): IO[List[GameState]] = ???

  def move(moveRequest: MoveRequest): IO[Unit] = ???
}

object ChessRepo {
  sealed trait GameVariant
  case object WhiteVsBlack extends GameVariant
  case object BlackVsWhite extends GameVariant

  case class NewGameRequest(player1Id: PlayerId, player2Id: PlayerId, variant: GameVariant)

  case class MoveRequest(playerId: PlayerId, gameId: GameId, move: Move)

  implicit def newGameRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, NewGameRequest] =
    jsonOf[F, NewGameRequest]

  implicit def moveRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, MoveRequest] =
    jsonOf[F, MoveRequest]

  implicit def listEntityEncoder[F[_] : Sync, A](implicit E: Encoder[A]): EntityEncoder[F, List[A]] = jsonEncoderOf[F, List[A]]
}
