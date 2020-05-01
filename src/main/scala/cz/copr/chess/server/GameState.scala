package cz.copr.chess.server

import cats.effect.Sync
import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player._
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }
import org.joda.time.DateTime

trait GameResult
case object Player1Won extends GameResult
case object Player2Won extends GameResult
case object Draw       extends GameResult
case object Ongoing    extends GameResult
final case class Undecided(player1Result: GameResult, player2Result: GameResult) extends GameResult

case class GameState (
  gameId: GameId,
  playerId1: PlayerId,
  playerId2: PlayerId,
  gameResult: GameResult,
  lastMoveTime: DateTime,
  moves: List[Move]
)

object GameState {
  case class GameId(value: String) extends AnyVal

  implicit val gameResultEncoder: Encoder[GameResult] = {
    case Player1Won => Json.fromString("1-0")
    case Player2Won => Json.fromString("0-1")
    case Draw       => Json.fromString("1/2-1/2")
    case Ongoing    => Json.fromString("-")
    case Undecided(player1Result, player2Result) => Json
      .fromFields(List(
        "player1-result" -> gameResultEncoder.apply(player1Result),
        "player2-result" -> gameResultEncoder.apply(player2Result)
      ))
  }

  implicit val gameIdDecoder: Decoder[GameId] = Decoder.decodeString.map(GameId)
  implicit val gameIdEncoder: Encoder[GameId] = (id: GameId) => Json.fromString(id.toString)

  implicit def gameIdEntityDecoder[F[_] : Sync]: EntityDecoder[F, GameId] = jsonOf[F, GameId]
  implicit def gameIdEntityEncoder[F[_] : Sync]: EntityEncoder[F, GameId] = jsonEncoderOf[F, GameId]

  implicit val datetimeEncoder: Encoder[DateTime] = (dt: DateTime) => Json.fromLong(dt.toInstant.getMillis)

  implicit val gameStateEncoder: Encoder[GameState] = deriveEncoder[GameState]

  implicit def gameStateEntityEncoder[F[_] : Sync]: EntityEncoder[F, GameState] = jsonEncoderOf[F, GameState]

}
