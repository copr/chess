package cz.copr.chess.server

import cats.implicits._
import cats.effect.Sync
import cz.copr.chess.server.ChessRepo.MoveRequest
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }
import org.joda.time.DateTime

trait GameStatus
case object WhiteWon extends  GameStatus
case object BlackWon extends  GameStatus
case object Draw     extends  GameStatus
case object WhiteTurn extends GameStatus
case object BlackTurn extends GameStatus

case class GameState (
  gameId:       GameId,
  whitePlayer:  PlayerId,
  blackPlayer:  PlayerId,
  gameStatus:   GameStatus,
  lastMoveTime: Option[DateTime],
  moves:        List[MoveRequest]
)

object GameState {
  case class GameId(value: String) extends AnyVal

  implicit val gameStatusEncoder: Encoder[GameStatus] = {
    case WhiteWon  => Json.fromString("1-0")
    case BlackWon  => Json.fromString("0-1")
    case Draw      => Json.fromString("1/2-1/2")
    case WhiteTurn => Json.fromString("white")
    case BlackTurn => Json.fromString("black")
  }

  implicit val gameStatusDecoder: Decoder[GameStatus] = (c: HCursor) => {
    val failure = DecodingFailure("Couldn't parse GameStatus", c.history)
    c.value.fold(
      failure.asLeft[GameStatus],
      _ => failure.asLeft,
      _ => failure.asLeft,
      decodeGameStatusFromString(_).toRight(failure),
      _ => failure.asLeft,
      _ => failure.asLeft
    )
  }

  def decodeGameStatusFromString(s: String): Option[GameStatus] = s match {
    case "1-0"     => Some(WhiteWon)
    case "0-1"     => Some(BlackWon)
    case "1/2-1/2" => Some(Draw)
    case "white"   => Some(WhiteTurn)
    case "black"   => Some(BlackTurn)
    case _         => None
  }

  implicit val gameIdDecoder: Decoder[GameId] = Decoder.decodeString.map(GameId)
  implicit val gameIdEncoder: Encoder[GameId] = (id: GameId) => Json.fromString(id.value)

  implicit def gameIdEntityDecoder[F[_] : Sync]: EntityDecoder[F, GameId] = jsonOf[F, GameId]
  implicit def gameIdEntityEncoder[F[_] : Sync]: EntityEncoder[F, GameId] = jsonEncoderOf[F, GameId]

  implicit val datetimeEncoder: Encoder[DateTime] = (dt: DateTime) => Json.fromLong(dt.toInstant.getMillis)

  implicit val gameStateEncoder: Encoder[GameState] = deriveEncoder[GameState]

  implicit def gameStateEntityEncoder[F[_] : Sync]: EntityEncoder[F, GameState] = jsonEncoderOf[F, GameState]

}
