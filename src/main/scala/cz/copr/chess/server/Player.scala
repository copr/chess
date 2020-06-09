package cz.copr.chess.server

import cats.effect.Sync
import cz.copr.chess.server.Player.PlayerId
import io.circe.generic.semiauto._
import io.circe.{ Decoder, Encoder, Json }
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }


final case class Player(
  id:        PlayerId,
  nickname:  String,
  firstName: Option[String],
  lastName:  Option[String],
  mail:      Option[String]
)

object Player {
  case class PlayerId(value: String) extends AnyVal

  implicit val playerIdDecoder: Decoder[PlayerId] = Decoder.decodeString.map(PlayerId)
  implicit val playerIdEncoder: Encoder[PlayerId] = (id: PlayerId) => Json.fromString(id.value)

  implicit def playerIdEntityDecoder[F[_] : Sync]: EntityDecoder[F, PlayerId] = jsonOf[F, PlayerId]
  implicit def playerIdEntityEncoder[F[_] : Sync]: EntityEncoder[F, PlayerId] = jsonEncoderOf[F, PlayerId]


  implicit val playerDecoder: Decoder[Player] = deriveDecoder[Player]
  implicit def playerEntityDecoder[F[_] : Sync]: EntityDecoder[F, Player] =
    jsonOf[F, Player]

  implicit val playerEncoder: Encoder[Player] = deriveEncoder[Player]
  implicit def playerEntityEncoder[F[_] : Sync]: EntityEncoder[F, Player] =
    jsonEncoderOf[F, Player]
}