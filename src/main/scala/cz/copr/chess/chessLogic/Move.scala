package cz.copr.chess.chessLogic

import cats.effect.Sync
import cz.copr.chess.chessLogic.Position.Position
import cz.copr.chess.portableGameNotation.NotationParser
import io.circe.{ Decoder, Encoder, Json }
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }


sealed trait Move
final case class BigPieceMove(piece: PieceType, file: Option[Position], rank: Option[Position], toFile: Position, toRank: Position) extends Move
final case class BigPieceCapture(piece: PieceType, file: Option[Position], rank: Option[Position], captureFile: Position, captureRank: Position) extends Move
final case class PawnMove(file: Position, toRank: Position, promoteTo: Option[PieceType]) extends Move
final case class PawnCapture(file: Position, captureFile: Position, captureRank: Position, promoteTo: Option[PieceType]) extends Move
case object SmallCastling extends Move
case object BigCastling extends Move

sealed trait PieceType
case object PawnType extends PieceType
case object RookType extends PieceType
case object BishopType extends PieceType
case object KnightType extends PieceType
case object QueenType extends PieceType
case object KingType extends PieceType

object Move {
  implicit val moveDecoder: Decoder[Move] = Decoder
    .decodeString
    .emap(s => NotationParser.parseMove(s))

  def fileNumberToLetter(y: Position): String = y.value match {
    case 1 => "a"
    case 2 => "b"
    case 3 => "c"
    case 4 => "d"
    case 5 => "e"
    case 6 => "f"
    case 7 => "g"
    case 8 => "h"
  }

  def rankToString(x: Position): String = x.value.toString

  def pieceTypeToString(pieceType: PieceType): String = pieceType match {
    case PawnType   => ""
    case RookType   => "R"
    case BishopType => "B"
    case KnightType => "N"
    case QueenType  => "Q"
    case KingType   => "K"
  }

  implicit val moveEncoder: Encoder[Move] = {
    case BigPieceMove(piece, file, rank, toFile, toRank) => Json.fromString(
      pieceTypeToString(piece) +
        file.map(fileNumberToLetter).getOrElse("") +
        rank.map(rankToString).getOrElse("") +
        fileNumberToLetter(toFile) +
        rankToString(toRank)
    )
    case BigPieceCapture(piece, file, rank, captureFile, captureRank) => Json.fromString(
      pieceTypeToString(piece) +
        file.map(fileNumberToLetter).getOrElse("") +
        rank.map(rankToString).getOrElse("") +
        "x" +
        fileNumberToLetter(captureFile) +
        rankToString(captureRank)
    )
    case PawnMove(file, toRank, promoteTo) => Json.fromString(
      fileNumberToLetter(file) +
        rankToString(toRank) +
        promoteTo.map(pt => "=" + pieceTypeToString(pt)).getOrElse("")
    )
    case PawnCapture(file, captureFile, captureRank, promoteTo) => Json.fromString(
      fileNumberToLetter(file) +
        "x" +
        fileNumberToLetter(captureFile) +
        rankToString(captureRank) +
        promoteTo.map(pt => "=" + pieceTypeToString(pt)).getOrElse("")
    )
    case SmallCastling => Json.fromString("O-O")
    case BigCastling => Json.fromString("O-O-O")
  }

  implicit def moveEntityEncoder[F[_]: Sync]: EntityEncoder[F, Move] = jsonEncoderOf[F, Move]
  implicit def moveEntityDecoder[F[_]: Sync]: EntityDecoder[F, Move] = jsonOf[F, Move]
}