package cz.copr.chess.game

import cz.copr.chess.game.Position.{PiecePosition, PositionX, PositionY}

sealed trait Move
final case class BigPieceMove(piece: PieceType, file: Option[PositionY], rank: Option[PositionX], toFile: PositionY, toRank: PositionX) extends Move
final case class BigPieceCapture(piece: PieceType, file: Option[PositionY], rank: Option[PositionX], captureFile: PositionY, captureRank: PositionX) extends Move
final case class PawnMove(file: PositionY, toRank: PositionX, promoteTo: Option[PieceType]) extends Move
final case class PawnCapture(file: PositionY, captureFile: PositionY, captureRank: PositionX, promoteTo: Option[PieceType]) extends Move
case object SmallCastling extends Move
case object BigCastling extends Move

sealed trait PieceType
case object PawnType extends PieceType
case object RookType extends PieceType
case object BishopType extends PieceType
case object KnightType extends PieceType
case object QueenType extends PieceType
case object KingType extends PieceType