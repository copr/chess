package cz.copr.chess.chessLogic

import cz.copr.chess.chessLogic.Position.{PositionX, PositionY}

sealed trait IllegalMoveReason

final case class CouldNotFindThePiece(pieceType: PieceType) extends IllegalMoveReason
final case class AmbiguousPieceDefinition(pieceType: PieceType) extends IllegalMoveReason
final case class PieceNotFound(pieceType: PieceType, rank: Option[PositionX], file: Option[PositionY], team: Team) extends IllegalMoveReason
final case class CastlingIllegal(msg: String) extends IllegalMoveReason
case object PieceNotFound extends IllegalMoveReason
case object Check extends IllegalMoveReason
case object PromotionPieceNotSpecified extends IllegalMoveReason
case object MoveNotAllowed extends IllegalMoveReason
case object EnPassantInapplicable extends IllegalMoveReason
case object LogicError extends IllegalMoveReason