package cz.copr.chess.game

import cz.copr.chess.game.Position.{PiecePosition, addMove, subtractMoves}

sealed trait ChessPiece {
  val position: PiecePosition
  val team: Team

  def isEmpty: Boolean = this match {
    case _: Empty => true
    case _ => false
  }
}

case class Empty(position: PiecePosition, team: Team) extends ChessPiece

case class Pawn(position: PiecePosition, team: Team, enpasantable: Boolean) extends ChessPiece

case class Rook(position: PiecePosition, team: Team, moved: Boolean) extends ChessPiece

case class Knight(position: PiecePosition, team: Team) extends ChessPiece

case class Bishop(position: PiecePosition, team: Team) extends ChessPiece

case class Queen(position: PiecePosition, team: Team) extends ChessPiece

case class King(position: PiecePosition, team: Team, moved: Boolean) extends ChessPiece

sealed trait Team {
  def getOtherTeam: Team = this match {
    case White => Black
    case Black => White
    case Nothing => Nothing
  }
}

case object White extends Team

case object Black extends Team

case object Nothing extends Team

object ChessPiece {

  def create(pieceType: PieceType, position: PiecePosition, team: Team): ChessPiece = pieceType match {
    case QueenType => Queen(position, team)
    case RookType => Rook(position, team, moved = false)
    case KnightType => Knight(position, team)
    case BishopType => Bishop(position, team)
    case KingType => King(position, team, moved = false)
    case PawnType => Pawn(position, team, enpasantable = false)
  }

  def empty(piecePosition: PiecePosition): Empty = Empty(piecePosition, Nothing)

  def canPawnMove(pawn: Pawn, otherPiece: ChessPiece): Boolean = {
    val team = pawn.team
    val sign = if (team == White) 1 else -1
    val start = if (team == White) 2 else 7
    val normalMoveAvailable = addMove(pawn.position, sign * 1, 0).contains(otherPiece.position) && otherPiece.isEmpty
    val doubleMoveAvailable = pawn.position.x.value == start  &&
      addMove(pawn.position, sign * 2, 0).contains(otherPiece.position) &&
      otherPiece.isEmpty
    val canOtherPieceBeAttacked = pawn.team != otherPiece.team && !otherPiece.isEmpty
    val leftAttackMoveAvailable = addMove(pawn.position, sign * 1, sign * -1).contains(otherPiece.position) && canOtherPieceBeAttacked
    val rightAttackMoveAvailable = addMove(pawn.position, sign * 1, sign * 1).contains(otherPiece.position) && canOtherPieceBeAttacked
    normalMoveAvailable || doubleMoveAvailable || leftAttackMoveAvailable || rightAttackMoveAvailable
  }

  def canRookMove(rook: ChessPiece, otherPiece: ChessPiece): Boolean = {
    (rook.position.x == otherPiece.position.x || rook.position.y == otherPiece.position.y) &&
      rook.team != otherPiece.team
  }

  def canKnightMove(knight: Knight, otherPiece: ChessPiece): Boolean = {
    val red = subtractMoves(knight.position, otherPiece.position)
    val absX = math.abs(red._1)
    val absY = math.abs(red._2)
    val ret = ((absX == 2 && absY == 1) || (absX == 1 && absY == 2)) && knight.team != otherPiece.team
    ret
  }

  def canBishopMove(bishop: ChessPiece, otherPiece: ChessPiece): Boolean = {
    val res = subtractMoves(bishop.position, otherPiece.position)
    val absed = (math.abs(res._1), math.abs(res._2))
    absed._1 == absed._2 && bishop.team != otherPiece.team
  }

  def canQueenMove(queen: Queen, otherPiece: ChessPiece): Boolean =
    canBishopMove(queen, otherPiece) || canRookMove(queen, otherPiece)

  def canKingMove(king: King, otherPiece: ChessPiece): Boolean = {
    val res = subtractMoves(king.position, otherPiece.position)
    val absed = (math.abs(res._1), math.abs(res._2))
    (absed._1 == 1 || absed._1 == 0) && (absed._2 == 1 || absed._2 == 0) &&
      (absed._1 != 0 || absed._2 != 0) && otherPiece.team != king.team
  }
}