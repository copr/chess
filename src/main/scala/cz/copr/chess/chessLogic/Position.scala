package cz.copr.chess.chessLogic

import eu.timepit.refined._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.numeric.Interval


object Position {
//  trait PositionOps[A] {
//    def add(pos: PiecePosition, x: Int, y: Int): Option[PiecePosition]
//  }

  def createPiecePosition(x: Int, y: Int): Option[PiecePosition] = for {
    positionX <- createPosition(x)
    positionY <- createPosition(y)
  } yield PiecePosition(positionX, positionY)

  def createPosition(x: Int): Option[Position] =
    RefType.applyRef[Position](x).toOption

  case class PiecePosition(x: Position, y: Position)

  type Position = Int Refined Interval.Closed[W.`1`.T, W.`8`.T]

  def addMove(piecePosition: PiecePosition, x: Int, y: Int): Option[PiecePosition] = for {
    availablePositionX <- createPosition(piecePosition.x.value + x)
    availablePositionY <- createPosition(piecePosition.y.value + y)
  } yield PiecePosition(availablePositionX, availablePositionY)

  def subtractMoves(piecePosition: PiecePosition, otherPiecePosition: PiecePosition): (Int, Int) =
    (piecePosition.x.value - otherPiecePosition.x.value, piecePosition.y.value - otherPiecePosition.y.value)

  def subtractMovesSafe(piecePosition: PiecePosition, x: Int, y: Int): Option[PiecePosition] = {
    val newX = piecePosition.x.value - x
    val newY = piecePosition.y.value - y
    createPiecePosition(newX, newY)
  }
}