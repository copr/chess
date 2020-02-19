package cz.copr.chess.game

import eu.timepit.refined._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.numeric.Interval


object Position {
//  trait PositionOps[A] {
//    def add(pos: PiecePosition, x: Int, y: Int): Option[PiecePosition]
//  }

  def createPiecePosition(x: Int, y: Int): Option[PiecePosition] = for {
    positionX <- createPositionX(x)
    positionY <- createPositionY(y)
  } yield PiecePosition(positionX, positionY)

  def createPositionX(x: Int): Option[PositionX] =
    RefType.applyRef[PositionX](x).toOption

  def createPositionY(y: Int): Option[PositionY] =
    RefType.applyRef[PositionY](y).toOption

  case class PiecePosition(x: PositionX, y: PositionY)

//  type PositionX = String Refined MatchesRegex[W.`"abcdefgh"`.T]
  type PositionX = Int Refined Interval.Closed[W.`1`.T, W.`8`.T]
  type PositionY = Int Refined Interval.Closed[W.`1`.T, W.`8`.T]

  def addMove(piecePosition: PiecePosition, x: Int, y: Int): Option[PiecePosition] = for {
    availablePositionX <- createPositionX(piecePosition.x.value + x)
    availablePositionY <- createPositionY(piecePosition.y.value + y)
  } yield PiecePosition(availablePositionX, availablePositionY)

  def subtractMoves(piecePosition: PiecePosition, otherPiecePosition: PiecePosition): (Int, Int) =
    (piecePosition.x.value - otherPiecePosition.x.value, piecePosition.y.value - otherPiecePosition.y.value)

  def subtractMovesSafe(piecePosition: PiecePosition, x: Int, y: Int): Option[PiecePosition] = {
    val newX = piecePosition.x.value - x
    val newY = piecePosition.y.value - y
    createPiecePosition(newX, newY)
  }
}