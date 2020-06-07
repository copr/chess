package cz.copr.chess.chessLogic

import cats.implicits._
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

  def piecePositionsBetween(position1: PiecePosition, position2: PiecePosition): Vector[PiecePosition] =
    positionsBetweenRookLike(position1, position2) ++ positionsBetweenCrossLike(position1, position2)

  private def positionsBetweenCrossLike(position1: PiecePosition, position2: PiecePosition): Vector[PiecePosition] = {
    val xPositions = positionsBetween(position1.x, position2.x)
    val yPositions = positionsBetween(position1.y, position2.y)

    if (xPositions.length == yPositions.length) {
      xPositions.zip(yPositions).map { case (x, y) => PiecePosition(x, y) }
    }  else {
      Vector()
    }
  }

  private def positionsBetweenRookLike(position1: PiecePosition, position2: PiecePosition): Vector[PiecePosition] = (position1, position2) match {
    case (p1, p2) if p1.x == p2.x => positionsBetween(p1.y, p2.y).map(PiecePosition(p1.x, _))
    case (p1, p2) if p1.y == p2.y => positionsBetween(p1.x, p2.x).map(PiecePosition(_, p1.y))
    case _ => Vector()
  }

  private def positionsBetween(position1: Position, position2: Position): Vector[Position] = (position1, position2) match {
    case (p1, p2) if p1.value > p2.value => Vector.range(position2.value + 1, position1.value).flatMap(createPosition)
    case (p1, p2) if p1.value < p2.value => Vector.range(position1.value + 1, position2.value).flatMap(createPosition)
    case _                               => Vector()
  }
}

