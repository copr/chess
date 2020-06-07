package cz.copr.chess.chessLogic

import org.specs2.mutable.Specification

class PositionTest extends Specification {
  "Positions piecePositionsBetween" should {
    "return all positions between a2 a5" in {
      val pos1 = Position.createPiecePosition(2, 1).get
      val pos2 = Position.createPiecePosition(5, 1).get

      val result = Position.piecePositionsBetween(pos1, pos2)

      val expectedYCoordinates = Vector(3, 4)

      result.map(_.x.value) shouldEqual expectedYCoordinates
    }
  }

  "return positions between two positions on the same diagonal" in {
    val pos1 = Position.createPiecePosition(1, 1).get
    val pos2 = Position.createPiecePosition(5, 5).get

    val result = Position.piecePositionsBetween(pos1, pos2)
    val expectedPositions = Vector(
      Position.createPiecePosition(2, 2),
      Position.createPiecePosition(3, 3),
      Position.createPiecePosition(4, 4)
    ).map(_.get)

    result shouldEqual expectedPositions
  }

  "should return empty vector for two positions different diagonals" in {
    val pos1 = Position.createPiecePosition(1, 1).get
    val pos2 = Position.createPiecePosition(6, 5).get

    val result = Position.piecePositionsBetween(pos1, pos2)
    val expectedPositions = Vector()

    result shouldEqual expectedPositions
  }

  "should return positions between two positions on the same diagonal, going from right to left" in {
    val pos1 = Position.createPiecePosition(5, 8).get
    val pos2 = Position.createPiecePosition(8, 5).get

    val result = Position.piecePositionsBetween(pos2, pos1).toSet
    val expectedPositions = Set(
      Position.createPiecePosition(6, 7),
      Position.createPiecePosition(7, 6)
    ).map(_.get)


    result shouldEqual expectedPositions
  }
}
