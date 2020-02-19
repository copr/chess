package cz.copr.chess.game

import org.specs2.mutable.Specification

class ChessBoardTest extends Specification {

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is empty in the same column" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(5, 1).get
      val chessBoard = ChessBoard.chessBoard(Map(1-> Map(1-> ChessPiece.empty(positionA)))).get
      chessBoard.isEmptyFromTo(positionA, positionB) mustEqual true
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is empty crosswise" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(5, 5).get
      val chessBoard = ChessBoard.chessBoard(Map(1-> Map(1-> ChessPiece.empty(positionA)))).get
      chessBoard.isEmptyFromTo(positionA, positionB) mustEqual true
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is empty crosswise" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(5, 5).get
      val chessBoard = ChessBoard.chessBoard(Map(1-> Map(1-> ChessPiece.empty(positionA)))).get
      chessBoard.isEmptyFromTo(positionB, positionA) mustEqual true
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is not empty going from one column to another if it is not crosswise or right next to it" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(3, 2).get
      val chessBoard = ChessBoard.chessBoard(Map(1-> Map(1-> ChessPiece.empty(positionA)))).get
      chessBoard.isEmptyFromTo(positionB, positionA) mustEqual false
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is not empty going from one column to another if it is not crosswise or right next to it" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(3, 2).get
      val chessBoard = ChessBoard.chessBoard(Map(1-> Map(1-> ChessPiece.empty(positionA)))).get
      chessBoard.isEmptyFromTo(positionA, positionB) mustEqual false
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is empty in the same row" in {
      val positionA = Position.createPiecePosition(1, 5).get
      val positionB = Position.createPiecePosition(1, 1).get
      val chessBoard = ChessBoard.chessBoard(Map()).get
      chessBoard.isEmptyFromTo(positionA, positionB) mustEqual true
    }
  }

  "ChessBoardTest isEmptyFromTo" should {
    "on an empty board say it is empty in the same row from left to right" in {
      val positionA = Position.createPiecePosition(1, 1).get
      val positionB = Position.createPiecePosition(1, 5).get
      val chessBoard = ChessBoard.chessBoard(Map()).get
      chessBoard.isEmptyFromTo(positionA, positionB) mustEqual true
    }
  }


}
