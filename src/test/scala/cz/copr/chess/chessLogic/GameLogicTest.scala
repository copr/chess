package cz.copr.chess.chessLogic

import cz.copr.chess.portableGameNotation.{NotationParser, PortableGameNotationParser}
import eu.timepit.refined.auto._
import org.specs2.mutable.Specification
import Position._


class GameLogicTest extends Specification {


  "GameLogic" should {
    "state that the game is finished when it is finished" in {
      val s = "[Event \"Aarhus Chess House GM Dec\"]\n[Site \"Aarhus DEN\"]\n[Date \"2019.12.15\"]\n[Round \"-\"]\n[White \"Vasli,A\"]\n[Black \"Jacobsen,Mikkel Manosri\"]\n[Result \"0-1\"]\n[WhiteTitle \"FM\"]\n[BlackTitle \"FM\"]\n[Opening \"Caro-Kann\"]\n[Variation \"Panov-Botvinnik attack\"]\n[BlackElo \"2348\"]\n[ECO \"B13\"]\n[EventDate \"2019.12.11\"]\n[WhiteElo \"2413\"]\n[BlackFideId \"1444514\"]\n[WhiteFideId \"417521\"]\n\n1.e4 c6 2.d4 d5 3.exd5 cxd5 4.c4 Nf6 5.Nc3 Nc6 6.Bg5 Be6 7.a3 Qd7 8.Nf3\nNe4 9.cxd5 Bxd5 10.Be3 Nxc3 11.bxc3 g6 12.Be2 Bg7 13.O-O O-O 14.c4 Bxf3\n15.Bxf3 Nxd4 16.Bxd4 Bxd4 17.Rb1 Rab8 18.Rb5 e6 19.Qb1 Qe7 20.Rxb7 Rxb7\n21.Qxb7 Qxa3 22.Qc7 Qc3 23.Rd1 Bb6 24.Qf4 Qc2 25.Rf1 Rc8 26.Bb7 Qxc4 27.Qf3\nRc5 28.g3 Rf5 29.Qa3 Qe2 30.Kg2 Rxf2+ 31.Rxf2 Qxf2+ 32.Kh1 Qg1#   0-1"
      PortableGameNotationParser.parsePng(s) match {
        case Right(value) =>
          val game = TestGame.playFromInitialState(value.moves.map(_._1))
          game.gameResult.isFinished shouldEqual true
        case Left(_) => true shouldEqual false
      }
    }

    "state that game is drawn when one team has nowhere to move" in {
      val board = ChessBoard(Map())
      val setupBoard = board
        .put(Queen(PiecePosition(3, 1), White), PiecePosition(3, 1))
        .put(King(PiecePosition(4, 6), White, moved = true), PiecePosition(4, 6))
        .put(King(PiecePosition(1, 8), Black, moved = true), PiecePosition(1, 8))
      val game = ChessState(setupBoard, White, Ongoing, List())
      val queenMove = BigPieceMove(QueenType, None, None, 7, 3)

      val gameAfterMove = TestGame.doMoves(List(queenMove), game)

      gameAfterMove match {
        case Left(value) =>
          println(value)
          false
        case Right(finalGameState) =>
          finalGameState.gameResult == Draw
      }

    }

    "state that white big castling is impossible if the rook has already moved" in {
      val (gameState, moves) = createCastlingSequenceRookMoves(White, big = true)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that black big castling is impossible if the rook has already moved" in {
      val (gameState, moves) = createCastlingSequenceRookMoves(Black, big = true)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that white small castling is impossible if the rook has already moved" in {
      val (gameState, moves) = createCastlingSequenceRookMoves(White, big = false)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that black small castling is impossible if the rook has already moved" in {
      val (gameState, moves) = createCastlingSequenceRookMoves(Black, big = false)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that black small castling is impossible if the king has already moved" in {
      val (gameState, moves) = createCastlingSequenceKingMoves(Black, big = false)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that black big castling is impossible if the king has already moved" in {
      val (gameState, moves) = createCastlingSequenceKingMoves(Black, big = true)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that white small castling is impossible if the king has already moved" in {
      val (gameState, moves) = createCastlingSequenceKingMoves(White, big = false)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) => {
          ConsoleGame.putCurrentState(gameState)
          false
        }
      }
    }

    "state that white big castling is impossible if the king has already moved" in {
      val (gameState, moves) = createCastlingSequenceKingMoves(White, big = true)

      TestGame.doMoves(moves, gameState) match {
        case Left(v) =>
          if (v.isInstanceOf[CastlingIllegal]) {
            true
          } else {
            println(v)
            false
          }
        case Right(gameState) =>
          ConsoleGame.putCurrentState(gameState)
          false
      }
    }

    "state that game is drawn when either team has not enough material to checkmate" in {
      val moves = parseMoves(List("Qd4+", "Qxd4+", "Kxd4"))
      val board = ChessBoard(Map())
        .putChessPiece(King(createPiecePosition(3, 5).get, White, moved = true))
        .putChessPiece(King(createPiecePosition(6, 4).get, Black, moved = true))
        .putChessPiece(Queen(createPiecePosition(4, 8).get, White))
        .putChessPiece(Queen(createPiecePosition(7, 7).get, Black))
        .putChessPiece(Knight(createPiecePosition(5, 3).get, White))
      val game = ChessState(board, White, Ongoing, List())
      TestGame.playFromState(moves, game).gameResult == Draw
    }

    "state that game is won by white when black king is checked and no one can prevent it" in {
      val moves = parseMoves(List("e4", "a6", "Qf3", "a5", "Bc4", "a4", "Qf7"))

      val result = TestGame.playFromInitialState(moves)

      result.gameResult shouldEqual WhiteWon
    }

    "state that game is ongoing when black king is checked, king has nowhere to move, but there is a piece that can prevent check mate" in {
      val moves = parseMoves(List("e4", "f6", "Qh5"))

      val result = TestGame.playFromInitialState(moves)

      result.gameResult shouldEqual Ongoing
    }
  }

  def parseMoves(moves: List[String]): List[Move] =
    moves
      .map(NotationParser.parseMove)
      .map(_.getOrElse(throw new RuntimeException("Moves incorrectly specified")))

  def createCastlingSequenceKingMoves(team: Team, big: Boolean): (ChessState, List[Move]) = {
    val (board, xPosition, newXPosition, _) = setupCastling(team, big)

    val gameState = ChessState(board, team, gameResult = Ongoing, List())
    val kingMove1 = BigPieceMove(KingType, None, None, 5, newXPosition)
    val kingMove2 = BigPieceMove(KingType, None, None, 5, xPosition)
    val castling  = if (big) BigCastling else SmallCastling

    (gameState, List(kingMove1, kingMove2, castling))
  }

  private def setupCastling(team: Team, big: Boolean): (ChessBoard, Position, Position, Position) = {
    val xPosition = createPosition(if (team == White) 1 else 8).get
    val yPosition = createPosition(if (big) 1 else 8).get
    val xIncrement = if (team == White) 1 else -1
    val newXPosition = Position.createPosition(xPosition + xIncrement).get

    val rookPosition = PiecePosition(xPosition, yPosition)
    val kingPosition = PiecePosition(xPosition, 5)
    val board = ChessBoard(Map())
      .put(Rook(rookPosition, team, moved = false), rookPosition)
      .put(King(kingPosition, team, moved = false), kingPosition)
    (board, xPosition, newXPosition, yPosition)
  }

  private def createCastlingSequenceRookMoves(team: Team, big: Boolean): (ChessState, List[Move]) = {
    val (board, xPosition, newXPosition, yPosition) = setupCastling(team, big)

    val gameState = ChessState(board, team, gameResult = Ongoing, moves = List())
    val rookMove1 = BigPieceMove(RookType, None, None, yPosition, newXPosition)
    val rookMove2 = BigPieceMove(RookType, None, None, yPosition, xPosition)
    val castling = if (big) BigCastling else SmallCastling

    (gameState, List(rookMove1, rookMove2, castling))
  }
}
