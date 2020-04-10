package cz.copr.chess.inputOutput

import cats.implicits._
import cats.effect.IO
import cats.effect.Console.io._
import cats.~>
import cz.copr.chess.chessLogic._
import cz.copr.chess.portableGameNotation.NotationParser

object ConsoleInputOutputInterpreter extends (InputOutput  ~> IO) {
  def apply[A](inputOutput: InputOutput[A]) = inputOutput match {
    case PrintState(gameState: ChessState) => putCurrentState(gameState)
    case GetMove(_: Team) => readMove
  }

  private def readMove: IO[Move] =  for {
    _          <- putStrLn("State your move:")
    moveString <- readLn
    move       <- NotationParser.parseMove(moveString) match {
      case Right(move) => IO.pure(move)
      case Left(e)     => for {
        _ <- putStrLn(e)
        move <- readMove
      } yield move
    }
  } yield move

  private def putCurrentState(state: ChessState): IO[Unit] = {
    val board = state.board
    val piecePositions = 1 to 8 map (rowIndex => 1 to 8 flatMap(columnIndex => Position.createPiecePosition(rowIndex, columnIndex)))
    val pieces = piecePositions.toVector.map(row => row.toVector.map(pp => pieceToString(board.getPieceOnPosition(pp))))
    val piecesAndRows = pieces zip (1 to 8) map (tup => Vector(" " + tup._2.toString + " ") ++ tup._1)
    val columnTags = Vector("   ", " A ", " B ", " C ", " D ", " E ", " F ", " G ", " H ")
    val piecesAndRowsAndColumns = Vector(columnTags) ++ piecesAndRows
    piecesAndRowsAndColumns.reverse.map(row => putStrLn(row.combineAll)).combineAll
  }

  private def pieceToString(chessPiece: ChessPiece): String = chessPiece match {
    case Pawn(_, team, _) => teamToString(team) + "P "
    case Rook(_, team, _) => teamToString(team) + "R "
    case Knight(_, team) => teamToString(team) + "N "
    case Bishop(_, team) => teamToString(team) + "B "
    case Queen(_, team) => teamToString(team) + "Q "
    case King(_, team, _) => teamToString(team) + "K "
    case Empty(_ ,_) => "   "
  }

  private def teamToString(team: Team): String = team match {
    case White => "w"
    case Black => "b"
    case Nothing => ""
  }
}
