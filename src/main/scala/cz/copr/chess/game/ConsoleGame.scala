package cz.copr.chess.game

import cats._
import cats.data.EitherT
import cats.implicits._
import cats.effect.IO
import cats.effect.Console.io._

object ConsoleGame {
  def main(args: Array[String]): Unit = {
    program(GameState.createInitialState, White).unsafeRunSync()
  }

  def program(gameState: GameState, team: Team): IO[Unit] = for {
    newState <- turn(gameState, team).value
    _ <- newState match {
      case Right(tup) => if (tup._1.finished) {
        for {
          _ <- putCurrentState(tup._1)
          _ <- putStrLn("Timto konci bal")
        } yield ()
      } else {
        program(tup._1, team.getOtherTeam)
      }
      case Left(s) => for {
        _ <- putStrLn(s)
        _ <- program(gameState, team)
      } yield ()
    }
  } yield ()

  def turn(gameState: GameState, team: Team): EitherT[IO, String, (GameState, Team)] = for {
    _          <- EitherT.liftF[IO, String, Unit](putStrLn("Current team " + team.toString))
    _          <- EitherT.liftF[IO, String, Unit](putStrLn("Current state:"))
    _          <- EitherT.liftF[IO, String, Unit](putCurrentState(gameState))
    _          <- EitherT.liftF[IO, String, Unit](putStrLn("State your move:"))
    moveString <- EitherT.liftF[IO, String, String](readLn)
    move       <- EitherT[IO, String, Move](NotationParser.parseMove(moveString).pure[IO])
    newState   <- EitherT[IO, String, GameState](GameState.move(move, team, gameState).leftMap(_.toString).pure[IO])
  } yield (newState, team.getOtherTeam)

  def putCurrentState(state: GameState): IO[Unit] = {
    val board = state.board
    val piecePositions = 1 to 8 map (rowIndex => 1 to 8 flatMap(columnIndex => Position.createPiecePosition(rowIndex, columnIndex)))
    val pieces = piecePositions.toVector.map(row => row.toVector.map(pp => pieceToString(board.getPieceOnPosition(pp))))
    val piecesAndRows = pieces zip (1 to 8) map (tup => Vector(" " + tup._2.toString + " ") ++ tup._1)
    val columnTags = Vector("   ", " A ", " B ", " C ", " D ", " E ", " F ", " G ", " H ")
    val piecesAndRowsAndColumns = Vector(columnTags) ++ piecesAndRows
    piecesAndRowsAndColumns.reverse.map(row => putStrLn(row.combineAll)).combineAll
  }

  def pieceToString(chessPiece: ChessPiece): String = chessPiece match {
    case Pawn(_, team, _) => teamToString(team) + "P "
    case Rook(_, team, _) => teamToString(team) + "R "
    case Knight(_, team) => teamToString(team) + "N "
    case Bishop(_, team) => teamToString(team) + "B "
    case Queen(_, team) => teamToString(team) + "Q "
    case King(_, team, _) => teamToString(team) + "K "
    case Empty(_ ,_) => "   "
  }

  def teamToString(team: Team): String = team match {
    case White => "w"
    case Black => "b"
    case Nothing => ""
  }
}
