package cz.copr.chess.chessClient

import cats.effect.{ Console, Sync }
import cats.implicits._
import cz.copr.chess.chessLogic.{ Bishop, Black, ChessPiece, ChessState, Empty, King, Knight, Move, Nothing, Pawn, Position, Queen, Rook, Team, White }
import cz.copr.chess.portableGameNotation.NotationParser


object ConsoleClient {
  def apply[F[_] : Sync](implicit C: Console[F]): ChessClient[F] = new ChessClient[F] {
    def getMove(chesState: ChessState): F[Move] = for {
      _    <- putCurrentState(chesState)
      move <- readMove[F]
    } yield move

    def inform(msg: String): F[Unit] = C.putStrLn(msg)
  }


  private def readMove[F[_] : Sync](implicit C: Console[F]): F[Move] =  for {
    _          <- C.putStrLn("State your move:")
    moveString <- C.readLn
    move       <- NotationParser.parseMove(moveString) match {
      case Right(move) => move.pure[F]
      case Left(e)     => for {
        _    <- C.putStrLn(e)
        move <- readMove[F]
      } yield move
    }
  } yield move

  private def putCurrentState[F[_] : Sync](state: ChessState)(implicit C: Console[F]): F[Unit] = {
    val board = state.board
    val piecePositions = 1 to 8 map (rowIndex => 1 to 8 flatMap(columnIndex => Position.createPiecePosition(rowIndex, columnIndex)))
    val pieces = piecePositions.toVector.map(row => row.toVector.map(pp => pieceToString(board.getPieceOnPosition(pp))))
    val piecesAndRows = pieces zip (1 to 8) map (tup => Vector(" " + tup._2.toString + " ") ++ tup._1)
    val columnTags = Vector("   ", " A ", " B ", " C ", " D ", " E ", " F ", " G ", " H ")
    val piecesAndRowsAndColumns = Vector(columnTags) ++ piecesAndRows
    piecesAndRowsAndColumns.reverse.map(row => C.putStrLn(row.combineAll)).sequence_
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
