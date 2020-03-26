package cz.copr.chess.chessLogic

import cats.data.State
import cz.copr.chess.chessLogic.Position.PiecePosition


sealed trait GameResult {
  def isFinished: Boolean = this match {
    case Ongoing => false
    case _       => true
  }
}

case object WhiteWon extends GameResult
case object BlackWon extends GameResult
case object Draw     extends GameResult
case object Ongoing  extends GameResult

case class ChessState(board: ChessBoard, team: Team, gameResult: GameResult, moves: List[Move])

object ChessState {
  type Result[A] = Either[IllegalMoveReason, A]
  type MoveResult = Result[ChessState]


  def createInitialState: ChessState = {
    val init: ChessBoard = ChessBoard(Map())
    val addPieces = for {
      _ <- putPawns
      _ <- putRooks
      _ <- putKnights
      _ <- putBishops
      _ <- putQueens
      _ <- putKings
    } yield ()
    ChessState(addPieces.run(init).value._1, team = White, gameResult = Ongoing, moves = List())
  }

  def putPieces(columns: Vector[Int], row: Int)(f: (PiecePosition, Team) => ChessPiece): State[ChessBoard, Unit] = for {
    board <- State.get
    whitePieces = columns.flatMap(column => Position.createPiecePosition(row, column)).map(f(_, White))
    blackPieces = columns.flatMap(column => Position.createPiecePosition(9 - row, column)).map(f(_, Black))
    newBoard = (whitePieces ++ blackPieces).foldLeft(board)((b, p) => b.put(p, p.position))
    _ <- State.set(newBoard)
  } yield ()

  def putPawns: State[ChessBoard, Unit] =
    putPieces(1 to 9 toVector, 2)((pp, t) => Pawn(pp, t, enpasantable = false))

  def putRooks: State[ChessBoard, Unit] =
    putPieces(Vector(1, 8), 1)((pp, t) => Rook(pp, t, moved = false))

  def putKnights: State[ChessBoard, Unit] =
    putPieces(Vector(2, 7), 1)((pp, t) => Knight(pp, t))

  def putBishops: State[ChessBoard, Unit] =
    putPieces(Vector(3, 6), 1)((pp, t) => Bishop(pp, t))

  def putQueens: State[ChessBoard, Unit] =
    putPieces(Vector(4), 1)((pp, t) => Queen(pp, t))

  def putKings: State[ChessBoard, Unit] =
    putPieces(Vector(5), 1)((pp, t) => King(pp, t, moved = false))
}
