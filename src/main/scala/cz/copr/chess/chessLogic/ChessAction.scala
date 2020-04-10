package cz.copr.chess.chessLogic

import cats.effect.IO
import cats.implicits._
import cats.free.Free
import cats.~>

sealed trait ChessAction[A]
case object CreateInitialState extends ChessAction[ChessState]
final case class ChessMove(chessState: ChessState, move: Move) extends ChessAction[ChessState.MoveResult]

object ChessAction {
  def createInitialState: Free[ChessAction, ChessState] = Free.liftF(CreateInitialState)
  def move(chessState: ChessState, move: Move): Free[ChessAction, ChessState.MoveResult] =
    Free.liftF(ChessMove(chessState, move))
}

object ChessActionInterpreter extends (ChessAction ~> IO) {
  def apply[A](chessAction: ChessAction[A]): IO[A] = chessAction match {
    case CreateInitialState => ChessState.createInitialState.pure[IO]
    case ChessMove(chessState, move) => ChessLogic.move(move, chessState).pure[IO]
  }
}