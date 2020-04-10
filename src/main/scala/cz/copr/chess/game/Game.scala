package cz.copr.chess.game

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import cz.copr.chess.chessLogic.{ChessAction, ChessActionInterpreter, ChessMove, ChessState, CreateInitialState, Move, Team}
import cz.copr.chess.inputOutput.{ConsoleInputOutputInterpreter, GetMove, InputOutput, PrintState}

object Game {
  type GameApp[A] = EitherK[InputOutput, ChessAction, A]

  val interpreter = ConsoleInputOutputInterpreter or ChessActionInterpreter

  class InputOutputs[F[_]](implicit I: InjectK[InputOutput, F]) {
    def printState(chessState: ChessState): Free[F, Unit] = Free.inject[InputOutput, F](PrintState(chessState))

    def getMove(team: Team): Free[F, Move] = Free.inject(GetMove(team))
  }

  object InputOutputs {
    implicit def interacts[F[_]](implicit I: InjectK[InputOutput, F]): InputOutputs[F] = new InputOutputs[F]
  }

  class ChessActions[F[_]](implicit I: InjectK[ChessAction, F]) {
    def createInitialState: Free[F, ChessState] = Free.inject[ChessAction, F](CreateInitialState)

    def chessMove(chessState: ChessState, move: Move): Free[F, ChessState.MoveResult] =
      Free.inject[ChessAction, F](ChessMove(chessState, move))
  }

  object ChessActions {
    implicit def chessActions[F[_]](implicit I: InjectK[ChessAction, F]): ChessActions[F] = new ChessActions[F]
  }


  def program(implicit I: InputOutputs[GameApp], CA: ChessActions[GameApp]): Free[GameApp, Unit] = {
    import I._, CA._

    for {
      state  <- createInitialState
      _      <- printState(state)
      move   <- getMove(state.team)
      result <- chessMove(state, move)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    val io = program.foldMap(interpreter)
    io.unsafeRunSync()
  }
}