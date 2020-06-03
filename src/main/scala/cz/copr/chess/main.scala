package cz.copr.chess

import cats.implicits._
import cats.mtl.implicits._
import cats.data.StateT
import cats.effect.{ ExitCode, IO, IOApp, SyncConsole }
import cz.copr.chess.chessClient.ConsoleClient
import cz.copr.chess.chessLogic.{ ChessLogic, ChessState }
import cz.copr.chess.game.Game
import cz.copr.chess.gameClient.LocalClient



object main extends IOApp {
  type LocalMonad[A] = StateT[IO, ChessState, A]

  def run(args: List[String]): IO[ExitCode] = {
    implicit val console = SyncConsole.stdio[LocalMonad]

    val initGame = ChessState.createInitialState

    val player1 = ConsoleClient[LocalMonad]
    val player2 = ConsoleClient[LocalMonad]

    val game = new Game[LocalMonad](player1, player2, LocalClient[LocalMonad])

    for {
      result <- game.playUntilTheEnd.run(initGame)
      _      <- IO(println(result._1.gameResult))
    } yield ExitCode.Success
  }
}
