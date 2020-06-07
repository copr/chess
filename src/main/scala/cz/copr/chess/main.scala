package cz.copr.chess

import scala.util.Random

import cats.data.StateT
import cats.effect.{ ExitCode, IO, IOApp, SyncConsole }
import cats.mtl.implicits._
import cz.copr.chess.chessClient.{ ConsoleClient, RandomClient }
import cz.copr.chess.chessLogic.ChessState
import cz.copr.chess.game.Game
import cz.copr.chess.gameClient.LocalClient



object main extends IOApp {
  type LocalMonad[A] = StateT[IO, ChessState, A]

  def run(args: List[String]): IO[ExitCode] = {
    implicit val console = SyncConsole.stdio[LocalMonad]

    val initGame = ChessState.createInitialState

    for {
      random <- IO.delay(new Random)
      player1 = ConsoleClient[LocalMonad]
      player2 = new RandomClient[LocalMonad](random)
      game    = new Game[LocalMonad](player1, player2, LocalClient[LocalMonad])
      result <- game.playUntilTheEnd.run(initGame)
      _      <- IO(println(result._1.gameResult))
    } yield ExitCode.Success
  }
}
