package cz.copr.chess.server

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._


object Main extends IOApp {
  def run(args: List[String]) =
    ChessServer.stream[IO].compile.drain.as(ExitCode.Success)
}
