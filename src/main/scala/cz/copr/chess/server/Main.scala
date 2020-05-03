package cz.copr.chess.server

import scala.concurrent.ExecutionContext.global

import cats.effect.concurrent.Ref
import cats.effect.{ ExitCode, IO, IOApp }
import com.olegpy.meow.effects._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder


object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    ref <- Ref[IO].of[MemoryRepoState](MemoryRepoState(Map(), Map()))
    res <- ref.runState { implicit monadState => {
      val repo = new MemoryRepo[IO]
      BlazeServerBuilder[IO](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(ChessRoutes.chessRoutes(repo).orNotFound)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }}
  } yield res
}
