package cz.copr.chess.server

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global


case object Client extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val res = BlazeClientBuilder[IO](global).resource.use({ client =>
      client.expect[String]("http://localhost:8080/joke")
    }).map(s => ExitCode(s.length))
    res
  }
}
