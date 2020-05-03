package cz.copr.chess.server

import cats.implicits._
import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.mtl.implicits._
import cz.copr.chess.chessLogic.SmallCastling
import cz.copr.chess.server.ChessRepo.{ FinishGameRequest, MoveRequest, NewGameRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import org.specs2.matcher
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class ChessRepoTest extends Specification with matcher.ResultMatchers {

  type TestMonad[A] = StateT[IO, MemoryRepoState, A]

  val testRepo: ChessRepo[TestMonad] = new MemoryRepo[TestMonad]
  val initState = MemoryRepoState(Map(), Map())


  "ChessRepo" should {
    "return unit when players play in the correct order" in {
      val testProgram = for {
        (whitePlayerId, blackPlayerId, gameId) <- startGame(testRepo)
        _ <- testRepo.move(MoveRequest(whitePlayerId, gameId, SmallCastling))
        _ <- testRepo.move(MoveRequest(blackPlayerId, gameId, SmallCastling))
      } yield ()

      val result = testProgram.run(initState).attempt.unsafeRunSync()

      result must beRight[(MemoryRepoState, Unit)].like({ case (_, ()) => true})
    }

    "return illegal argument exception when player DON'T play in the correct order" in {
      val testProgram = for {
        (whitePlayerId, blackPlayerId, gameId) <- startGame(testRepo)
        _ <- testRepo.move(MoveRequest(blackPlayerId, gameId, SmallCastling))
        _ <- testRepo.move(MoveRequest(whitePlayerId, gameId, SmallCastling))
      } yield ()

      val result = testProgram.run(initState).attempt.unsafeRunSync()

      result must beLeft[Throwable].like({ case _: IllegalArgumentException => true})
    }

    "return gameState that is finished when one player says the game is finished" in {
      val testProgram = for {
        (whitePlayerId, _, gameId) <- startGame(testRepo)
        _         <- testRepo.finish(FinishGameRequest(whitePlayerId, gameId, WhiteWon))
        gameState <- testRepo.getGame(gameId)
      } yield gameState

      val result = testProgram.run(initState).unsafeRunSync()

      result._2.gameStatus === WhiteWon
    }

    "return an error when one player sets the result of a game and the other one tries to set a different result" in {
      val testProgram = for {
        (whitePlayerId, blackPlayerId, gameId) <- startGame(testRepo)
        _         <- testRepo.finish(FinishGameRequest(whitePlayerId, gameId, WhiteWon))
        _         <- testRepo.finish(FinishGameRequest(blackPlayerId, gameId, BlackWon))
        gameState <- testRepo.getGame(gameId)
      } yield gameState

      val result = testProgram.run(initState).attempt.unsafeRunSync()

      result must beLeft[Throwable].like({ case _: IllegalArgumentException => true})
    }

    "return an error when one player sets the result of a game and the same one tries to set a different result" in {
      val testProgram = for {
        (whitePlayerId, blackPlayerId, gameId) <- startGame(testRepo)
        _         <- testRepo.finish(FinishGameRequest(whitePlayerId, gameId, WhiteWon))
        _         <- testRepo.finish(FinishGameRequest(whitePlayerId, gameId, BlackWon))
        gameState <- testRepo.getGame(gameId)
      } yield gameState

      val result = testProgram.run(initState).attempt.unsafeRunSync()

      result must beLeft[Throwable].like({ case _: IllegalArgumentException => true})
    }
  }

  private def startGame[F[_] : Monad](repo: ChessRepo[F]): F[(PlayerId, PlayerId, GameId)] = {
    val whitePlayerRequest = RegistrationRequest("white", None, None, None)
    val blackPlayerRequest = RegistrationRequest("black", None, None, None)
    for {
      whitePlayerId <- repo.register(whitePlayerRequest)
      blackPlayerId <- repo.register(blackPlayerRequest)
      newGameRequest = NewGameRequest(whitePlayerId, blackPlayerId)
      gameId        <- repo.startGame(newGameRequest)
    } yield (whitePlayerId, blackPlayerId, gameId)
  }

}
