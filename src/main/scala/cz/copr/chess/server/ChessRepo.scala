package cz.copr.chess.server

import java.util.UUID

import cats.implicits._
import cats.{ Id, Monad, MonadError }
import com.olegpy.meow.effects._
import cats.data.{ OptionT, ReaderT }
import cats.effect.{ IO, Sync }
import cats.effect.concurrent.Ref
import cats.mtl._
import cats.mtl.implicits._
import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.ChessRepo.{ MoveRequest, NewGameRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import io.circe.Encoder
import io.circe.generic.auto._
import monocle.macros.GenLens
import monocle.function.all._
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }
import org.joda.time.DateTime

trait ChessRepo[F[_]] {
  def register(registrationRequest: RegistrationRequest): F[PlayerId]

  def startGame(request: NewGameRequest): F[GameId]

  def getGames(playerId: PlayerId): F[List[GameState]]

  def move(moveRequest: MoveRequest): F[Unit]
}

trait IdGenerator[F[_]] {
  def getId: F[PlayerId]
}

case class MemoryRepoState(playerMap: Map[PlayerId, Player], gameStates: Map[GameId, GameState])

object MemoryRepoState {
  val playerMap  = GenLens[MemoryRepoState](_.playerMap)
  val gameStates = GenLens[MemoryRepoState](_.gameStates)

  val lastMoveTime = GenLens[GameState](_.lastMoveTime)
  def moves        = GenLens[GameState](_.moves)

  def setLastMoveTime(newMoveTime: DateTime)(gameState: GameState): GameState =
    lastMoveTime.set(Some(newMoveTime))(gameState)

  def addMove(newMove: MoveRequest)(gameState: GameState): GameState =
    moves.modify(_ ++ List(newMove))(gameState)

  def updateGameState(newMoveTime: DateTime, newMove: MoveRequest)(gameState: GameState): GameState = {
    val gameState1 = setLastMoveTime(newMoveTime)(gameState)
    addMove(newMove)(gameState1)
  }


  def addPlayer(player: Player)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (playerMap composeLens at(player.id)).set(Some(player))(memoryRepoState)

  def addGameState(gameState: GameState)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (gameStates composeLens at(gameState.gameId)).set(Some(gameState))(memoryRepoState)

  def getGameState(gameId: GameId)(memoryRepoState: MemoryRepoState): Option[GameState] =
    (gameStates composeLens at(gameId)).get(memoryRepoState)

  def setGameState(gameState: GameState)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (gameStates composeLens at(gameState.gameId)).set(Some(gameState))(memoryRepoState)

}

case class RegistrationRequest(
  nickname: String,
  firstName: Option[String],
  lastName: Option[String],
  email: Option[String]
)

object RegistrationRequest {
  implicit def registrationRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, RegistrationRequest] =
    jsonOf[F, RegistrationRequest]
}


class MemoryRepo[F[_]](implicit
                       M: Monad[F],
                       ME: MonadError[F, Throwable],
                       S: MonadState[F, MemoryRepoState]) extends ChessRepo[F] {
  import MemoryRepoState._

  def register(registrationRequest: RegistrationRequest): F[PlayerId] = for {
    player <- playerFromRequest(registrationRequest)
    _      <- S.modify(addPlayer(player))
  } yield player.id

  def startGame(request: NewGameRequest): F[GameId] = for {
    game <- gameStateFromGameRequest(request)
    _    <- S.modify(addGameState(game))
  } yield game.gameId

  def getGames(playerId: PlayerId): F[List[GameState]] = for {
    games <- S.get.map(_.gameStates.values.toList)
  } yield games.filter(game => game.playerId1 == playerId || game.playerId2 == playerId)

  def move(moveRequest: MoveRequest): F[Unit] = for {
    gameState   <- getGame(moveRequest)
    _           <- ensureCorrectOrder(gameState, moveRequest)
    date        <- DateTime.now().pure[F]
    newGameState = updateGameState(date, moveRequest)(gameState)
    _           <- S.modify(setGameState(newGameState))
  } yield ()

  private def getGame(moveRequest: MoveRequest): F[GameState] = {
    OptionT(S.get.map(getGameState(moveRequest.gameId)))
      .toRight(new IllegalArgumentException("Game not found"))
      .rethrowT
  }

  private def ensureCorrectOrder(game: GameState, moveRequest: MoveRequest): F[Unit] =
    if (game.moves.last.playerId == moveRequest.playerId) {
      ME.raiseError(new IllegalArgumentException("It's the other player's turn"))
    }else {
      ().pure[F]
    }

  private def playerFromRequest(registrationRequest: RegistrationRequest): F[Player] = for {
    playerId  <- UUID.randomUUID().pure[F].map(uuid => PlayerId(uuid.toString))
  } yield Player(playerId,
    registrationRequest.nickname,
    registrationRequest.firstName,
    registrationRequest.lastName,
    registrationRequest.email)

  private def gameStateFromGameRequest(newGameRequest: NewGameRequest): F[GameState] = for {
    gameId <- UUID.randomUUID().pure[F].map(uuid => GameId(uuid.toString))
  } yield GameState(gameId,
    newGameRequest.player1Id,
    newGameRequest.player2Id,
    Ongoing,
    None,
    List()
  )
}

object MemoryRepo {

  def main(args: Array[String]): Unit = {

    val res = for {
      ref <- Ref.of[IO, MemoryRepoState](MemoryRepoState(Map(), Map()))
      out <- ref.runState(implicit monadState => {
        new MemoryRepo[IO].register(RegistrationRequest("copr", None, None, None))
      })
      map <- ref.get
      _   <- IO(println(map))
    } yield out
    println(res.unsafeRunSync())
  }
}


object ChessRepo {
  def apply[F[_]](implicit repo: ChessRepo[F]): ChessRepo[F] = repo

  
  sealed trait GameVariant
  case object WhiteVsBlack extends GameVariant
  case object BlackVsWhite extends GameVariant

  case class NewGameRequest(player1Id: PlayerId, player2Id: PlayerId, variant: GameVariant)

  case class MoveRequest(playerId: PlayerId, gameId: GameId, move: Move)

  implicit def newGameRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, NewGameRequest] =
    jsonOf[F, NewGameRequest]

  implicit def moveRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, MoveRequest] =
    jsonOf[F, MoveRequest]

  implicit def listEntityEncoder[F[_] : Sync, A](implicit E: Encoder[A]): EntityEncoder[F, List[A]] = jsonEncoderOf[F, List[A]]
}
