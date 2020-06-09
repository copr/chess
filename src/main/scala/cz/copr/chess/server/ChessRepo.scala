package cz.copr.chess.server

import java.util.UUID

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.mtl._
import cats.{ Monad, MonadError }
import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.ChessRepo.{ FinishGameRequest, MoveRequest, NewGameRequest }
import cz.copr.chess.server.GameState.GameId
import cz.copr.chess.server.Player.PlayerId
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import monocle.function.all._
import monocle.macros.GenLens
import org.http4s.circe._
import org.http4s.{ EntityDecoder, EntityEncoder }
import org.joda.time.DateTime

trait ChessRepo[F[_]] {
  def getPlayers: F[List[Player]]

  def register(registrationRequest: RegistrationRequest): F[PlayerId]

  def startGame(request: NewGameRequest): F[GameId]

  def getGames(playerId: PlayerId): F[List[GameState]]

  def getGame(gameId: GameId): F[GameState]

  def move(moveRequest: MoveRequest): F[Unit]

  def finish(finishGameRequest: FinishGameRequest): F[Unit]
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
  val gameStatus   = GenLens[GameState](_.gameStatus)

  def setLastMoveTime(newMoveTime: DateTime)(gameState: GameState): GameState =
    lastMoveTime.set(Some(newMoveTime))(gameState)

  def addMove(newMove: MoveRequest)(gameState: GameState): GameState =
    moves.modify(_ ++ List(newMove))(gameState)

  def updateGameState(newMoveTime: DateTime, newMove: MoveRequest)(gameState: GameState): GameState = {
    // TODO: podivat se jak se to composituje spravne tema lensama
    val gameState1 = setLastMoveTime(newMoveTime)(gameState)
    val gameState2 = gameStatus.modify({
      case WhiteTurn     => BlackTurn
      case BlackTurn     => WhiteTurn
      case s: GameStatus => s
    })(gameState1)
    addMove(newMove)(gameState2)
  }


  def addPlayer(player: Player)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (playerMap composeLens at(player.id)).set(Some(player))(memoryRepoState)

  def addGameState(gameState: GameState)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (gameStates composeLens at(gameState.gameId)).set(Some(gameState))(memoryRepoState)

  def getGameState(gameId: GameId)(memoryRepoState: MemoryRepoState): Option[GameState] =
    (gameStates composeLens at(gameId)).get(memoryRepoState)

  def setGameState(gameState: GameState)(memoryRepoState: MemoryRepoState): MemoryRepoState =
    (gameStates composeLens at(gameState.gameId)).set(Some(gameState))(memoryRepoState)

  def gameStateLens(gameId: GameId) =
    (gameStates composeLens at(gameId))
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

  implicit def registrationRequestEntityEncoder[F[_] : Sync]: EntityEncoder[F, RegistrationRequest] =
    jsonEncoderOf[F, RegistrationRequest]
}

// TODO: mozna predelat monad na sync?
class MemoryRepo[F[_]](implicit
                       M: Monad[F],
                       ME: MonadError[F, Throwable],
                       S: MonadState[F, MemoryRepoState]) extends ChessRepo[F] {
  import MemoryRepoState._

  def getPlayers: F[List[Player]] = for {
    state <- S.get
  } yield playerMap.get(state).values.toList

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
  } yield games.filter(game => game.whitePlayer == playerId || game.blackPlayer == playerId)

  def getGame(gameId: GameId): F[GameState] = getGameFromState(gameId)

  def move(moveRequest: MoveRequest): F[Unit] = for {
    gameState    <- getGameFromState(moveRequest.gameId)
    _            <- ensureGameOngoing(gameState)
    _            <- ensureCorrectOrder(moveRequest)
    date         <- DateTime.now().pure[F]
    newGameState  = updateGameState(date, moveRequest)(gameState)
    _            <- S.modify(setGameState(newGameState))
  } yield ()

  def finish(finishGameRequest: FinishGameRequest): F[Unit] = for {
    gameState        <- getGameFromState(finishGameRequest.gameId)
    updatedGameState <- updateGameStatus(finishGameRequest, gameState)
    _                <- S.modify(setGameState(updatedGameState))
  } yield ()

  private def getGameFromState(gameId: GameId): F[GameState] =
    OptionT(S.get.map(getGameState(gameId)))
      .toRight(new IllegalArgumentException("Game not found"))
      .rethrowT

  private def ensureGameOngoing(gameState: GameState): F[Unit] =
    if (gameState.gameStatus == WhiteTurn || gameState.gameStatus == BlackTurn) {
      ().pure[F]
    } else {
      ME.raiseError(new IllegalArgumentException("Game already finished"))
    }

  private def ensureCorrectOrder(moveRequest: MoveRequest): F[Unit] = (for {
    gameState <- OptionT.liftF[F, GameState](getGameFromState(moveRequest.gameId))
    _         <- OptionT.fromOption[F](gameState.gameStatus match {
      case WhiteTurn => Some(gameState.whitePlayer)
      case BlackTurn => Some(gameState.blackPlayer)
      case _         => None
    }).ensure(new IllegalArgumentException(s"This is not player ${moveRequest.playerId} turn"))(_ == moveRequest.playerId)
  } yield ()).getOrElse(ME.raiseError(new IllegalArgumentException(s"This is not player ${moveRequest.playerId} turn")))


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
    newGameRequest.whitePlayer,
    newGameRequest.blackPlayer,
    WhiteTurn,
    None,
    List()
  )

  private def updateGameStatus(finishGameRequest: FinishGameRequest, gameState: GameState): F[GameState] = {
    val newGameStatus = finishGameRequest.result
    val currentStatus = gameState.gameStatus
    currentStatus match {
      case WhiteTurn => gameStatus.set(newGameStatus)(gameState).pure[F]
      case BlackTurn => gameStatus.set(newGameStatus)(gameState).pure[F]
      case status if status == newGameStatus => gameState.pure[F]
      case _ => ME.raiseError[GameState](
        new IllegalArgumentException("Result you are trying to set is different from the existing one")
      )
    }
  }
}

object ChessRepo {
  import GameState._

  def apply[F[_]](implicit repo: ChessRepo[F]): ChessRepo[F] = repo

  final case class NewGameRequest(whitePlayer: PlayerId, blackPlayer: PlayerId)

  sealed trait PlayRequest
  final case class MoveRequest(playerId: PlayerId, gameId: GameId, move: Move) extends PlayRequest
  // TODO: predelat GameStatus na neco omezenjsiho co povoluje jen WhiteWin, BlackWin a Draw
  final case class FinishGameRequest(playerId: PlayerId, gameId: GameId, result: GameStatus) extends PlayRequest

  implicit def newGameRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, NewGameRequest] =
    jsonOf[F, NewGameRequest]

  implicit def newGameRequestEntityEncoder[F[_]: Sync]: EntityEncoder[F, NewGameRequest] =
    jsonEncoderOf[F, NewGameRequest]

  implicit def moveRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, MoveRequest] =
    jsonOf[F, MoveRequest]

  implicit def moveRequestEntityEncoder[F[_] : Sync]: EntityEncoder[F, MoveRequest] =
    jsonEncoderOf[F, MoveRequest]

  implicit def finishGameRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, FinishGameRequest] =
    jsonOf[F, FinishGameRequest]

  implicit def finishGameRequestEntityEncoder[F[_] : Sync]: EntityEncoder[F, FinishGameRequest] =
    jsonEncoderOf[F, FinishGameRequest]

  implicit def playRequestEntityDecoder[F[_] : Sync]: EntityDecoder[F, PlayRequest] =
    jsonOf[F, PlayRequest]

  implicit def playRequestEntityEncoder[F[_] : Sync]: EntityEncoder[F, PlayRequest] =
    jsonEncoderOf[F, PlayRequest]

  implicit def listEntityEncoder[F[_] : Sync, A](implicit E: Encoder[A]): EntityEncoder[F, List[A]] = jsonEncoderOf[F, List[A]]
}
