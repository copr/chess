package cz.copr.chess.client

import cz.copr.chess.chessLogic.Move
import cz.copr.chess.server.{ GameMove, GameState, Player }

trait ChessClient[F[_]] {
  def startGame(oponentId: String): F[String]

  def play(move: Move): F[_]

  def getGameState(gameId: String): F[GameState]

  def register(player: Player): F[String]

  def getLastMove(gameId: String): F[GameMove]
}
