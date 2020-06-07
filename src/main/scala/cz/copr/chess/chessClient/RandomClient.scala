package cz.copr.chess.chessClient

import cats.implicits._
import cats.effect.Sync
import cz.copr.chess.chessLogic.{ ChessLogic, ChessState, Move }

class RandomClient[F[_] : Sync](val random: scala.util.Random) extends ChessClient[F] {

  def getMove(gameState: ChessState): F[Move] = {
    val moves = ChessLogic.getAllPossibleMoves(gameState.switchTeam).toVector
    for {
      index <- Sync[F].delay(random.nextInt(moves.length))
      move   = moves(index)
    } yield move
  }

  def inform(msg: String): F[Unit] = Sync[F].delay(println(msg))
}
