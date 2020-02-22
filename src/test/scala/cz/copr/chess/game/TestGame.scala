package cz.copr.chess.game

import cats.implicits._

object TestGame {
  def play(moves: List[Move]): Game = {
    val z = Game.createInitialState
    moves.foldLeft(z) { case (gameState, move) => Game.move(move, gameState) match {
        case Left(value) => throw new RuntimeException(move.toString + value)
        case Right(value) => value.copy(team = value.team.getOtherTeam)
      }
    }
  }

  def playMoves(moves: List[Move], gameState: Game): Game.MoveResult =
    moves.foldLeft(gameState.asRight[IllegalMoveReason]) { case (game, move) => for {
        oldGame <- game
        newGame <- Game.move(move, oldGame)
      } yield newGame
    }


}
