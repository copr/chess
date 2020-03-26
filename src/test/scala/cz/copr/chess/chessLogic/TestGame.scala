package cz.copr.chess.chessLogic

import cats.implicits._

object TestGame {
  def playFromInitialState(moves: List[Move]): ChessState = playFromState(moves, ChessState.createInitialState)

  def playFromState(moves: List[Move], game: ChessState): ChessState =
    moves.foldLeft(game) {
      case (gameState, move) => ChessLogic.move(move, gameState) match {
        case Left(value) => throw new RuntimeException(move.toString + value)
        case Right(value) => value.copy(team = value.team.getOtherTeam)
      }
    }

  def doMoves(moves: List[Move], gameState: ChessState): ChessState.MoveResult =
    moves.foldLeft(gameState.asRight[IllegalMoveReason]) { case (game, move) => for {
        oldGame <- game
        newGame <- ChessLogic.move(move, oldGame)
      } yield newGame
    }


}
