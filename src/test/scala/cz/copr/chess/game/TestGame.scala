package cz.copr.chess.game

object TestGame {
  def play(moves: List[Move]): GameState = {
    val z = GameState.createInitialState
    moves.foldLeft(z) { case (gameState, move) => GameState.move(move, gameState) match {
        case Left(value) => throw new RuntimeException(move.toString + value)
        case Right(value) => value.copy(team = value.team.getOtherTeam)
      }
    }
  }

}
