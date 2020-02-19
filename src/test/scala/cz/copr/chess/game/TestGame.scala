package cz.copr.chess.game

object TestGame {
  def play(moves: List[Move]): GameState = {
    val z: (GameState, Team) = (GameState.createInitialState, White)
    moves.foldLeft(z) { case ((gameState, team), move) => GameState.move(move, team, gameState) match {
        case Left(value) => throw new RuntimeException(move.toString + value)
        case Right(value) => (value, team.getOtherTeam)
      }
    }._1
  }

}
