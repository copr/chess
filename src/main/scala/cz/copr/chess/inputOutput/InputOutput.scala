package cz.copr.chess.inputOutput

import cats.free.Free

import cz.copr.chess.chessLogic.{ChessState, Move, Team}

sealed trait InputOutput[A]
final case class PrintState(gameState: ChessState) extends InputOutput[Unit]
final case class GetMove(team: Team) extends InputOutput[Move]

object InputOutput {
  def printState(gameState: ChessState): Free[InputOutput, Unit] = Free.liftF(PrintState(gameState))
  def getMove(team: Team): Free[InputOutput, Move] = Free.liftF(GetMove(team))
}


