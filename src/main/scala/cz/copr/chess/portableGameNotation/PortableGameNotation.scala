package cz.copr.chess.portableGameNotation

import cz.copr.chess.chessLogic.Move
import cz.copr.chess.portableGameNotation.PortableGameNotation.{Comment, Event, Player, Site}


sealed trait Result
case object WhiteWon extends Result
case object BlackWon extends Result
case object Draw     extends Result


case class PortableGameNotation(
  event: Event,
  site: Site,
  date: String,
  round: String,
  white: Player,
  black: Player,
  result: Result,
  optionalTags: Map[String, String],
  moves: List[(Move, Option[Comment])]
)

object PortableGameNotation {
  case class Event(value: String) extends AnyVal
  case class Site(value: String) extends AnyVal
  case class Player(firstName: String, lastName: String)

  type Comment = String
}
