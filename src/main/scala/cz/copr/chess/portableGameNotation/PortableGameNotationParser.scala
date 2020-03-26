package cz.copr.chess.portableGameNotation

import cats.implicits._
import cz.copr.chess.chessLogic.Move
import cz.copr.chess.portableGameNotation.PortableGameNotation.{Comment, Event, Player, Site}

import scala.util.parsing.combinator.RegexParsers

object PortableGameNotationParser extends RegexParsers{
  private def string: Parser[String] = """[a-zA-Z]+""".r

  private def quote: Parser[String] = """"""".r

  private def openingBracket: Parser[String] = """\[""".r

  private def closingBracket: Parser[String] = """\]""".r

  private def whitespace: Parser[String] = """[ \\n]*""".r

  private def stringAndWhiteSpace: Parser[String] = """[a-zA-Z ]+""".r

  private def anythingExceptQuote: Parser[String] = """[^"]+""".r

  private def date: Parser[String] = """[\d\.]+""".r

  private def round: Parser[String] = """[\d-]+""".r


  private def generalTagParser[A](tagName: Parser[String], valueParser: Parser[A]): Parser[(String, A)] = for {
    _        <- openingBracket
    name     <- tagName
    _        <- quote
    tagValue <- valueParser
    _        <- quote
    _        <- closingBracket
    _        <- whiteSpace.?
  } yield (name, tagValue)

  private def tagParser[A](tagName: Parser[String], valueParser: Parser[A]): Parser[A] =
    generalTagParser(tagName, valueParser) ^^ { tup => tup._2 }

  private def eventParser: Parser[Event] = tagParser("Event", stringAndWhiteSpace ^^ { s => Event(s)})

  private def siteParser: Parser[Site] = tagParser("Site", stringAndWhiteSpace ^^ { s => Site(s)})

  private def dateParser: Parser[String] = tagParser("Date", date)

  private def roundParser: Parser[String] = tagParser("Round", round)

  private def playerParser: Parser[Player] = for {
    lastName  <- string
    _         <- ",".r
    _         <- whitespace
    firstName <- stringAndWhiteSpace
  } yield Player(firstName, lastName)

  private def whiteParser: Parser[Player] = tagParser("White", playerParser)

  private def blackParser: Parser[Player] = tagParser("Black", playerParser)

  private def whiteWonParser: Parser[Result] = """1-0""" ^^ { _ => WhiteWon}

  private def blackWonParser: Parser[Result] = """0-1""" ^^ { _ => BlackWon}

  private def drawParser: Parser[Result] = """1/2-1/2""" ^^ { _ => Draw}

  private def result: Parser[Result] = whiteWonParser | blackWonParser | drawParser

  private def resultParser: Parser[Result] = tagParser("Result", result)

  private def optionalTagsParser: Parser[Map[String, String]] = rep(generalTagParser(string, anythingExceptQuote)) ^^ {
    list =>
      val z = Map(): Map[String, String]
      list.foldLeft(z)((map, tuple) => map + tuple)
  }

  private def commentParser: Parser[Comment] = for {
    _       <- """\{""".r
    comment <- stringAndWhiteSpace
    _       <- """\}""".r
  } yield comment

  def dot: Parser[String] = "."

  def orderParser: Parser[Int] = for {
    order <- """[0-9]+""".r ^^ { _.toInt }
    _     <- dot
  } yield order

  private def moveParser: Parser[Move] = NotationParser.moveParser.asInstanceOf[Parser[Move]]

  private def movesParser: Parser[List[(Move, Option[Comment])]] = {
    val move = for {
      _       <- whiteSpace.?
      _       <- orderParser.?
      _       <- whitespace
      move    <- moveParser
      _       <- whitespace
      comment <- commentParser.?
      _       <- whitespace
    } yield move -> comment
    for {
      moves <- rep1(move)
      _     <- result
    } yield moves
  }

  def pngParser: Parser[PortableGameNotation] = for {
    _            <- whitespace.withFailureMessage("White space 1 parsing failed")
    event        <- eventParser.withFailureMessage("Event parsing failed")
    site         <- siteParser.withFailureMessage("Site parsing failed")
    date         <- dateParser.withFailureMessage("Date parsing failed")
    round        <- roundParser.withFailureMessage("Round parsing failed")
    whitePlayer  <- whiteParser.withFailureMessage("White parsing failed")
    blackPlayer  <- blackParser.withFailureMessage("Black parsing failed")
    result       <- resultParser.withFailureMessage("Result parsing failed")
    optionalTags <- optionalTagsParser.withFailureMessage("Optional tags parsing failed")
    _            <- whiteSpace.withFailureMessage("White space 2 parsing failed").?
    moves        <- movesParser
  } yield PortableGameNotation(
    event,
    site,
    date,
    round,
    whitePlayer,
    blackPlayer,
    result,
    optionalTags,
    moves
  )

  def parsePng(s: String): Either[String, PortableGameNotation] = {
    parse(pngParser, s) match {
      case Success(result, _) => result.asRight[String]
      case NoSuccess(str, rest)  => (str + "rest: " + rest).asLeft[PortableGameNotation]
    }
  }

  def parsePngs(s: String): Either[String, List[PortableGameNotation]] = {
    parse(rep1(pngParser), s) match {
      case Success(result, _) => result.asRight[String]
      case NoSuccess(str, rest)  => (str + "rest: " + rest).asLeft[List[PortableGameNotation]]
    }
  }
}
