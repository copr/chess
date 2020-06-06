package cz.copr.chess.portableGameNotation

import scala.util.parsing.combinator.RegexParsers

import cats.implicits._
import cz.copr.chess.chessLogic.Position.Position
import cz.copr.chess.chessLogic._

object NotationParser extends RegexParsers {
  private def char2BigPiece(c: Char): Option[PieceType] = c match {
    case 'N' => Some(KnightType)
    case 'R' => Some(RookType)
    case 'B' => Some(BishopType)
    case 'K' => Some(KingType)
    case 'Q' => Some(QueenType)
    case _   => None
  }


  private def file: Parser[Position] = """[a-h]""".r ^^ { s => fileToInt(s.head).head }

  private def fileToInt(c: Char): Option[Position] = c match {
    case 'a' => Position.createPosition(1)
    case 'b' => Position.createPosition(2)
    case 'c' => Position.createPosition(3)
    case 'd' => Position.createPosition(4)
    case 'e' => Position.createPosition(5)
    case 'f' => Position.createPosition(6)
    case 'g' => Position.createPosition(7)
    case 'h' => Position.createPosition(8)
    case _   => None
  }

  private def checkSymbol: Parser[String] = """[+#]+""".r

  private def rank: Parser[Position] = """[12345678]""".r ^^ { x => Position.createPosition(x.toInt).head }

  private def bigPiece: Parser[PieceType] = """[NBRQK]""".r ^^ { s => char2BigPiece(s.charAt(0)).get }

  private def promotionPiece: Parser[PieceType] = """[NBRQ]""".r ^^ { s => char2BigPiece(s.charAt(0)).get }

  private def promotion: Parser[PieceType] = for {
    _     <- """=""".r
    piece <- promotionPiece
  } yield piece

  private def smallCastling: Parser[Move] = for {
    _ <- """O-O""".r
    _ <- checkSymbol.?
  } yield SmallCastling

  private def bigCastling: Parser[Move] = for {
    _ <- """O-O""".r
    _ <- checkSymbol.?
  } yield BigCastling


  private def bigPieceCapture: Parser[Move] = for {
    bigP     <- bigPiece
    fromFile <- file.?
    fromRank <- rank.?
    _        <- """x""".r
    toFile   <- file
    toRank   <- rank
    _        <- checkSymbol.?
  } yield BigPieceCapture(bigP, fromFile, fromRank, toFile, toRank)

  private def bigPieceMove: Parser[Move] = bigPieceMoveVar1 | bigPieceMoveVar2

  private def bigPieceMoveVar1: Parser[Move] = for {
    bigP   <- bigPiece
    toFile <- file
    toRank <- rank
    _        <- checkSymbol.?
  } yield BigPieceMove(bigP, None, None, toFile, toRank)

  private def bigPieceMoveVar2: Parser[Move] = for {
    bigP     <- bigPiece
    fromFile <- file.?
    fromRank <- rank.?
    toFile   <- file
    toRank   <- rank
    _        <- checkSymbol.?
  } yield BigPieceMove(bigP, fromFile, fromRank, toFile, toRank)

  private def pawnMove: Parser[Move] = for {
    fromFile <- file
    toRank   <- rank
    pp       <- promotion.?
    _        <- checkSymbol.?
  } yield PawnMove(fromFile, toRank, pp)

  private def pawnCapture: Parser[Move] = for {
    fromFile <- file
    _        <- """x""".?
    toFile   <- file
    toRank   <- rank
    pp       <- promotion.?
    _        <- checkSymbol.?
  } yield PawnCapture(fromFile, toFile, toRank, pp)


  def moveParser: Parser[Move] = pawnCapture | pawnMove | bigPieceMove | bigPieceCapture | smallCastling | bigCastling

  def parseMove(move: String): Either[String, Move] = {
    parse(moveParser, move) match {
      case Success(result, next) => if (next.atEnd) result.asRight[String]
                                    else "There's some bogus in the notation".asLeft[Move]
      case _ => "Parsing move notation failed".asLeft[Move]
    }
  }
}
