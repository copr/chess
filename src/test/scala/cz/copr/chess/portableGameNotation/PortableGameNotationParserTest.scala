package cz.copr.chess.portableGameNotation

import org.specs2.mutable.Specification

class PortableGameNotationParserTest extends Specification {
  "PortableGameNotationParser" should {
    "be able to parse png notation" in {
      val s = "[Event \"Aarhus Chess House GM Dec\"]\n[Site \"Aarhus DEN\"]\n[Date \"2019.12.17\"]\n[Round \"-\"]\n[White \"Ochsner,B\"]\n[Black \"Jacobsen,Mikkel Manosri\"]\n[Result \"1-0\"]\n[WhiteTitle \"IM\"]\n[BlackTitle \"FM\"]\n[Opening \"Sicilian\"]\n[Variation \"Alapin's variation (2.c3)\"]\n[BlackElo \"2348\"]\n[ECO \"B22\"]\n[EventDate \"2019.12.11\"]\n[WhiteElo \"2492\"]\n[BlackFideId \"1444514\"]\n[WhiteFideId \"1416928\"]\n\n1.e4 c5 2.c3 Nf6 3.e5 Nd5 4.Nf3 e6 5.Bc4 d6 6.d4 Nc6 7.O-O Be7 8.exd6 Qxd6\n9.dxc5 Qxc5 10.Qe2 Nb6 11.Bd3 Qh5 12.Nbd2 O-O 13.Re1 Bd7 14.Nf1 f6 15.Ng3\nQf7 16.b4 Rfe8 17.h4 Bf8 18.Ne4 h6 19.b5 Na5 20.c4 Rac8 21.c5 Bxc5 22.Nxc5\nRxc5 23.Qe4 Rec8 24.Qh7+ Kf8 25.Ba3 Nac4 26.Bb4 Nd6 27.Rac1 Na4 28.Bxc5\nNxc5 29.Bg6 Qg8 30.Qxg8+ Kxg8 31.Red1 Be8 32.Bxe8 Nxe8 33.Rd7 Nxd7 34.Rxc8\nKf7 35.Ra8 Nb6 36.Rxa7 Nd6 37.Ra3 e5 38.h5 Ke6 39.Nh4 Ne4 40.Rd3 Nd5 41.Nf5\n  1-0"
      PortableGameNotationParser.parsePng(s) match {
        case Right(value) => true
        case Left(msg) => {
          println(msg)
          false
        }
      }
    }

    "be able to parse order" in {
      val s = "10."
      PortableGameNotationParser.parse(PortableGameNotationParser.orderParser, s) match {
        case PortableGameNotationParser.Success(result, input) => {
          input.atEnd
        }
        case PortableGameNotationParser.NoSuccess(msg, _) => {
          println("Message follows: ")
          println(msg)
          false
        }
      }
    }
  }

  def printRest(rest: PortableGameNotationParser.Input): Unit = {
    if (!rest.atEnd) {
      println(rest.first)
      printRest(rest.rest)
    }
  }
}
