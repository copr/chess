package cz.copr.chess.game

import cz.copr.chess.portableGameNotation.PortableGameNotationParser
import org.specs2.mutable.Specification

class GameStateTest extends Specification {

  "GameState" should {
    "state that the game is finished when it is finished" in {
      val s = "[Event \"Aarhus Chess House GM Dec\"]\n[Site \"Aarhus DEN\"]\n[Date \"2019.12.15\"]\n[Round \"-\"]\n[White \"Vasli,A\"]\n[Black \"Jacobsen,Mikkel Manosri\"]\n[Result \"0-1\"]\n[WhiteTitle \"FM\"]\n[BlackTitle \"FM\"]\n[Opening \"Caro-Kann\"]\n[Variation \"Panov-Botvinnik attack\"]\n[BlackElo \"2348\"]\n[ECO \"B13\"]\n[EventDate \"2019.12.11\"]\n[WhiteElo \"2413\"]\n[BlackFideId \"1444514\"]\n[WhiteFideId \"417521\"]\n\n1.e4 c6 2.d4 d5 3.exd5 cxd5 4.c4 Nf6 5.Nc3 Nc6 6.Bg5 Be6 7.a3 Qd7 8.Nf3\nNe4 9.cxd5 Bxd5 10.Be3 Nxc3 11.bxc3 g6 12.Be2 Bg7 13.O-O O-O 14.c4 Bxf3\n15.Bxf3 Nxd4 16.Bxd4 Bxd4 17.Rb1 Rab8 18.Rb5 e6 19.Qb1 Qe7 20.Rxb7 Rxb7\n21.Qxb7 Qxa3 22.Qc7 Qc3 23.Rd1 Bb6 24.Qf4 Qc2 25.Rf1 Rc8 26.Bb7 Qxc4 27.Qf3\nRc5 28.g3 Rf5 29.Qa3 Qe2 30.Kg2 Rxf2+ 31.Rxf2 Qxf2+ 32.Kh1 Qg1#   0-1"
      PortableGameNotationParser.parsePng(s) match {
        case Right(value) => {
          val game = TestGame.play(value.moves.map(_._1))
          ConsoleGame.putCurrentState(game).unsafeRunSync()
          game.finished
        }
        case Left(_) => false
      }
    }
  }

}
