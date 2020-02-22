package cz.copr.chess.game

import cats._
import cats.implicits._
import cats.data.State

import cz.copr.chess.game.Position.PiecePosition
import ChessPiece._


case class GameState(board: ChessBoard, finished: Boolean)


object GameState {
  type Result[A] = Either[IllegalMoveReason, A]
  type MoveResult = Result[GameState]
  type PieceSearchResult = Result[ChessPiece]

  def move(move: Move, team: Team, game: GameState): MoveResult = {
    val cleareEnpasantableState = clearEnpasantable(game, team)
    val stateAfterMove = move match {
      case SmallCastling        => smallCastling(cleareEnpasantableState, team)
      case BigCastling          => bigCastling(cleareEnpasantableState, team)
      case bpm: BigPieceMove    => bigPieceMove(bpm, team, cleareEnpasantableState)
      case bpc: BigPieceCapture => bigPieceCapture(bpc, team, cleareEnpasantableState)
      case pc: PawnCapture      => pawnCapture(pc, team, cleareEnpasantableState)
      case pm: PawnMove         => pawnMove(pm, team, cleareEnpasantableState)
    }
    stateAfterMove.map(gs => isCheckMate(gs, team))
  }

  def bigCastling(gameState: GameState, team: Team): MoveResult = for {
    king <- gameState.board
      .getKing(team)
      .headOption
      .toRight(CouldNotFindThePiece(KingType))
      .filterOrElse(!_.moved, CastlingIllegal("King has already moved"))
    xPos = if (team == White) 1 else 8
    rook <- gameState.board
      .getRooks(team)
      .find(x => x.position.y.value == 1 && x.position.x.value == xPos)
      .toRight(CastlingIllegal("Rook is not in the right position"))
      .filterOrElse(!_.moved, CastlingIllegal("Rook has already moved"))
    newKingsPosition <- Position.createPiecePosition(xPos, 3).toRight(LogicError)
    newRooksPosition <- Position.createPiecePosition(xPos, 4).toRight(LogicError)
    rooksGoThrouPos  <- Position.createPiecePosition(xPos, 2).toRight(LogicError)
    isKingsPositionFree = gameState.board.getPieceOnPosition(newKingsPosition).isInstanceOf[Empty]
    isRooksPositionFree = gameState.board.getPieceOnPosition(newRooksPosition).isInstanceOf[Empty]
    isRooksGoThrouhFree = gameState.board.getPieceOnPosition(rooksGoThrouPos).isInstanceOf[Empty]
    canKingBeChecked = isCheck(gameState.board.move(king, newRooksPosition), team) ||
      isCheck(gameState.board.move(king, newKingsPosition), team)
    newState <- if (isKingsPositionFree && isRooksPositionFree && !canKingBeChecked && isRooksGoThrouhFree) {
      GameState(gameState.board
        .move(king, newKingsPosition)
        .move(rook, newRooksPosition),
        finished = false
      ).asRight
    } else {
      CastlingIllegal("King would be checked by castling or Rook can't move to position").asLeft
    }
  } yield newState

  def smallCastling(gameState: GameState, team: Team): MoveResult = for {
    king <- gameState.board
      .getKing(team)
      .headOption
      .toRight(CouldNotFindThePiece(KingType))
      .filterOrElse(!_.moved, CastlingIllegal("King has already moved"))
    xPos = if (team == White) 1 else 8
    rook <- gameState.board
      .getRooks(team)
      .find(x => x.position.y.value == 8 && x.position.x.value == xPos)
      .toRight(CastlingIllegal("Rook is not in the right place"))
      .filterOrElse(!_.moved, CastlingIllegal("Rook has already moved"))
    newKingsPosition <- Position.createPiecePosition(xPos, 7).toRight(LogicError)
    newRooksPosition <- Position.createPiecePosition(xPos, 6).toRight(LogicError)
    isKingsPositionFree = gameState.board.getPieceOnPosition(newKingsPosition).isInstanceOf[Empty]
    isRooksPositionFree = gameState.board.getPieceOnPosition(newRooksPosition).isInstanceOf[Empty]
    canKingBeChecked = isCheck(gameState.board.put(king.copy(position = newKingsPosition), newKingsPosition).remove(king), team) ||
      isCheck(gameState.board.put(king.copy(position = newRooksPosition), newKingsPosition).remove(king), team)
    newState <- if (isKingsPositionFree && isRooksPositionFree && !canKingBeChecked) {
      GameState(gameState.board
        .move(king, newKingsPosition)
        .move(rook, newRooksPosition),
        finished = false
      ).asRight
    } else {
      CastlingIllegal("King could be checked").asLeft
    }
  } yield newState

  def clearEnpasantable(gameState: GameState, team: Team): GameState = {
    val pawns = gameState.board.getPawns(team)
    val newBoard = pawns.foldLeft(gameState.board)((board, p) =>
      board
        .remove(p)
        .put(p.copy(enpasantable = false), p.position)
    )
    GameState(newBoard, finished = false)
  }

  def bigPieceCapture(bigPieceCapture: BigPieceCapture, team: Team, gameState: GameState): MoveResult = {
    val otherPiecePosition = PiecePosition(bigPieceCapture.captureRank, bigPieceCapture.captureFile)
    val pieces = gameState.board
      .getPieces(bigPieceCapture.piece, team)
      .filter(p => canMoveToPosition(p, otherPiecePosition, gameState.board))
    for {
      piece <- getTheRightPiece(bigPieceCapture.file, bigPieceCapture.rank, pieces)
      newState <- GameState(gameState.board.move(piece, otherPiecePosition), finished = false).asRight[IllegalMoveReason]
    } yield newState
  }

  def bigPieceMove(bigPieceMove: BigPieceMove, team: Team, gameState: GameState): MoveResult = {
    val newPosition = PiecePosition(bigPieceMove.toRank, bigPieceMove.toFile)
    val pieces = gameState.board
      .getPieces(bigPieceMove.piece, team)
      .filter(p => canMoveToPosition(p, newPosition, gameState.board))
    for {
      piece <- getTheRightPiece(bigPieceMove.file, bigPieceMove.rank, pieces)
      newState <- GameState(gameState.board.move(piece, newPosition), finished = false).asRight[IllegalMoveReason]
    } yield newState
  }

  private def getTheRightPiece(file: Option[Position.PositionY], rank: Option[Position.PositionX], pieces: Vector[ChessPiece]): PieceSearchResult = {
    getPiece(pieces) <+> getPieceByFile(pieces, file) <+> getPieceByRank(pieces, rank) <+>
      getPieceByFileAndRank(pieces, file, rank)
  }

  private def getPiece(pieces: Vector[ChessPiece]): PieceSearchResult =
    if (pieces.length == 1) {
      pieces.headOption.toRight(LogicError)
    } else {
      PieceNotFound.asLeft
    }

  private def getPieceByFile(pieces: Vector[ChessPiece], file: Option[Position.PositionY]): PieceSearchResult = {
    val pieceAccordingToFile = pieces.filter(cp => file.contains(cp.position.y))
    if (pieceAccordingToFile.length == 1) {
      pieceAccordingToFile.headOption.toRight(LogicError)
    } else {
      PieceNotFound.asLeft
    }
  }

  private def getPieceByRank(pieces: Vector[ChessPiece], rank: Option[Position.PositionX]): PieceSearchResult = {
    val pieceAccordingToRank = pieces.filter(cp => rank.contains(cp.position.x))
    if (pieceAccordingToRank.length == 1) {
      pieceAccordingToRank.headOption.toRight(LogicError)
    } else {
      PieceNotFound.asLeft
    }
  }

  private def getPieceByFileAndRank(pieces: Vector[ChessPiece], file: Option[Position.PositionY],
                                    rank: Option[Position.PositionX]): PieceSearchResult = {
    val pieceAccordingToFileAndRank =
      pieces.filter(cp => file.contains(cp.position.y) && rank.contains(cp.position.x))
    if (pieceAccordingToFileAndRank.length == 1) {
      pieceAccordingToFileAndRank.headOption.toRight(LogicError)
    } else {
      PieceNotFound.asLeft
    }
  }

  def pawnCapture(pawnCapture: PawnCapture, team: Team, game: GameState): MoveResult = {
    val pawns = game.board.getPawnsInFile(pawnCapture.file, team)
    val otherPiecePosition = PiecePosition(pawnCapture.captureRank, pawnCapture.captureFile)
    val legiblePawns = pawns.filter(p => {
      canMoveToPosition(p, otherPiecePosition, game.board)
    })
    if (legiblePawns.length == 1 && pawnCapture.captureRank.value == 8) {
      for {
        pawn <- legiblePawns.headOption.toRight(LogicError)
        newPiece <- pawnCapture.promoteTo.toRight(PromotionPieceNotSpecified)
                                         .map(pt => ChessPiece.create(pt, otherPiecePosition, team))
        newBoard = game.board.remove(pawn).put(newPiece, otherPiecePosition)
      } yield GameState(newBoard, finished = false)
    } else if (legiblePawns.length == 1) {
      val enpassant = for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newState <- enpassantCapture(pawn.asInstanceOf[Pawn], team, game)
      } yield newState
      val normal = for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPawn = Pawn(otherPiecePosition, pawn.team, enpasantable = false)
        newBoard = game.board.remove(pawn).put(newPawn, otherPiecePosition)
      } yield GameState(newBoard, finished = false)
      enpassant <+> normal
    } else {
      MoveNotAllowed.asLeft
    }
  }

  private def enpassantCapture(pawn: Pawn, team: Team, gameState: GameState): MoveResult = for {
      potentialPosition <- Position.subtractMovesSafe(pawn.position, 1, 0).toRight(MoveNotAllowed)
      pawnToTake <- gameState.board.getPawns(team.getOtherTeam)
                                   .find(p => p.position == potentialPosition && p.enpasantable)
                                   .toRight(EnPassantInapplicable)
    } yield GameState(gameState.board.remove(pawnToTake)
    .put(Pawn(potentialPosition, pawn.team, enpasantable = false), potentialPosition)
    .remove(pawn), finished = false)

  def pawnMove(pawnMove: PawnMove, team: Team, game: GameState): MoveResult = {
    val pawns = game.board.getPawnsInFile(pawnMove.file, team)
    val positionToMoveTo = PiecePosition(pawnMove.toRank, pawnMove.file)
    val legiblePawns = pawns.filter(p => {
      canMoveToPosition(p, positionToMoveTo, game.board)
    })
    if (positionToMoveTo.x.value == 8 && legiblePawns.length == 1) {
      for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPiece <- pawnMove.promoteTo.toRight(PromotionPieceNotSpecified)
                                      .map(pt => ChessPiece.create(pt, positionToMoveTo, team))
        newBoard = game.board.remove(pawn).put(newPiece, positionToMoveTo)
      } yield GameState(newBoard, finished = false)
    } else if (legiblePawns.length == 1) {
      for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPawn = if (pawn.position.x.value == 2 && positionToMoveTo.x.value == 4 && team == White ||
        pawn.position.x.value == 7 && positionToMoveTo.x.value == 5 && team == Black) {
          Pawn(positionToMoveTo, team, enpasantable = true)
        } else {
          Pawn(positionToMoveTo, team, enpasantable = false)
        }
      } yield GameState(game.board.put(newPawn, positionToMoveTo).remove(pawn), finished = false)
    } else {
      MoveNotAllowed.asLeft
    }
  }

  def canMoveToPosition(chessPiece: ChessPiece, otherPiecePosition: PiecePosition, board: ChessBoard): Boolean = {
    val otherPiece = board.getPieceOnPosition(otherPiecePosition)
    canMove(chessPiece, otherPiece, board)
  }

  // TODO: presunout ten ischeck nekam vys at se da spravne reportovat ze je v sachu
  def canMove(chessPiece: ChessPiece, otherChessPiece: ChessPiece, board: ChessBoard): Boolean =
    canPieceMove(chessPiece, otherChessPiece, board) && !isCheck(board.move(chessPiece, otherChessPiece.position), chessPiece.team)

  private def canPieceMove(piece: ChessPiece, otherPiece: ChessPiece, board: ChessBoard): Boolean = piece match {
    case k@Knight(_, _) => canKnightMove(k, otherPiece)
    case p@Pawn(_, _, _) => canPawnMove(p, otherPiece)
    case r@Rook(p, _, _) => canRookMove(r, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case b@Bishop(p, _) => canBishopMove(b, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case q@Queen(p, _) => canQueenMove(q, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case k@King(p, _, _) => canKingMove(k, otherPiece)
    case Empty(_, _) => false
  }

  def isCheck(board: ChessBoard, team: Team): Boolean = (for {
    king <- board.getKing(team).headOption
    otherTeamsPieces = board.getOtherTeamsPieces(team)
    isChecked = otherTeamsPieces.exists(p => canPieceMove(p, king, board))
  } yield isChecked).getOrElse(false)

  def isCheckMate(gameState: GameState, team: Team): GameState =
    if (isCheck(gameState.board, team.getOtherTeam) && !canKingGoAnywhere(gameState.board, team.getOtherTeam)) {
      GameState(gameState.board, finished = true)
    } else {
      gameState
    }

  def canKingGoAnywhere(board: ChessBoard, team: Team): Boolean = (for {
    king <- board.getKing(team).headOption
    canHe = allPiecesAround(king, board) exists (p => canMoveToPosition(king, p.position, board))
  } yield canHe).getOrElse(false)

  def allPiecesAround(chessPiece: ChessPiece, board: ChessBoard): Vector[ChessPiece] = {
    val thisPosition = chessPiece.position
    val possibleAdditions = Vector((1, 1), (1, 0), (0, 1), (-1, -1), (-1, 0), (0, -1), (1, -1), (-1, 1))
    for {
      possibleAddition <- possibleAdditions
      otherPosition <- Position.addMove(thisPosition, possibleAddition._1, possibleAddition._2)
    } yield board.getPieceOnPosition(otherPosition)
  }

  def createInitialState: GameState = {
    val init: ChessBoard = ChessBoard(Map())
    val addPieces = for {
      _ <- putPawns
      _ <- putRooks
      _ <- putKnights
      _ <- putBishops
      _ <- putQueens
      _ <- putKings
    } yield ()
    GameState(addPieces.run(init).value._1, finished = false)
  }

  def putPieces(columns: Vector[Int], row: Int)(f: (PiecePosition, Team) => ChessPiece): State[ChessBoard, Unit] = for {
    board <- State.get
    whitePieces = columns.flatMap(column => Position.createPiecePosition(row, column)).map(f(_, White))
    blackPieces = columns.flatMap(column => Position.createPiecePosition(9 - row, column)).map(f(_, Black))
    newBoard = (whitePieces ++ blackPieces).foldLeft(board)((b, p) => b.put(p, p.position))
    _ <- State.set(newBoard)
  } yield ()

  def putPawns: State[ChessBoard, Unit] =
    putPieces(1 to 9 toVector, 2)((pp, t) => Pawn(pp, t, enpasantable = false))

  def putRooks: State[ChessBoard, Unit] =
    putPieces(Vector(1, 8), 1)((pp, t) => Rook(pp, t, moved = false))

  def putKnights: State[ChessBoard, Unit] =
    putPieces(Vector(2, 7), 1)((pp, t) => Knight(pp, t))

  def putBishops: State[ChessBoard, Unit] =
    putPieces(Vector(3, 6), 1)((pp, t) => Bishop(pp, t))

  def putQueens: State[ChessBoard, Unit] =
    putPieces(Vector(4), 1)((pp, t) => Queen(pp, t))

  def putKings: State[ChessBoard, Unit] =
    putPieces(Vector(5), 1)((pp, t) => King(pp, t, moved = false))
}
