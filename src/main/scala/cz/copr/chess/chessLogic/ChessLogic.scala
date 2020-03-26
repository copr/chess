package cz.copr.chess.chessLogic

import cats.implicits._
import cz.copr.chess.chessLogic.ChessPiece._
import cz.copr.chess.chessLogic.ChessState.Result
import cz.copr.chess.chessLogic.Position.PiecePosition

object ChessLogic {
  private type PieceSearchResult = Result[ChessPiece]


  def move(move: Move, game: ChessState): ChessState.MoveResult = {
    val clearedEnpasantableState = clearEnpasantable(game)
    val stateAfterMove = move match {
      case SmallCastling        => smallCastling(clearedEnpasantableState)
      case BigCastling          => bigCastling(clearedEnpasantableState)
      case bpm: BigPieceMove    => bigPieceMove(bpm, clearedEnpasantableState)
      case bpc: BigPieceCapture => bigPieceCapture(bpc, clearedEnpasantableState)
      case pc: PawnCapture      => pawnCapture(pc, clearedEnpasantableState)
      case pm: PawnMove         => pawnMove(pm, clearedEnpasantableState)
    }
    stateAfterMove.map(gs => isDraw(isCheckMate(gs)))
  }

  def bigCastling(gameState: ChessState): ChessState.MoveResult = for {
    king <- gameState.board
      .getKing(gameState.team)
      .headOption
      .toRight(CouldNotFindThePiece(KingType))
      .filterOrElse(!_.moved, CastlingIllegal("King has already moved"))
    xPos = if (gameState.team == White) 1 else 8
    rook <- gameState.board
      .getRooks(gameState.team)
      .find(x => x.position.y.value == 1 && x.position.x.value == xPos)
      .toRight(CastlingIllegal("Rook is not in the right position"))
      .filterOrElse(!_.moved, CastlingIllegal("Rook has already moved"))
    newKingsPosition <- Position.createPiecePosition(xPos, 3).toRight(LogicError)
    newRooksPosition <- Position.createPiecePosition(xPos, 4).toRight(LogicError)
    rooksGoThrouPos  <- Position.createPiecePosition(xPos, 2).toRight(LogicError)
    isKingsPositionFree = gameState.board.getPieceOnPosition(newKingsPosition).isInstanceOf[Empty]
    isRooksPositionFree = gameState.board.getPieceOnPosition(newRooksPosition).isInstanceOf[Empty]
    isRooksGoThrouhFree = gameState.board.getPieceOnPosition(rooksGoThrouPos).isInstanceOf[Empty]
    canKingBeChecked = isCheck(gameState.board.move(king, newRooksPosition), gameState.team) ||
      isCheck(gameState.board.move(king, newKingsPosition), gameState.team)
    newState <- if (isKingsPositionFree && isRooksPositionFree && !canKingBeChecked && isRooksGoThrouhFree) {
      gameState.copy(
        board = gameState.board
        .move(king, newKingsPosition)
        .move(rook, newRooksPosition)
      ).asRight
    } else {
      CastlingIllegal("King would be checked by castling or Rook can't move to position").asLeft
    }
  } yield newState

  def smallCastling(gameState: ChessState): ChessState.MoveResult = for {
    king <- gameState.board
      .getKing(gameState.team)
      .headOption
      .toRight(CouldNotFindThePiece(KingType))
      .filterOrElse(!_.moved, CastlingIllegal("King has already moved"))
    xPos = if (gameState.team == White) 1 else 8
    rook <- gameState.board
      .getRooks(gameState.team)
      .find(x => x.position.y.value == 8 && x.position.x.value == xPos)
      .toRight(CastlingIllegal("Rook is not in the right place"))
      .filterOrElse(!_.moved, CastlingIllegal("Rook has already moved"))
    newKingsPosition <- Position.createPiecePosition(xPos, 7).toRight(LogicError)
    newRooksPosition <- Position.createPiecePosition(xPos, 6).toRight(LogicError)
    isKingsPositionFree = gameState.board.getPieceOnPosition(newKingsPosition).isInstanceOf[Empty]
    isRooksPositionFree = gameState.board.getPieceOnPosition(newRooksPosition).isInstanceOf[Empty]
    canKingBeChecked = isCheck(gameState.board.put(king.copy(position = newKingsPosition), newKingsPosition).remove(king), gameState.team) ||
      isCheck(gameState.board.put(king.copy(position = newRooksPosition), newKingsPosition).remove(king), gameState.team)
    newState <- if (isKingsPositionFree && isRooksPositionFree && !canKingBeChecked) {
      gameState.copy(
        board = gameState.board
        .move(king, newKingsPosition)
        .move(rook, newRooksPosition)
      ).asRight
    } else {
      CastlingIllegal("King could be checked").asLeft
    }
  } yield newState

  def clearEnpasantable(gameState: ChessState): ChessState = {
    val pawns = gameState.board.getPawns(gameState.team)
    val newBoard = pawns.foldLeft(gameState.board)((board, p) =>
      board
        .remove(p)
        .put(p.copy(enpasantable = false), p.position)
    )
    gameState.copy(board = newBoard)
  }

  def bigPieceCapture(bigPieceCapture: BigPieceCapture, gameState: ChessState): ChessState.MoveResult = {
    val otherPiecePosition = PiecePosition(bigPieceCapture.captureRank, bigPieceCapture.captureFile)
    val pieces = gameState.board
      .getPieces(bigPieceCapture.piece, gameState.team)
      .filter(p => canMoveToPosition(p, otherPiecePosition, gameState.board))
    for {
      piece    <- getTheRightPiece(bigPieceCapture.file, bigPieceCapture.rank, pieces)
        .leftMap(_ => PieceNotFound(bigPieceCapture.piece, bigPieceCapture.rank, bigPieceCapture.file, gameState.team))
      newState <- gameState.copy(board = gameState.board.move(piece, otherPiecePosition)).asRight[IllegalMoveReason]
    } yield newState
  }

  def bigPieceMove(bigPieceMove: BigPieceMove, gameState: ChessState): ChessState.MoveResult = {
    val newPosition = PiecePosition(bigPieceMove.toRank, bigPieceMove.toFile)
    val pieces = gameState.board
      .getPieces(bigPieceMove.piece, gameState.team)
      .filter(p => canMoveToPosition(p, newPosition, gameState.board))
    for {
      piece    <- getTheRightPiece(bigPieceMove.file, bigPieceMove.rank, pieces)
        .leftMap(_ => PieceNotFound(bigPieceMove.piece, bigPieceMove.rank, bigPieceMove.file, gameState.team))
      newState <- gameState.copy(board = gameState.board.move(piece, newPosition)).asRight[IllegalMoveReason]
    } yield newState
  }

  private def getTheRightPiece(file: Option[Position.PositionY], rank: Option[Position.PositionX], pieces: Vector[ChessPiece]): PieceSearchResult = {
    getPiece(pieces) orElse getPieceByFile(pieces, file) orElse getPieceByRank(pieces, rank) orElse
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

  def pawnCapture(pawnCapture: PawnCapture, gameState: ChessState): ChessState.MoveResult = {
    val pawns = gameState.board.getPawnsInFile(pawnCapture.file, gameState.team)
    val otherPiecePosition = PiecePosition(pawnCapture.captureRank, pawnCapture.captureFile)
    val legiblePawns = pawns.filter(p => {
      canMoveToPosition(p, otherPiecePosition, gameState.board)
    })
    if (legiblePawns.length == 1 && pawnCapture.captureRank.value == 8) {
      for {
        pawn <- legiblePawns.headOption.toRight(LogicError)
        newPiece <- pawnCapture.promoteTo.toRight(PromotionPieceNotSpecified)
                                         .map(pt => ChessPiece.create(pt, otherPiecePosition, gameState.team))
        newBoard = gameState.board.remove(pawn).put(newPiece, otherPiecePosition)
      } yield gameState.copy(board = newBoard)
    } else if (legiblePawns.length == 1) {
      val enpassant = for {
        pawn     <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newState <- enpassantCapture(pawn.asInstanceOf[Pawn], gameState)
      } yield newState
      val normal = for {
        pawn     <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPawn  = Pawn(otherPiecePosition, pawn.team, enpasantable = false)
        newBoard = gameState.board.remove(pawn).put(newPawn, otherPiecePosition)
      } yield gameState.copy(board = newBoard)
      enpassant <+> normal
    } else {
      MoveNotAllowed.asLeft
    }
  }

  private def enpassantCapture(pawn: Pawn, gameState: ChessState): ChessState.MoveResult = for {
      potentialPosition <- if (pawn.team == White) {
        Position.addMove(pawn.position, -1, 0).toRight(MoveNotAllowed)
      } else {
        Position.addMove(pawn.position,  1, 0).toRight(MoveNotAllowed)
      }
      pawnToTake <- gameState.board.getPawns(gameState.team.getOtherTeam)
                                   .find(p => p.position == potentialPosition && p.enpasantable)
                                   .toRight(EnPassantInapplicable)
    } yield gameState.copy(
    board = gameState.board.remove(pawnToTake)
      .put(Pawn(potentialPosition, pawn.team, enpasantable = false), potentialPosition)
      .remove(pawn))

  def pawnMove(pawnMove: PawnMove, gameState: ChessState): ChessState.MoveResult = {
    val pawns = gameState.board.getPawnsInFile(pawnMove.file, gameState.team)
    val positionToMoveTo = PiecePosition(pawnMove.toRank, pawnMove.file)
    val legiblePawns = pawns.filter(p => {
      canMoveToPosition(p, positionToMoveTo, gameState.board)
    })
    val lastRow = if (gameState.team == White) 8 else 1
    // promotion
    if (positionToMoveTo.x.value == lastRow && legiblePawns.length == 1) {
      for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPiece <- pawnMove.promoteTo.toRight(PromotionPieceNotSpecified)
                                      .map(pt => ChessPiece.create(pt, positionToMoveTo, gameState.team))
        newBoard = gameState.board.remove(pawn).put(newPiece, positionToMoveTo)
      } yield gameState.copy(board = newBoard)
    } else if (legiblePawns.length == 1) {
      for {
        pawn <- legiblePawns.headOption.toRight(CouldNotFindThePiece(PawnType))
        newPawn = if (pawn.position.x.value == 2 && positionToMoveTo.x.value == 4 && gameState.team == White ||
        pawn.position.x.value == 7 && positionToMoveTo.x.value == 5 && gameState.team == Black) {
          Pawn(positionToMoveTo, gameState.team, enpasantable = true)
        } else {
          Pawn(positionToMoveTo, gameState.team, enpasantable = false)
        }
      } yield gameState.copy(board = gameState.board.put(newPawn, positionToMoveTo).remove(pawn))
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
    case k@Knight(_, _)  => canKnightMove(k, otherPiece)
    case p@Pawn(_, _, _) => canPawnMove(p, otherPiece)
    case r@Rook(p, _, _) => canRookMove(r, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case b@Bishop(p, _)  => canBishopMove(b, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case q@Queen(p, _)   => canQueenMove(q, otherPiece) && board.isEmptyFromTo(p, otherPiece.position)
    case k@King(_, _, _) => canKingMove(k, otherPiece)
    case Empty(_, _)     => false
  }

  def isCheck(board: ChessBoard, team: Team): Boolean = (for {
    king <- board.getKing(team).headOption
    otherTeamsPieces = board.getOtherTeamsPieces(team)
    isChecked = otherTeamsPieces.exists(p => canPieceMove(p, king, board))
  } yield isChecked).getOrElse(false)

  def isCheckMate(gameState: ChessState): ChessState =
    if (isCheck(gameState.board, gameState.team.getOtherTeam) && !canKingGoAnywhere(gameState.board, gameState.team.getOtherTeam)) {
      val finishedState = if (gameState.team == White) WhiteWon else BlackWon
      gameState.copy(gameResult = finishedState)
    } else {
      gameState
    }

  def isDraw(game: ChessState): ChessState =
    if (getAllPossibleMoves(game).isEmpty) {
      game.copy(gameResult = Draw)
    } else if (!isEnoughMaterial(game)) {
      game.copy(gameResult = Draw)
    } else{
      game
    }

  def isEnoughMaterial(game: ChessState): Boolean =
    hasTeamEnoughMaterial(game, White) && hasTeamEnoughMaterial(game, Black)

  def hasTeamEnoughMaterial(game: ChessState, team: Team): Boolean = {
    val pieces = game.board.getAllTeamsPieces(team)
    if (pieces.length == 1) {
      false
    } else if (pieces.length == 2) {
      !pieces.exists {
        case _: Bishop => true
        case _: Knight => true
        case _         => false
      }
    } else {
      true
    }
  }

  def getAllPossibleMoves(game: ChessState): Stream[Move] =
    game.board.getAllTeamsPieces(game.team.getOtherTeam).toStream.flatMap {
      case p: Pawn   => getAllPosibbleMovesForPawn(p, game)
      case r: Rook   => getAllPossibleMovesForRook(r, game)
      case b: Bishop => getAllPossibleMovesForBishop(b, game)
      case n: Knight => getAllPossibleMovesForKnight(n, game)
      case q: Queen  => getAllPossibleMovesForQueen(q, game)
      case k: King   => getAllPossibleMovesForKing(k, game)
      case _         => Stream.empty
    }

  def getAllPosibbleMovesForPawn(pawn: Pawn, game: ChessState): Stream[Move] = {
    val possibleMoves = Stream((1, 0), (2, 0))
      .flatMap(tup => Position.addMove(pawn.position, tup._1, tup._2))
      .map(game.board.getPieceOnPosition)
      .filter(canPawnMove(pawn, _))
      .map(otherPiece => PawnMove(otherPiece.position.y, otherPiece.position.x, None))
    val possibleCaptures = Stream((1, 1), (1, -1))
      .flatMap(tup => Position.addMove(pawn.position, tup._1, tup._2))
      .map(game.board.getPieceOnPosition)
      .filter(canPawnMove(pawn, _))
      .map(otherPiece => PawnCapture(pawn.position.y, otherPiece.position.y, otherPiece.position.x, None))
    possibleMoves ++ possibleCaptures
  }

  def getAllPossibleMovesForRook(rook: ChessPiece, game: ChessState): Stream[Move] = {
    val movesInAColumn = for {
      x  <- Stream.range(1, 8)
      px <- Position.createPositionX(x)
    } yield PiecePosition(px, rook.position.y)
    val movesInARow = for {
      y  <- Stream.range(1, 8)
      py <- Position.createPositionY(y)
    } yield PiecePosition(rook.position.x, py)
    val possibleMoves = movesInAColumn ++ movesInARow
    possibleMoves
      .map(game.board.getPieceOnPosition)
      .filter(canMove(rook, _, game.board))
      .map(createMoveOrCapture(rook, RookType, _))
  }

  def getAllPossibleMovesForBishop(bishop: ChessPiece, game: ChessState): Stream[Move] = {
    val leftToRightCrossMoves = for {
      increment   <- Stream.range(1, 8)
      startPositionX = bishop.position.x.value - bishop.position.y.value + 1
      startPositionY = bishop.position.y.value - bishop.position.x.value + 1
      newPosition <- Position.createPiecePosition(startPositionX + increment, startPositionY + increment)
    } yield newPosition
    val rightToLeftCrossMoves = for {
      increment   <- Stream.range(1, 8)
      startPositionX = bishop.position.x.value - bishop.position.y.value + 1
      startPositionY = bishop.position.y.value + bishop.position.x.value - 1
      newPosition <- Position.createPiecePosition(startPositionX + increment, startPositionY - increment)
    } yield newPosition
    val possibleMoves = leftToRightCrossMoves ++ rightToLeftCrossMoves
    possibleMoves
      .map(game.board.getPieceOnPosition)
      .filter(canMove(bishop, _, game.board))
      .map(createMoveOrCapture(bishop, BishopType, _))
  }

  def getAllPossibleMovesForKnight(knight: Knight, game: ChessState): Stream[Move] = {
    val possibleMoves = for {
      a <- Stream((1, 2), (2, 1))
      b <- Stream((1, 1), (1, -1), (-1, 1), (-1, -1))
    } yield (a._1 * b._1, a._2 * b._2)
    possibleMoves
      .flatMap { case (x, y) => Position.createPiecePosition(x, y).map(game.board.getPieceOnPosition) }
      .filter(canMove(knight, _, game.board))
      .map(createMoveOrCapture(knight, KnightType, _))
  }

  def getAllPossibleMovesForKing(king: King, game: ChessState): Stream[Move] =
  allPiecesAround(king, game.board)
    .toStream
    .filter(canMove(king, _, game.board))
    .map(createMoveOrCapture(king, KingType, _))

  def getAllPossibleMovesForQueen(queen: Queen, game: ChessState): Stream[Move] =
    getAllPossibleMovesForBishop(queen, game) ++ getAllPossibleMovesForRook(queen, game)

  private def createMoveOrCapture(piece: ChessPiece, pieceType: PieceType, otherChessPiece: ChessPiece): Move = {
    if (otherChessPiece.isEmpty) {
      BigPieceMove(KnightType, Some(piece.position.y), Some(piece.position.x), otherChessPiece.position.y, otherChessPiece.position.x)
    } else {
      BigPieceCapture(KnightType, Some(piece.position.y), Some(piece.position.x), otherChessPiece.position.y, otherChessPiece.position.x)
    }
  }

  def getAllPossibleMovesFor(game: ChessState, chessPiece: ChessPiece): Stream[Move] = chessPiece match {
    case pawn: Pawn     => getAllPosibbleMovesForPawn(pawn, game)
    case rook: Rook     => getAllPossibleMovesForRook(rook, game)
    case bishop: Bishop => getAllPossibleMovesForBishop(bishop, game)
    case knight: Knight => getAllPossibleMovesForKnight(knight, game)
    case king: King     => getAllPossibleMovesForKing(king, game)
    case queen: Queen   => getAllPossibleMovesForQueen(queen, game)
    case _              => Stream.empty
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
}
