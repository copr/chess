package cz.copr.chess.chessLogic

import cz.copr.chess.chessLogic.ChessBoard.Board
import cz.copr.chess.chessLogic.Position.{PiecePosition, createPiecePosition}

case class ChessBoard(board: Board) {
  def getPawnsInFile(file: Position.Position, team: Team): Vector[ChessPiece] =
    getPawns(team).filter(p => p.position.y == file)


  def getOtherTeamsPieces(team: Team): Vector[ChessPiece] =
    getAllPieces.filter(p => p.team == team.getOtherTeam)


  def isEmptyFromTo(p: PiecePosition, position: PiecePosition): Boolean = {
    val res = Position.subtractMoves(p, position)
    val x = p.x.value
    val y = p.y.value
    val xIncrement = if (x > position.x.value) -1 else 1
    val yIncrement = if (y > position.y.value) -1 else 1
    if (res._1 == 0 && res._2 != 0) {
      // column check
      val yPositions = List.range(y + yIncrement, position.y.value, yIncrement)
      yPositions.forall(yy => createPiecePosition(x, yy).exists(pos => getPieceOnPosition(pos).isEmpty))
    } else if (res._1 != 0 && res._2 == 0) {
      // row check
      val xPositions = List.range(x + xIncrement, position.x.value, xIncrement)
      xPositions.forall(xx => createPiecePosition(xx, y).exists(pos => getPieceOnPosition(pos).isEmpty))
    } else if (math.abs(res._1) == math.abs(res._2) && res._1 != 0) {
      // crosswise check
      val xPositions = List.range(x + xIncrement, position.x.value, xIncrement)
      val yPositions = List.range(y + yIncrement, position.y.value, yIncrement)
      val positions = xPositions.zip(yPositions)
      positions.forall(xy => createPiecePosition(xy._1, xy._2).exists(pos => getPieceOnPosition(pos).isEmpty))
    } else {
      false
    }
  }

  def getAllPieces: Vector[ChessPiece] =
    board.values.flatMap(row => row.values).toVector

  def getAllTeamsPieces(team: Team): Vector[ChessPiece] =
    getAllPieces.filter(_.team == team)

  def move(chessPiece: ChessPiece, position: PiecePosition): ChessBoard = chessPiece match {
    case k: King   => this.remove(chessPiece).put(k.copy(position = position, moved = true), position)
    case k: Bishop => this.remove(chessPiece).put(k.copy(position = position), position)
    case k: Knight => this.remove(chessPiece).put(k.copy(position = position), position)
    case k: Queen  => this.remove(chessPiece).put(k.copy(position = position), position)
    case k: Rook   => this.remove(chessPiece).put(k.copy(position = position, moved = true), position)
    case k: Pawn   => this.remove(chessPiece).put(k.copy(position = position), position)
    case  _        => this
  }

  // TODO: Je put potreba takhle? Nestaci putChessPiece?
  def put(chessPiece: ChessPiece, position: PiecePosition): ChessBoard = {
    val x = position.x.value
    val y = position.y.value
    val newRow = board.getOrElse(x, Map()) + (y -> chessPiece)
    ChessBoard(board + (x -> newRow))
  }

  def putChessPiece(chessPiece: ChessPiece): ChessBoard = {
    val x = chessPiece.position.x.value
    val y = chessPiece.position.y.value
    val newRow = board.getOrElse(x, Map()) + (y -> chessPiece)
    ChessBoard(board + (x -> newRow))
  }

  def remove(chessPiece: ChessPiece): ChessBoard = {
    val x = chessPiece.position.x.value
    val y = chessPiece.position.y.value
    val newRow = board(x).updated(y, Empty(chessPiece.position, Nothing))
    ChessBoard(board.updated(x, newRow))
  }

  def getPieceOnPosition(position: PiecePosition): ChessPiece = (for {
    row <- board.get(position.x.value)
    piece <- row.get(position.y.value)
  } yield piece).getOrElse(Empty(position, Nothing))

  def getPieces(pieceType: PieceType, team: Team): Vector[ChessPiece] = pieceType match {
    case PawnType   => getPawns(team)
    case RookType   => getRooks(team)
    case KnightType => getKnights(team)
    case BishopType => getBishops(team)
    case QueenType  => getQueens(team)
    case KingType   => getKing(team)
  }

  def getPawns(team: Team): Vector[Pawn] = getAllPieces.filter(x => x match {
    case Pawn(_, t, _) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[Pawn])

  def getRooks(team: Team): Vector[Rook] = getAllPieces.filter(x => x match {
    case Rook(_, t, _) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[Rook])

  def getKnights(team: Team): Vector[Knight] = getAllPieces.filter(x => x match {
    case Knight(_, t) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[Knight])

  def getBishops(team: Team): Vector[Bishop] = getAllPieces.filter(x => x match {
    case Bishop(_, t) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[Bishop])

  def getQueens(team: Team): Vector[Queen] = getAllPieces.filter(x => x match {
    case Queen(_, t) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[Queen])

  def getKing(team: Team): Vector[King] = getAllPieces.filter(x => x match {
    case King(_, t, _) if t == team => true
    case _ => false
  }).map(_.asInstanceOf[King])
}

object ChessBoard {
  type Board = Map[Int, Map[Int, ChessPiece]]

  def chessBoard(board: Board): Option[ChessBoard] =
    Some(ChessBoard(board))
}
