package jp.sndyuk.shogi.kifu

import jp.sndyuk.shogi.core.{
  Piece => CorePieceObject, // Alias for the Piece object
  Point => CorePoint,
  Turn => CoreTurn,
  Transition => CoreTransition,
  Board => CoreBoard,
  PlayerA => CorePlayerA,
  PlayerB => CorePlayerB,
  SimplePiece // For SimplePiece.SimplePieceType
}
import jp.sndyuk.shogi.core.Piece // Imports the type Piece = Int (actual type of piece values)
import jp.sndyuk.shogi.kifu.{TempCore => KifuTempCore}

object KifuMapper {

  def coreTurnToKifuPlayer(coreTurn: CoreTurn): KifuTempCore.Player = coreTurn match {
    case CorePlayerA => KifuTempCore.SENTE
    case CorePlayerB => KifuTempCore.GOTE
  }

  def corePointToKifuPosition(corePoint: CorePoint): KifuTempCore.Position = {
    val kifuX = 9 - corePoint.x
    val kifuY = corePoint.y + 1
    KifuTempCore.Position(kifuX, kifuY)
  }

  private def capturedCorePointXToKifuPiece(indicator: Int): KifuTempCore.Piece = {
    indicator match {
      case 1 => KifuTempCore.KI
      case 2 => KifuTempCore.FU
      case 3 => KifuTempCore.GI
      case 4 => KifuTempCore.HI
      case 5 => KifuTempCore.KA
      case 6 => KifuTempCore.KE
      case 7 => KifuTempCore.KY
      case 8 => KifuTempCore.OU 
      case _ => throw new IllegalArgumentException(s"Unknown captured piece indicator for Kifu mapping: $indicator")
    }
  }

  def corePieceToKifuPiece(corePieceValue: Piece): KifuTempCore.Piece = { // Use Piece (Int) type
    val generalizedCorePiece = CorePieceObject.generalize(corePieceValue)
    generalizedCorePiece match {
      case CorePieceObject.◯.FU => KifuTempCore.FU
      case CorePieceObject.◯.KY => KifuTempCore.KY
      case CorePieceObject.◯.KE => KifuTempCore.KE
      case CorePieceObject.◯.GI => KifuTempCore.GI
      case CorePieceObject.◯.KI => KifuTempCore.KI
      case CorePieceObject.◯.KA => KifuTempCore.KA
      case CorePieceObject.◯.HI => KifuTempCore.HI
      case CorePieceObject.◯.OU => KifuTempCore.OU
      case CorePieceObject.❏ if generalizedCorePiece == CorePieceObject.❏ => // Corrected: CorePieceObject.❏
        throw new IllegalArgumentException(s"Cannot map empty core piece (❏) to KifuTempCore.Piece")
      case _ => throw new IllegalArgumentException(s"Unknown or unexpected generalized core piece: $generalizedCorePiece (${CorePieceObject.name(generalizedCorePiece)})")
    }
  }
  
  def simplePieceTypeToKifuPiece(spt: SimplePiece.SimplePieceType): KifuTempCore.Piece = spt match {
    case SimplePiece.FU => KifuTempCore.FU
    case SimplePiece.KY => KifuTempCore.KY
    case SimplePiece.KE => KifuTempCore.KE
    case SimplePiece.GI => KifuTempCore.GI
    case SimplePiece.KI => KifuTempCore.KI
    case SimplePiece.KA => KifuTempCore.KA
    case SimplePiece.HI => KifuTempCore.HI
    case SimplePiece.OU => KifuTempCore.OU
    // Not expecting other SimplePieceTypes like KING, ROOK from GameState anmore,
    // as GameState.SimplePiece was updated to use FU, KY etc.
    // If other enums from a different SimplePiece object were passed, this would need a default.
  }

  def coreTransitionToKifuMove(
    coreTrans: CoreTransition,
    playerWhoseMoveItWas: CoreTurn,
    boardBeforeMove: CoreBoard
  ): KifuTempCore.Move = {

    val kifuPlayer = coreTurnToKifuPlayer(playerWhoseMoveItWas)
    val kifuToPos = corePointToKifuPosition(coreTrans.newPos)
    val isDrop = CorePoint.isCaptured(coreTrans.oldPos)
    
    val kifuFromPosOpt: Option[KifuTempCore.Position] = if (isDrop) {
      None
    } else {
      Some(corePointToKifuPosition(coreTrans.oldPos))
    }

    val kifuPiece: KifuTempCore.Piece = if (isDrop) {
      capturedCorePointXToKifuPiece(coreTrans.oldPos.x)
    } else {
      val pieceMoved = boardBeforeMove.piece(coreTrans.oldPos, playerWhoseMoveItWas)
      if (pieceMoved == CorePieceObject.❏) { // Corrected: CorePieceObject.❏ // Check for empty piece from board
        throw new IllegalStateException(s"Attempted to map a move from board where no piece was present at oldPos: ${coreTrans.oldPos} for player ${playerWhoseMoveItWas}")
      }
      corePieceToKifuPiece(pieceMoved)
    }

    KifuTempCore.Move(
      player = kifuPlayer,
      from = kifuFromPosOpt,
      to = kifuToPos,
      piece = kifuPiece,
      promote = coreTrans.nari,
      isDrop = isDrop
    )
  }
}
