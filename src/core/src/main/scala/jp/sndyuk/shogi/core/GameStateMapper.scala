package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.core.{Piece => CorePieceType}
import jp.sndyuk.shogi.core.Piece._
import jp.sndyuk.shogi.core.{Turn => CoreTurnAlias}
import jp.sndyuk.shogi.core.Player.{Player => GamePlayer}
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => GameSimplePieceType}
// Ensure jp.sndyuk.shogi.core.Position is imported if not automatically available
// For Position case class defined in GameState.scala, it's in jp.sndyuk.shogi.core.Position
// For SimpleTransition case class defined in GameState.scala, it's in jp.sndyuk.shogi.core.SimpleTransition

object GameStateMapper {

  // --- Basic Mappings (Turn, CorePiece <-> SimplePieceType/Player) ---
  def coreTurnToPlayer(coreTurn: CoreTurnAlias): GamePlayer = coreTurn match {
    case PlayerA => Player.SENTE
    case PlayerB => Player.GOTE
  }

  def playerToCoreTurn(player: GamePlayer): CoreTurnAlias = player match {
    case Player.SENTE => PlayerA
    case Player.GOTE  => PlayerB
  }

  def corePieceToSimplePieceTypeAndPlayer(corePiece: CorePieceType): Option[(GameSimplePieceType, GamePlayer, Boolean)] = {
    if (corePiece == ❏) None else {
      val gamePlayer = if (Piece.△(corePiece)) Player.GOTE else Player.SENTE
      val isPromotedFlag = isPromoted(corePiece)
      val generalizedPiece = generalize(corePiece)
      val simplePieceType = generalizedPiece match {
        case ◯.FU => SimplePiece.FU
        case ◯.KY => SimplePiece.KY
        case ◯.KE => SimplePiece.KE
        case ◯.GI => SimplePiece.GI
        case ◯.KI => SimplePiece.KI
        case ◯.KA => SimplePiece.KA
        case ◯.HI => SimplePiece.HI
        case ◯.OU => SimplePiece.OU
        case _    => throw new IllegalArgumentException(s"Unknown generalized piece: $generalizedPiece, original core piece: $corePiece")
      }
      Some((simplePieceType, gamePlayer, isPromotedFlag))
    }
  }

  def simplePiecePlayerToCorePiece(simplePiece: GameSimplePieceType, player: GamePlayer, isPromotedFlag: Boolean): CorePieceType = {
    val baseGeneralizedPiece = simplePiece match {
      case SimplePiece.FU => ◯.FU
      case SimplePiece.KY => ◯.KY
      case SimplePiece.KE => ◯.KE
      case SimplePiece.GI => ◯.GI
      case SimplePiece.KI => ◯.KI
      case SimplePiece.KA => ◯.KA
      case SimplePiece.HI => ◯.HI
      case SimplePiece.OU => ◯.OU
    }
    val corePlayerTurn = playerToCoreTurn(player)
    val playerPiece = convert(baseGeneralizedPiece, corePlayerTurn)
    if (isPromotedFlag) promote(playerPiece) else playerPiece
  }

  // --- Point <-> Position Mappings ---
  def corePointToPosition(corePoint: Point): Position = {
    // Core Point(y,x) from core.Point
    // GameState Position(x,y) from GameState.Position
    // Position.x = corePoint.x (File) and Position.y = corePoint.y (Rank)
    Position(corePoint.x, corePoint.y)
  }

  def positionToCorePoint(position: Position): Point = {
    // Position(x,y) -> file_0idx_rtl, rank_0idx_ttb
    // Core Point(y,x) -> rank_0idx_ttb, file_0idx_rtl
    // So, Point.y = position.y (Rank) and Point.x = position.x (File)
    Point(position.y, position.x)
  }

  // --- Board Setup Mappings ---
  def coreBoardToBoardSetup(coreBoard: Board): Map[Position, GameSimplePieceType] = {
    coreBoard.allBlocks.filter(_.piece != ❏).map { block =>
      corePointToPosition(block.point) ->
        (corePieceToSimplePieceTypeAndPlayer(block.piece) match {
          case Some((spt, _, _)) => spt // We only need SimplePieceType for boardSetup value
          case None              => throw new IllegalStateException(s"coreBoardToBoardSetup: Non-empty Piece ${block.piece} at ${block.point} mapped to None for SimplePieceType")
        })
    }.toMap
  }

  def reconstructCoreBoard(
    boardSetup: Map[Position, (GameSimplePieceType, GamePlayer, Boolean)],
    senteCaptured: List[GameSimplePieceType],
    goteCaptured: List[GameSimplePieceType]
  ): Board = {
    // new Board() creates a board with empty Squares and empty CapturedPieces due to default arguments.
    // It does NOT call board.init() itself. Board.apply() calls board.init().
    val newBoard = new Board()

    boardSetup.foreach { case (pos, (spt, player, isPromoted)) =>
      val coreP = positionToCorePoint(pos)
      val corePiece = simplePiecePlayerToCorePiece(spt, player, isPromoted)
      newBoard.squares <+ (corePiece, coreP) // place piece on board
    }

    senteCaptured.foreach { spt =>
      val gotePieceVariant = simplePiecePlayerToCorePiece(spt, Player.GOTE, false)
      newBoard.capturedPieces.put(gotePieceVariant)
    }
    goteCaptured.foreach { spt =>
      val sentePieceVariant = simplePiecePlayerToCorePiece(spt, Player.SENTE, false)
      newBoard.capturedPieces.put(sentePieceVariant)
    }
    newBoard
  }

  // --- USI Move String and Transition Mappings ---
  private def pointToUSI(point: Point): String = {
    // Core Point is (y,x) where y=row (0-8, top-to-bottom), x=col (0-8, right-to-left, Shogi file 9 to 1)
    // USI: file (1-9), rank (a-i)
    // USI file = 9 - point.x
    // USI rank char = ('a' + point.y).toChar
    s"${9 - point.x}${('a' + point.y).toChar}"
  }

  // This method was determined to be unused due to changes in coreTransitionToMoveString logic.
  // private def capturedCorePointXToSimplePieceType(indicatorX: Int): GameSimplePieceType = { ... }


  def simplePieceToUSIChar(spt: GameSimplePieceType): String = spt match {
    case SimplePiece.FU => "P"
    case SimplePiece.KY => "L"
    case SimplePiece.KE => "N"
    case SimplePiece.GI => "S"
    case SimplePiece.KI => "G"
    case SimplePiece.OU => "K" // King is K, not OU
    case SimplePiece.KA => "B"
    case SimplePiece.HI => "R"
  }

  def coreTransitionToMoveString(coreTransition: Transition, boardBeforeMove: Board): String = {
    // Point.isCaptured(point: Point): Boolean = point.x == 9 && point.y >=0 && point.y <= 7
    // This means for a drop, oldPos.x is 9, and oldPos.y indicates the piece type.
    if (Point.isCaptured(coreTransition.oldPos)) { // It's a drop
      // For drops, oldPos.y should be 9. Piece type is encoded in oldPos.x.
      // See Point.ofCaptured(piece) and Point.toString for captured pieces.
      val pieceX = coreTransition.oldPos.x
      val droppedSpt = pieceX match {
          // Mapping based on Point.ofCaptured(piece: Piece) which stores type in x for y=9
          case 1 => SimplePiece.KI // ◯.KI -> Point(y=9, x=1) in Point.ofCaptured if we adapt its y to x.
                                   // Point.scala ofCaptured: KI -> (9,1) (y,x)
                                   // Point.scala toString for captured: x match { case 1 => "金" (KI) }
          case 2 => SimplePiece.FU // FU -> (9,2)
          case 3 => SimplePiece.GI // GI -> (9,3)
          case 4 => SimplePiece.HI // HI -> (9,4)
          case 5 => SimplePiece.KA // KA -> (9,5)
          case 6 => SimplePiece.KE // KE -> (9,6)
          case 7 => SimplePiece.KY // KY -> (9,7)
          // OU (King) is case 8 in Point.ofCaptured but cannot be dropped.
          case _ => throw new IllegalArgumentException(s"Unknown captured piece indicator x-value for drop: $pieceX. oldPos was ${coreTransition.oldPos}")
      }
      s"${simplePieceToUSIChar(droppedSpt)}*${pointToUSI(coreTransition.newPos)}"
    } else { // It's a move from board
      val fromStr = pointToUSI(coreTransition.oldPos)
      val toStr = pointToUSI(coreTransition.newPos)
      val promotionSuffix = if (coreTransition.nari) "+" else ""
      s"$fromStr$toStr$promotionSuffix"
    }
  }

  def coreTransitionToSimpleTransition(
    coreTransition: Transition,
    boardBeforeMove: Board,
    boardAfterMove: Board
  ): SimpleTransition = {
    SimpleTransition(
      move = coreTransitionToMoveString(coreTransition, boardBeforeMove),
      boardStateAfterMove = coreBoardToBoardSetup(boardAfterMove)
    )
  }
}
