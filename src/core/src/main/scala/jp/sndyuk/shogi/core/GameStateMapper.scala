package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.core.{Piece => CorePieceType}
import jp.sndyuk.shogi.core.Piece._
import jp.sndyuk.shogi.core.{Turn => CoreTurnAlias}
import jp.sndyuk.shogi.core.Player.{Player => GamePlayer}
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => GameSimplePieceType}
// Ensure jp.sndyuk.shogi.core.Position is imported if not automatically available
// For Position case class defined in GameState.scala, it's in jp.sndyuk.shogi.core.Position
// For SimpleTransition and PieceInfo case classes defined in GameState.scala, they are in jp.sndyuk.shogi.core
// Note: PieceInfo is now GameState.PieceInfo if defined inside GameState object, or just PieceInfo if top-level in package.
// Assuming PieceInfo is accessible as jp.sndyuk.shogi.core.PieceInfo or GameState.PieceInfo based on previous step.
// For clarity, let's assume direct import or it's in scope.

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

  /**
   * Adjusts a board position based on the player's perspective. The internal
   * representation always assumes Sente at the bottom. When Gote is the active
   * player, coordinates provided from their point of view need to be rotated
   * 180 degrees.
   */
  def adjustPositionForPlayer(position: Position, player: GamePlayer): Position =
    if (player == Player.GOTE) Position(x = 10 - position.x, y = 10 - position.y) else position

  // --- Point <-> Position Mappings ---
  def positionToCorePoint(position: Position): Point = {
    // GameState.Position(x: Int, y: Int) with x=file (1-9), y=rank (1-9 for a-i)
    // core.Point(y: Int, x: Int) with y = rank_idx (0-8 for a-i), x = file_idx (0-8 for USI 9-1)
    Point(y = position.y - 1, x = 9 - position.x)
  }

  def corePointToPosition(corePoint: Point): Position = {
    // core.Point(y: Int, x: Int) with y = rank_idx (0-8 for a-i), x = file_idx (0-8 for USI 9-1)
    // GameState.Position(x: Int, y: Int) with x=file (1-9), y=rank (1-9 for a-i)
    Position(x = 9 - corePoint.x, y = corePoint.y + 1)
  }

  // --- Board Setup Mappings ---
  def coreBoardToBoardSetup(coreBoard: Board): Map[String, PieceInfo] = {
    coreBoard.allBlocks.filter(_.piece != ❏).map { block =>
      val corePoint = block.point // This is jp.sndyuk.shogi.core.Point (0-indexed x, y)
      val keyString = s"${corePoint.x}_${corePoint.y}" // Format as "x_y" using core 0-indexed coords
      keyString ->
        (corePieceToSimplePieceTypeAndPlayer(block.piece) match {
          // corePieceToSimplePieceTypeAndPlayer returns Option[(GameSimplePieceType, GamePlayer, Boolean)]
          case Some((spt, player, isPromoted)) => PieceInfo(spt, player, isPromoted) // Ensure PieceInfo is used
          case None => throw new IllegalStateException(s"coreBoardToBoardSetup: Non-empty Piece ${block.piece} at ${block.point} mapped to None for PieceInfo components")
        })
    }.toMap
  }

  def reconstructCoreBoard(
    boardSetup: Map[String, PieceInfo],
    senteCaptured: List[GameSimplePieceType],
    goteCaptured: List[GameSimplePieceType]
  ): Board = {
    // new Board() creates a board with empty Squares and empty CapturedPieces due to default arguments.
    // It does NOT call board.init() itself. Board.apply() calls board.init().
    val newBoard = new Board()

    boardSetup.foreach { case (keyString, pieceInfo) =>
      // keyString is "x_y", needs to be parsed back to core.Point
      // PieceInfo contains spt, player, isPromoted
      val parts = keyString.split('_')
      if (parts.length == 2) {
        try {
          val x = parts(0).toInt
          val y = parts(1).toInt
          val coreP = Point(y, x) // core.Point is (y,x)
          val corePiece = simplePiecePlayerToCorePiece(pieceInfo.pieceType, pieceInfo.player, pieceInfo.isPromoted)
          newBoard.squares <+ (corePiece, coreP) // place piece on board
        } catch {
          case e: NumberFormatException => throw new IllegalArgumentException(s"Invalid keyString format in boardSetup: $keyString. Expected 'x_y' with integers.", e)
        }
      } else {
        throw new IllegalArgumentException(s"Invalid keyString format in boardSetup: $keyString. Expected 'x_y'.")
      }
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
    boardBeforeMove: Board, // Corrected typo: Mobe -> Move
    boardAfterMove: Board
  ): SimpleTransition = {
    SimpleTransition(
      move = coreTransitionToMoveString(coreTransition, boardBeforeMove), // Corrected typo: Mobe -> Move
      boardStateAfterMove = coreBoardToBoardSetup(boardAfterMove) // This now passes Map[String, PieceInfo]
    )
  }
}
