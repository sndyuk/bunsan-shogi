package jp.sndyuk.shogi.core // Piece, Turn, PlayerA, PlayerB, Point, Board, Transition are in this package

import jp.sndyuk.shogi.core.Player.Player
import jp.sndyuk.shogi.core.SimplePiece.SimplePieceType

object GameStateMapper {

  // --- Existing functions from previous subtask ---
  def coreTurnToPlayer(coreTurn: Turn): Player = {
    if (coreTurn == PlayerA) Player.SENTE else Player.GOTE
  }

  def playerToCoreTurn(player: Player): Turn = {
    if (player == Player.SENTE) PlayerA else PlayerB
  }

  def corePieceToSimplePieceTypeAndPlayer(corePiece: Piece): Option[(SimplePieceType, Player, Boolean)] = {
    if (corePiece == Piece.❏) {
      None
    } else {
      val player = if (Piece.△(corePiece)) Player.GOTE else Player.SENTE
      val isPromoted = Piece.isPromoted(corePiece)
      val generalizedPiece = Piece.generalize(corePiece)
      val simplePieceType = generalizedPiece match {
        case Piece.◯.FU => SimplePiece.FU
        case Piece.◯.KY => SimplePiece.KY
        case Piece.◯.KE => SimplePiece.KE
        case Piece.◯.GI => SimplePiece.GI
        case Piece.◯.KI => SimplePiece.KI
        case Piece.◯.KA => SimplePiece.KA
        case Piece.◯.HI => SimplePiece.HI
        case Piece.◯.OU => SimplePiece.OU
        case _ => throw new IllegalArgumentException(s"Unknown generalized piece: ${Piece.name(generalizedPiece)} raw: $generalizedPiece")
      }
      Some((simplePieceType, player, isPromoted))
    }
  }

  def simplePiecePlayerToCorePiece(simplePieceType: SimplePieceType, player: Player, isPromoted: Boolean): Piece = {
    val basePiece = simplePieceType match {
      case SimplePiece.FU => Piece.◯.FU
      case SimplePiece.KY => Piece.◯.KY
      case SimplePiece.KE => Piece.◯.KE
      case SimplePiece.GI => Piece.◯.GI
      case SimplePiece.KI => Piece.◯.KI
      case SimplePiece.KA => Piece.◯.KA
      case SimplePiece.HI => Piece.◯.HI
      case SimplePiece.OU => Piece.◯.OU
      case _ => throw new IllegalArgumentException(s"Unknown SimplePieceType: $simplePieceType")
    }
    val coreTurn = playerToCoreTurn(player)
    val playerPiece = Piece.convert(basePiece, coreTurn)
    if (isPromoted) Piece.promote(playerPiece) else playerPiece
  }

  // --- New functions for this subtask ---

  // 1. Core Point to GameState Position Mapping (and vice-versa)
  def corePointToPosition(corePoint: Point): Position = {
    // core.Point is (y: Int, x: Int)
    // GameState.Position is (x: Int, y: Int)
    // Position.x from corePoint.x, Position.y from corePoint.y
    Position(corePoint.x, corePoint.y)
  }

  def positionToCorePoint(position: Position): Point = {
    // GameState.Position is (x: Int, y: Int)
    // core.Point is (y: Int, x: Int)
    Point(position.y, position.x)
  }

  // 2. Core Board to GameState Board Setup Mapping
  def coreBoardToBoardSetup(coreBoard: Board): Map[Position, SimplePieceType] = {
    coreBoard.allBlocks.flatMap { block =>
      if (block.piece == Piece.❏) {
        None
      } else {
        corePieceToSimplePieceTypeAndPlayer(block.piece) match {
          case Some((spt, _, _)) => Some(corePointToPosition(block.point) -> spt)
          case None => None // Should not happen if piece is not EMPTY
        }
      }
    }.toMap
  }

  // 3. GameState Board Info to Core Board
  def reconstructCoreBoard(
    boardSetup: Map[Position, (SimplePieceType, Player, Boolean)],
    senteCaptured: List[SimplePieceType],
    goteCaptured: List[SimplePieceType]
  ): Board = {
    // Ensure Squares() and CapturedPieces() are the correct constructors for empty states
    val newCoreBoard = new Board(new Squares(), new CapturedPieces())

    boardSetup.foreach { case (position, (spt, player, isPromoted)) =>
      val corePoint = positionToCorePoint(position)
      val corePiece = simplePiecePlayerToCorePiece(spt, player, isPromoted)
      newCoreBoard.squares <+ (corePiece, corePoint) // Or set(corePiece, corePoint)
    }

    senteCaptured.foreach { spt =>
      // To make piece appear in Sente's hand, 'put' must receive a Gote piece,
      // as 'put' assumes the piece color indicates the player who lost it.
      val goteVersionOfPiece = simplePiecePlayerToCorePiece(spt, Player.GOTE, false) 
      newCoreBoard.capturedPieces.put(goteVersionOfPiece)
    }

    goteCaptured.foreach { spt =>
      // To make piece appear in Gote's hand, 'put' must receive a Sente piece.
      val senteVersionOfPiece = simplePiecePlayerToCorePiece(spt, Player.SENTE, false)
      newCoreBoard.capturedPieces.put(senteVersionOfPiece)
    }
    newCoreBoard
  }

  // 4. Core Transition to SimpleTransition Move String (USI format) - Helpers
  private def pointToUSI(point: Point): String = {
    // core.Point(y,x): USI file is (9-x).toString, USI rank is ('a'.toInt + y).toChar
    (9 - point.x).toString + ('a'.toInt + point.y).toChar.toString
  }

  private def capturedPointIndicatorToSimplePieceType(indicator: Int): SimplePieceType = {
    // From Point.ofCaptured(piece: Piece): Point
    // case ◯.OU => (9, 8) -> x=8
    // case ◯.KI => (9, 1) -> x=1
    // case ◯.FU => (9, 2) -> x=2
    // case ◯.GI => (9, 3) -> x=3
    // case ◯.HI => (9, 4) -> x=4
    // case ◯.KA => (9, 5) -> x=5
    // case ◯.KE => (9, 6) -> x=6
    // case ◯.KY => (9, 7) -> x=7
    indicator match {
      case 1 => SimplePiece.KI
      case 2 => SimplePiece.FU
      case 3 => SimplePiece.GI
      case 4 => SimplePiece.HI
      case 5 => SimplePiece.KA
      case 6 => SimplePiece.KE
      case 7 => SimplePiece.KY
      case 8 => SimplePiece.OU
      case _ => throw new IllegalArgumentException(s"Unknown captured piece indicator: $indicator")
    }
  }

  def simplePieceToUSIChar(spt: SimplePieceType): String = {
    spt match {
      case SimplePiece.FU => "P" // Pawn
      case SimplePiece.KY => "L" // Lance
      case SimplePiece.KE => "N" // Knight
      case SimplePiece.GI => "S" // Silver
      case SimplePiece.KI => "G" // Gold
      case SimplePiece.OU => "K" // King
      case SimplePiece.KA => "B" // Bishop
      case SimplePiece.HI => "R" // Rook
      // Note: Promoted pieces are not handled by this function, USI adds "+" suffix for moves.
      // For drops, only unpromoted pieces are used.
      case _ => throw new IllegalArgumentException(s"Unknown SimplePieceType for USI char: $spt")
    }
  }

  // 4. Core Transition to SimpleTransition Move String (USI format) - Main function
  def coreTransitionToMoveString(coreTransition: Transition, boardBeforeMove: Board): String = {
    val oldPos = coreTransition.oldPos
    val newPos = coreTransition.newPos

    if (Point.isCaptured(oldPos)) { // It's a drop
      // oldPos.x indicates the piece type based on Point.ofCaptured convention
      val droppedSpt = capturedPointIndicatorToSimplePieceType(oldPos.x)
      simplePieceToUSIChar(droppedSpt) + "*" + pointToUSI(newPos)
    } else { // It's a move from board
      val moveStr = pointToUSI(oldPos) + pointToUSI(newPos)
      if (coreTransition.nari) {
        moveStr + "+"
      } else {
        moveStr
      }
    }
  }

  // 5. Core Transition and Resulting Board to SimpleTransition Mapping
  def coreTransitionToSimpleTransition(
    coreTransition: Transition,
    boardBeforeMove: Board, // May be needed if drop piece identification is complex
    boardAfterMove: Board
  ): SimpleTransition = {
    val moveString = coreTransitionToMoveString(coreTransition, boardBeforeMove) // Pass boardBeforeMove
    val boardStateAfterMove = coreBoardToBoardSetup(boardAfterMove)
    SimpleTransition(moveString, boardStateAfterMove)
  }
}
