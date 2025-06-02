package jp.sndyuk.shogi.core

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer // Added for ListBuffer

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import jp.sndyuk.shogi.core.Piece._

object Rule {

  val logger = Logger(LoggerFactory.getLogger(this.getClass().getName()))

  /**
   * 駒が指定された場所に移動可能ならtrue
   */
  private def isSpecificMoveValid(board: Board, piece: Piece, oldPos: Point, newPos: Point, turn: Turn): Boolean = {
    if (Point.isCaptured(oldPos)) { // Drop from hand
      // 1. King cannot be dropped (already handled by generateMovablePoints, but good to have)
      //    The problem description asks for Piece.generalize(piece) != Piece.◯.OU,
      //    but generateMovablePoints (which is a reference for drop rules) also has this.
      //    isValidPosition in canMove will handle newPos being on board.
      //    The prompt implies isSpecificMoveValid should check newPos is empty.
      val newPosIsEmpty = board.pieceOnBoard(newPos).contains(Piece.❏)

      newPosIsEmpty &&
      Piece.generalize(piece) != Piece.◯.OU &&
      !is2FU(board, piece, newPos, turn) &&
      canMoveAtNextTurn(newPos, movableScopes(piece)) // Pass newPos and scopes for the piece being dropped
    } else { // Move on board
      val scopes = movableScopes(piece)
      scopes.exists { case (dy, dx, isSliding) =>
        if (!isSliding) { // Non-sliding piece
          val checkPos = Point(oldPos.y + dy, oldPos.x + dx)
          checkPos == newPos
        } else { // Sliding piece (Rook, Bishop, Lance, Dragon, Horse)
          var currentY = oldPos.y + dy
          var currentX = oldPos.x + dx
          var pathClear = true
          var foundTarget = false

          while (isOnBoard(Point(currentY, currentX)) && pathClear && !foundTarget) {
            val currentDest = Point(currentY, currentX)
            if (currentDest == newPos) {
              foundTarget = true // Path to newPos is clear
            } else {
              // If any piece is on currentDest (between oldPos and newPos), path is not clear for further sliding
              if (board.pieceOnBoardNotEmpty(currentDest).isDefined) {
                pathClear = false
              }
            }
            if (pathClear && !foundTarget) { // Only advance if path still clear and target not yet found
              currentY += dy
              currentX += dx
            }
          }
          foundTarget // True if newPos was reached along this sliding path
        }
      }
    }
  }

  def canMove(board: Board, piece: Piece, oldPos: Point, newPos: Point, turn: Turn, nari: Boolean = false): Boolean = {
    if (oldPos == newPos) {
      logger.warn("Select an another point.")
      false
    } else if (▲△(piece, turn.change)) {
      logger.warn("It's not your piece.")
      false
    } else {
      // isValidPosition checks if newPos is on board and if it's empty or an opponent's piece.
      isValidPosition(board, piece, newPos, true) &&
      isSpecificMoveValid(board, piece, oldPos, newPos, turn) &&
      (!nari || canBePromoted(board, oldPos, newPos, piece))
    }
  }

  /**
   * 駒が移動可能な場所を返す
   */
  def generateMovablePoints(board: Board, oldPos: Point, piece: Piece, turn: Turn, includePromoted: Boolean): Iterator[Move] = {
    val scopes = movableScopes(piece)
    (if (Point.isCaptured(oldPos)) { // This means it's a drop from hand
      board.allEmptyPoints().filter { np =>
        Piece.generalize(piece) != Piece.◯.OU && // ADDED: Cannot drop a King
        !is2FU(board, piece, np, turn) &&
        canMoveAtNextTurn(np, scopes)
      }.map { (_, false) } // Drops are never promotions
    } else {
      generateMovePoints(board, piece, oldPos, turn, includePromoted, scopes, scopes)
    })
  }

  private class MovePointIterator(board: Board, piece: Piece, oldPos: Point, turn: Turn, includePromoted: Boolean, scopes: List[Scope], originalScopes: List[Scope]) extends Iterator[Move] {
    private var rest =  originalScopes
    private var nextMove: Move = _
    private var cache: List[Move] = Nil
    private var full = false

    def next: Move = {
      if (hasNext) {
        full = false
        nextMove
      } else throw new NoSuchElementException
    }

    def hasNext: Boolean = if (full) {
      true
    } else {
      full = false
      if (cache.isEmpty) {
        rest match {
          case Nil => false
          case x :: xs => { // x is a Scope (dy, dx, isSliding)
            rest = xs
            if (x._3 == ∞) { // isSliding is true for ∞
              // Process sliding moves
              @tailrec
              def f(currentPosIter: Point, acceptCapturing: Boolean, movesBuffer: ListBuffer[Move]): ListBuffer[Move] = {
                val nextPotentialPos = Point(currentPosIter.y + x._1, currentPosIter.x + x._2) // x._1 is dy, x._2 is dx
                if (acceptCapturing && isValidPosition(board, piece, nextPotentialPos, acceptCapturing)) {
                  // Valid square to move to
                  if (includePromoted && canBePromoted(board, oldPos, nextPotentialPos, piece)) {
                    movesBuffer += ((nextPotentialPos, false)) // Add unpromoted move first
                    movesBuffer += ((nextPotentialPos, true))  // Add promoted move second
                  } else {
                    movesBuffer += ((nextPotentialPos, false)) // Add unpromoted move only
                  }
                  // Continue sliding if the path wasn't blocked by capturing an opponent's piece at nextPotentialPos
                  f(nextPotentialPos, !board.pieceOnBoardNotEmpty(nextPotentialPos).isDefined, movesBuffer)
                } else {
                  movesBuffer // Base case: cannot move further along this path or invalid position
                }
              }

              val generatedMoves = f(oldPos, true, ListBuffer.empty[Move]).toList.reverse // Reverse to get furthest moves first
              generatedMoves match {
                case Nil => hasNext // No moves found along this sliding path
                case l_head :: l_tail =>
                  nextMove = l_head
                  cache = l_tail
                  full = true
                  true
              }
            } else { // Non-sliding piece move
              val newPos = Point(oldPos.y + x._1, oldPos.x + x._2)
              // Check if the single step is valid; also ensure piece can move if it stays or if it's promoted
              // The canMoveAtNextTurn check is important for pieces like knights at edge of board.
              if (isValidPosition(board, piece, newPos, true) && (canMoveAtNextTurn(newPos, originalScopes) || canMoveIfPromoted(piece, oldPos, newPos))) {
                if (includePromoted && canBePromoted(board, oldPos, newPos, piece)) {
                  // For non-sliding, the order for cache is (promoted as next, unpromoted in cache)
                  nextMove = (newPos, true)
                  cache = (newPos, false) :: Nil
                } else {
                  nextMove = (newPos, false)
                  // cache remains empty or is not set here
                }
                full = true
                true
              } else hasNext // This specific non-sliding move is not valid
            }
          }
        }
      } else cache match { // Cache has pending moves (typically the unpromoted version of a previous promoted move)
        case Nil => hasNext // Should not happen if cache was not empty
        case x :: xs => // x is a Move (Point, Boolean)
          nextMove = x
          cache = xs
          full = true
          true
      }
    }
  }

  // originalScopes is passed to MovePointIterator to check canMoveAtNextTurn for non-sliding moves
  private def generateMovePoints(board: Board, piece: Piece, oldPos: Point, turn: Turn, includePromoted: Boolean, scopes: List[Scope], originalScopes: List[Scope]): Iterator[Move] = {
    new MovePointIterator(board, piece, oldPos, turn, includePromoted, scopes, originalScopes)
  }

  private def canMoveIfPromoted(piece: Piece, oldPos: Point, newPos: Point): Boolean = {
    if (Point.isCaptured(oldPos)) false
    else {
      val promoted = promote(piece)
      if (promoted == piece) {
        false
      }
      canMoveAtNextTurn(newPos, movableScopes(promoted))
    }
  }

  @inline private def canMoveAtNextTurn(pos: Point, scopes: List[Scope]): Boolean = scopes.exists(p => isOnBoard(Point(pos.y + p._1, pos.x + p._2)))

  // 次のターンも移動可能か
  def canMoveAtNextTurn(piece: Piece, pos: Point): Boolean = canMoveAtNextTurn(pos, movableScopes(piece))

  @inline private def isOnBoard(pos: Point) = pos.y < 9 && pos.x < 9 && pos.y >= 0 && pos.x >= 0

  // 駒の移動先が有効か
  private def isValidPosition(board: Board, piece: Piece, pos: Point, acceptCapturing: Boolean): Boolean = {
    if (isOnBoard(pos)) {
      board.pieceOnBoard(pos) match {
        case Some(target) => target == Piece.❏ || (acceptCapturing && (△(piece) ^ △(target)))
        case None => false
      }
    } else false
  }

  // 上下左右(▲が下側):
  //   (-1, -1) ↑(-1, 0)   (-1, 1)
  // ←(0, -1)            →(0, 1)
  //   (1, -1)  ↓(1, 0)    (1, 1)
  type Scope = (Int, Int, Boolean) // (y, x)

  // 無限に移動可能
  val ∞ = true

  def movableScopes(piece: Piece): List[Scope] = {
    piece match {
      case ▲.OU => List((-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false), (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false))
      case ▲.FU => List((-1, 0, false))
      case ▲.KI => List((1, 0, false), (0, 1, false), (-1, 1, false), (-1, 0, false), (-1, -1, false), (0, -1, false))
      case ▲.GI => List((-1, 0, false), (-1, 1, false), (1, 1, false), (1, -1, false), (-1, -1, false))
      case ▲.HI => List((1, 0, true), (0, 1, true), (-1, 0, true), (0, -1, true))
      case ▲.KA => List((1, 1, true), (-1, 1, true), (-1, -1, true), (1, -1, true))
      case ▲.KE => List((-2, 1, false), (-2, -1, false))
      case ▲.KY => List((-1, 0, true))
      case ▲.TO => movableScopes(▲.KI)
      case ▲.NG => movableScopes(▲.KI)
      case ▲.RY => movableScopes(▲.OU) ::: movableScopes(▲.HI)
      case ▲.UM => movableScopes(▲.OU) ::: movableScopes(▲.KA)
      case ▲.NK => movableScopes(▲.KI)
      case ▲.NY => movableScopes(▲.KI)

      case ❏ => Nil
      case △.OU => List((1, 0, false), (1, 1, false), (0, 1, false), (-1, 1, false), (-1, 0, false), (-1, -1, false), (0, -1, false), (1, -1, false))
      case △.FU => List((1, 0, false))
      case △.KI => List((1, 0, false), (1, 1, false), (0, 1, false), (-1, 0, false), (0, -1, false), (1, -1, false))
      case △.GI => List((1, 0, false), (1, 1, false), (-1, 1, false), (-1, -1, false), (1, -1, false))
      case △.HI => List((1, 0, true), (0, 1, true), (-1, 0, true), (0, -1, true))
      case △.KA => List((1, 1, true), (-1, 1, true), (-1, -1, true), (1, -1, true))
      case △.KE => List((2, 1, false), (2, -1, false))
      case △.KY => List((1, 0, true))
      case △.TO => movableScopes(△.KI)
      case △.NG => movableScopes(△.KI)
      case △.RY => movableScopes(△.OU) ::: movableScopes(△.HI)
      case △.UM => movableScopes(△.OU) ::: movableScopes(△.KA)
      case △.NK => movableScopes(△.KI)
      case △.NY => movableScopes(△.KI)
    }
  }

  private val _0_8 = (0 to 8)

  /**
   *  2歩判定
   */
  def is2FU(board: Board, piece: Piece, pos: Point, turn: Turn): Boolean = {
    if (!isPromoted(piece) && generalize(piece) == ◯.FU) {
      _0_8.exists { y =>
        val p = Point(y, pos.x)
        board.pieceOnBoard(p).exists(_ == piece)
      }
    } else false
  }

  /**
   *  千日手判定
   */
  def isThreefoldRepetition(board: Board, state: State): Boolean = {
    val his = state.history // his(0) is the most recent move
    val size = his.size

    @inline def same(a: Transition, b: Transition): Boolean = a.newPos == b.newPos // And implicitly same player due to turn structure

    // Check for 3-fold repetition by the current player (X . X . X pattern)
    // Needs at least 5 moves in history for pattern P1, P2, P1, P2, P1 (indices 0,1,2,3,4)
    if (size >= 5) {
      // Current player's moves: his(0), his(2), his(4)
      if (same(his(0), his(2)) && same(his(0), his(4))) {
        return true
      }
      // Opponent's moves: his(1), his(3), his(5)
      // Needs at least 6 moves for this specific check
      if (size >= 6 && same(his(1), his(3)) && same(his(1), his(5))) {
        return true
      }
    }

    // Check for 3-fold repetition by sequence (X Y Z X Y Z X Y Z pattern)
    // Needs at least 7 moves for pattern P1, P2, P3, P1, P2, P3, P1 (indices 0,1,2,3,4,5,6)
    if (size >= 7) {
      // Current player's sequence start: his(0), his(3), his(6)
      if (same(his(0), his(3)) && same(his(0), his(6))) {
        return true
      }
      // Opponent's sequence start (P2): his(1), his(4), his(7)
      // Needs at least 8 moves for this specific check
      if (size >= 8 && same(his(1), his(4)) && same(his(1), his(7))) {
        return true
      }
      // Third player in sequence (P3, if applicable, though it's 2 player game, this means player C's turn): his(2), his(5), his(8)
      // Needs at least 9 moves for this specific check
      if (size >= 9 && same(his(2), his(5)) && same(his(2), his(8))) {
        return true
      }
    }
    false
  }

  /**
   * 成駒判定
   */
  def canBePromoted(board: Board, oldPos: Point, newPos: Point, piece: Piece): Boolean = {
    // 既に成っていない、かつ...
    !isPromoted(piece) && !board.isCaptured(oldPos) && (
      // 敵陣に居る or 持駒以外が敵陣に入る
      (if (▲(piece)) oldPos.y <= 2 else oldPos.y >= 6) ||
      (!board.isCaptured(oldPos) && (if (▲(piece)) {
        newPos.y <= 2
      } else {
        newPos.y >= 6
      })))
  }

  /**
   * Checks if the specified player's King is currently in check.
   * @param board The current board state.
   * @param playerWhoseKingIsChecked The player whose King's safety is being checked.
   * @return True if playerWhoseKingIsChecked's King is under attack, false otherwise.
   */
  def isInCheck(board: Board, playerWhoseKingIsChecked: Turn): Boolean = {
    val kingPiece = Piece.convert(Piece.◯.OU, playerWhoseKingIsChecked)
    board.squares.find(kingPiece) match {
      case None =>
        logger.warn(s"King not found for player $playerWhoseKingIsChecked. Cannot determine check status.")
        false // Or throw an error, as this is an invalid state
      case Some(kingPos) =>
        val opponentTurn = playerWhoseKingIsChecked.change

        // Helper to check a specific relative position for an attacking piece
        def isAttackedAtRelativePosition(dy: Int, dx: Int, attackerPieceTypes: Set[Piece]): Boolean = {
          val attackFromPos = Point(kingPos.y + dy, kingPos.x + dx)
          if (isOnBoard(attackFromPos)) {
            board.pieceOnBoard(attackFromPos) match {
              case Some(piece) if Piece.▲△(piece, opponentTurn) && attackerPieceTypes.contains(Piece.generalize(piece)) =>
                true
              case Some(piece) if Piece.▲△(piece, opponentTurn) && attackerPieceTypes.contains(piece) => // For specific promoted pieces
                true
              case _ => false
            }
          } else false
        }

        // Helper to check for sliding piece attacks
        def isAttackedBySlidingPiece(directions: List[(Int, Int)], attackerPieceTypes: Set[Piece]): Boolean = {
          directions.exists { case (dy, dx) =>
            var currentPos = Point(kingPos.y + dy, kingPos.x + dx)
            while (isOnBoard(currentPos)) {
              board.pieceOnBoard(currentPos) match {
                case Some(piece) =>
                  if (Piece.▲△(piece, opponentTurn) && (attackerPieceTypes.contains(Piece.generalize(piece)) || attackerPieceTypes.contains(piece))) {
                    return true // Found an attacker
                  }
                  return false // Path blocked by another piece (either own or non-attacker opponent)
                case None => // Empty square, continue along this direction
                  currentPos = Point(currentPos.y + dy, currentPos.x + dx)
              }
            }
            false // Reached edge of board without finding attacker in this direction
          }
        }

        // Define opponent's pieces (generalized and specific promoted)
        val opponentPawn = Piece.convert(Piece.◯.FU, opponentTurn)
        val opponentLance = Piece.convert(Piece.◯.KY, opponentTurn)
        val opponentKnight = Piece.convert(Piece.◯.KE, opponentTurn)
        val opponentSilver = Piece.convert(Piece.◯.GI, opponentTurn)
        val opponentGold = Piece.convert(Piece.◯.KI, opponentTurn) // Also for TO, NG, NK, NY
        val opponentBishop = Piece.convert(Piece.◯.KA, opponentTurn)
        val opponentRook = Piece.convert(Piece.◯.HI, opponentTurn)
        val opponentKing = Piece.convert(Piece.◯.OU, opponentTurn)

        val opponentPromotedPawn = Piece.promote(opponentPawn) // TO
        val opponentPromotedLance = Piece.promote(opponentLance) // NY
        val opponentPromotedKnight = Piece.promote(opponentKnight) // NK
        val opponentPromotedSilver = Piece.promote(opponentSilver) // NG
        val opponentPromotedBishop = Piece.promote(opponentBishop) // UM
        val opponentPromotedRook = Piece.promote(opponentRook) // RY

        // 1. Check Pawn attacks
        val pawnAttackDy = if (opponentTurn == Turn.Sente) -1 else 1
        if (isAttackedAtRelativePosition(pawnAttackDy, 0, Set(Piece.◯.FU))) return true

        // 2. Check Knight attacks
        val knightDeltas = if (opponentTurn == Turn.Sente) List((-2, -1), (-2, 1)) else List((2, -1), (2, 1))
        for ((dy, dx) <- knightDeltas) {
          if (isAttackedAtRelativePosition(dy, dx, Set(Piece.◯.KE))) return true
        }

        // 3. Check Silver General attacks
        val silverDeltas = if (opponentTurn == Turn.Sente) List((-1, -1), (-1, 0), (-1, 1), (1, -1), (1, 1))
                           else List((1, -1), (1, 0), (1, 1), (-1, -1), (-1, 1))
        for ((dy, dx) <- silverDeltas) {
          if (isAttackedAtRelativePosition(dy, dx, Set(Piece.◯.GI))) return true
        }

        // 4. Check Gold General (and equivalents: TO, NG, NK, NY) attacks
        val goldDeltas = if (opponentTurn == Turn.Sente) List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, 0))
                         else List((1, -1), (1, 0), (1, 1), (0, -1), (0, 1), (-1, 0))
        val goldLikePieces = Set(Piece.◯.KI, Piece.◯.TO, Piece.◯.NG, Piece.◯.NK, Piece.◯.NY)
        for ((dy, dx) <- goldDeltas) {
          if (isAttackedAtRelativePosition(dy, dx, goldLikePieces)) return true
        }

        // 5. Check King attacks (from opponent's King, or Dragon/Horse single steps)
        val kingDeltas = List((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1))
        val kingLikeAttackers = Set(Piece.◯.OU, Piece.◯.RY, Piece.◯.UM) // Generalized RY, UM for king-like moves
        for ((dy, dx) <- kingDeltas) {
          // Check for opponent King, or promoted Rook/Bishop that have King-like moves
           val targetPos = Point(kingPos.y + dy, kingPos.x + dx)
           if (isOnBoard(targetPos)) {
             board.pieceOnBoard(targetPos) match {
               case Some(p) if Piece.▲△(p, opponentTurn) =>
                 if (Piece.generalize(p) == Piece.◯.OU) return true
                 if (Piece.generalize(p) == Piece.◯.HI && Piece.isPromoted(p)) return true // RY has king moves
                 if (Piece.generalize(p) == Piece.◯.KA && Piece.isPromoted(p)) return true // UM has king moves
               case _ =>
             }
           }
        }

        // 6. Check Lance attacks
        val lanceAttackDir = if (opponentTurn == Turn.Sente) List((-1, 0)) else List((1, 0))
        if (isAttackedBySlidingPiece(lanceAttackDir, Set(Piece.◯.KY))) return true

        // 7. Check Rook attacks (Rook or Dragon)
        val rookDirections = List((-1,0), (1,0), (0,-1), (0,1))
        val rookLikePieces = Set(Piece.◯.HI, Piece.◯.RY) // RY also slides like a rook
        if (isAttackedBySlidingPiece(rookDirections, rookLikePieces)) return true

        // 8. Check Bishop attacks (Bishop or Horse)
        val bishopDirections = List((-1,-1), (-1,1), (1,-1), (1,1))
        val bishopLikePieces = Set(Piece.◯.KA, Piece.◯.UM) // UM also slides like a bishop
        if (isAttackedBySlidingPiece(bishopDirections, bishopLikePieces)) return true

        // No attacks found
        false
    }
  }
}