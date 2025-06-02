package jp.sndyuk.shogi.core

import scala.annotation.tailrec

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import jp.sndyuk.shogi.core.Piece._

object Rule {

  val logger = Logger(LoggerFactory.getLogger(this.getClass().getName()))

  /**
   * 駒が指定された場所に移動可能ならtrue
   */
  def canMove(board: Board, piece: Piece, oldPos: Point, newPos: Point, turn: Turn, nari: Boolean = false): Boolean = {
    if (oldPos == newPos) {
      logger.warn("Select an another point.")
      false
    } else if (▲△(piece, turn.change)) {
      logger.warn("It's not your piece.")
      false
    } else {
      isValidPosition(board, piece, newPos, true) &&
        generateMovablePoints(board, oldPos, piece, turn, false).exists(_._1 == newPos) &&
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
          case x :: xs => {
            rest = xs
            if (x._3 == ∞) {
              // 1つずつ進めて駒の移動先が有効である限り再帰
              @tailrec def f(pos: Point, acceptCapturing: Boolean, tmpPoints: List[Move]): List[Move] = {
                val newPos = Point(pos.y + x._1, pos.x + x._2)
                if (acceptCapturing && isValidPosition(board, piece, newPos, acceptCapturing)) {
                  // 敵の駒を通りすぎないように1回敵の駒に届いたあとは acceptCapturing を falseにする
                  f(newPos, !board.pieceOnBoardNotEmpty(newPos).isDefined,
                    (if (includePromoted && canBePromoted(board, oldPos, newPos, piece)) {
                      val gpiece = generalize(piece)
                      if (gpiece == ◯.FU || gpiece == ◯.KA || gpiece == ◯.HI || gpiece == ◯.KY) {
                        (newPos, true) :: tmpPoints // 無駄に成らない歩、角、飛、香は不要
                      } else (newPos, true) :: (newPos, false) :: tmpPoints
                    } else (newPos, false) :: tmpPoints))
                } else tmpPoints
              }
              // 飛べる駒は元の場所に遷移できるので更にもう1手進められることを確認する必要はない
              f(oldPos, true, Nil) match {
                case Nil => hasNext
                case l =>
                  nextMove = l.head
                  cache = l.tail
                  full = true
                  true
              }
            } else {
              val newPos = Point(oldPos.y + x._1, oldPos.x + x._2)
              if (isValidPosition(board, piece, newPos, true) && (canMoveAtNextTurn(newPos, originalScopes) || canMoveIfPromoted(piece, oldPos, newPos))) {
                if (includePromoted && canBePromoted(board, oldPos, newPos, piece)) {
                  nextMove = (newPos, true)
                  cache = (newPos, false) :: Nil
                } else {
                  nextMove = (newPos, false)
                }
                full = true
                true
              } else hasNext
            }
          }
        }
      } else cache match {
        case Nil => hasNext
        case x :: xs =>
          nextMove = x
          cache = xs
          full = true
          true
      }
    }
  }

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
}