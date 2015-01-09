package jp.sndyuk.shogi.ai.simulator

import scala.annotation.tailrec
import scala.util.Random

import jp.sndyuk.shogi.core.Block
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Rule
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn

object Utils {

  private class MovePointIterator(board: Board, state: State, availableBlocks: List[Block], includePromoted: Boolean, random: Boolean) extends Iterator[Transition] {
    private var rest = if (random) Random.shuffle(availableBlocks) else availableBlocks
    private var nextMove: Transition = _
    private var cache: Iterator[Transition] = _
    private var full = false

    def next: Transition = {
      if (hasNext) {
        full = false
        nextMove
      } else throw new NoSuchElementException
    }
    def hasNext: Boolean = if (full)
      true
    else {
      full = false
      if (cache == null || cache.isEmpty) {
        rest match {
          case Block(point, piece) :: xs =>
            rest = xs
            cache = Rule.generateMovablePoints(board, point, piece, state.turn, true, random).map {
              case (newPos, nari) => Transition(point, newPos, nari, None)
            }
            hasNext
          case _ => false
        }
      } else {
        nextMove = cache.next
        full = true
        true
      }
    }
  }

  def plans(board: Board, state: State, random: Boolean): Iterator[Transition] = {
    val pieces = board.allMovablePieces(state.turn)
    new MovePointIterator(board, state, pieces, true, random)
  }

  private def isCaptureOuAtNextTurn(transition: Transition, state: State, board: Board): Boolean = {
    val nextState = board.move(state, transition.oldPos, transition.newPos, false, transition.nari)
    val nextTransitions = plans(board, nextState, true)
    val result = findTransitionCaputuringOu(nextTransitions, nextState, board).isDefined

    board.rollback(nextState)
    result
  }

  def findTransitionCaputuringOu(xs: TraversableOnce[Transition], state: State, board: Board): Option[Transition] = xs.find(isCaptureOu(_, state, board))
  def isCaptureOu(transition: Transition, state: State, board: Board): Boolean = {
    val nextState = board.move(state, transition.oldPos, transition.newPos, false, transition.nari)
    // Get OU if the transition can capture it.
    if (board.isFinish(state.turn)) {
      board.rollback(nextState)
      true
    } else {
      board.rollback(nextState)
      false
    }
  }

  // 詰めろであることを証明する
  def simulateTsumero(board: Board, nextState: State, playerTurn: Turn, tsumeroMaxDepth: Int): (Int, Int, Int) = {
    val boardCp = board.copy()
    val prevState = boardCp.rollback(nextState)
    simulateTsumero(boardCp, prevState, nextState.history.head, playerTurn, nextState.history.length, 0, Math.min(nextState.history.length, tsumeroMaxDepth))
  }
  @tailrec private def simulateTsumero(board: Board, nextState: State, transition: Transition, playerTurn: Turn, depth: Int, tsumeroDepth: Int, tsumeroMaxDepth: Int): (Int, Int, Int) = {
    if (tsumeroDepth >= tsumeroMaxDepth) {
      (1, 1, depth)
    } else if (nextState.turn != playerTurn && !Utils.isCaptureOuAtNextTurn(transition, nextState, board)) {
      // 王手になっていなければ無効な手とする
      (0, 1, depth)
    } else {
      val boardCp = board.copy()
      val prevState = boardCp.rollback(nextState)
      simulateTsumero(boardCp, prevState, nextState.history.head, playerTurn, depth + 1, tsumeroDepth + 1, tsumeroMaxDepth)
    }
  }

}
