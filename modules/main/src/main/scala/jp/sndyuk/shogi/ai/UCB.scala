package jp.sndyuk.shogi.ai

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.util.Random
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import scala.concurrent.duration._
import akka.dispatch.Foreach
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB

class UCB extends AI {
  private val rnd = new Random(666667)

  private val size = 1
  private var maxDepth = 81
  private val depthOverMaxDepth = 10

  // 最後のn手を探索
  private val overflowEnd = 3
  private val overflowEndK = 4

  // 最初の探索
  private val overflowStart = 3
  private val overflowStartK = 4

  private val tsumeroMaxDepth = 3

  // 詰めろであることを証明する
  def simulateTsumero(board: Board, nextState: State, playerTurn: Turn, depth: Int): (Int, Int, Int) = {
    val boardCp = board.copy()
    val prevState = boardCp.rollback(nextState)
    simulateTsumero(boardCp, prevState, nextState.history.head, playerTurn, depth, 0, Math.min(nextState.history.length, tsumeroMaxDepth))
  }
  def simulateTsumero(board: Board, nextState: State, transition: Transition, playerTurn: Turn, depth: Int, tsumeroDepth: Int, tsumeroMaxDepth: Int): (Int, Int, Int) = {
    if (tsumeroDepth >= tsumeroMaxDepth) {
      (1, 1, depth)
    } else if (nextState.turn == playerTurn && !Utils.isCaptureOu(transition, nextState, board)) {
      // 王手になっていなければ無効な手とする
      (0, 1, depth)
    } else {
      val boardCp = board.copy()
      val prevState = boardCp.rollback(nextState)
      simulateTsumero(boardCp, prevState, nextState.history.head, playerTurn, depth + 1, tsumeroDepth + 1, tsumeroMaxDepth)
    }
  }

  // _1 = [ loose: 0 | win: 1 ]
  // _2 = branch size
  // _3 = board size
  def simulate(board: Board, state: State, next: Transition, playerTurn: Turn, depth: Int): (Int, Int, Int) = {
    val nextState = board.move(state, next.oldPos, next.newPos, false, next.nari)

    if (board.isFinish(playerTurn)) {
      if (depth > 50) {
        val tumi = simulateTsumero(board, nextState, playerTurn, depth)
        if (tumi._1 == 1) {
          println(board)
        }
        tumi
      } else {
        (0, 1, depth)
      }
    } else if (board.isFinish(playerTurn.change)) {
      (0, 1, depth)
    } else if (state.history.length == maxDepth) {
      (0, 1, depth)
    } else {
      val sizeK = if (state.history.length >= (maxDepth - overflowEnd)) {
        size * overflowEndK
      } else if (depth < overflowStart) {
        size * overflowStartK
      } else size
      val moves = getOuOrRandom(Utils.plans(board, nextState).filter(Utils.isEffectiveMove(_, nextState, board)), sizeK, nextState, board)
      moves.foldLeft((0, 0, depth)) { (acc, transition) =>
        val x = simulate(board.copy(), nextState, transition, playerTurn, depth + 1)
        (acc._1 + x._1, acc._2 + x._2, acc._3 + (x._3 - depth))
      }
    }
  }

  def next(board: Board, state: State): Transition = {
    val plans = Utils.plans(board, state)
    next(board, state, plans.filter(Utils.isEffectiveMove(_, state, board)))
  }

  private[ai] def next(board: Board, state: State, moves: Seq[Transition]): Transition = {
    if ((state.history.length + depthOverMaxDepth) > maxDepth) {
      maxDepth += depthOverMaxDepth
    }
    val start = System.currentTimeMillis
    val ou = Utils.findTransitionCaputuringOu(moves, state, board)
    if (ou.isDefined) {
      return ou.get
    }
    val score = moves.par.map { transition =>
      val result = simulate(board.copy(), state, transition, PlayerB, 0)
      (result._1.toDouble / result._2.toDouble, transition, result._2, result._3)
    }.seq.sortBy(_._1)
    val totalBranchSize = score.foldLeft(0) { (a, b) => a + b._3 }
    val totalBoardSize = score.foldLeft(0) { (a, b) => a + b._4 }
    val last = score.last
    val max = score.collect { case x if x._1 == last._1 => x }.sortBy(_._4).head
    score.foreach { println _ }
    val msec = System.currentTimeMillis - start
    println(s"Max: $max, branch size: $totalBranchSize, board size: $totalBoardSize(${totalBoardSize / Math.max(1, (msec / 1000))}/sec), $msec")
    max._2
  }

  private def getOuOrRandom(xs: List[Transition], size: Int, state: State, board: Board): List[Transition] = {
    if (xs.length <= 1) {
      return xs
    }
    val ou = Utils.findTransitionCaputuringOu(xs, state, board)
    if (ou.isDefined) {
      return List(ou.get)
    }
    if (size == 1) {
      return List(xs(rnd.nextInt(xs.length - 1)))
    }

    val buf = new ArrayBuffer ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to Math.max(Math.min(buf.length, size), 0) by -1) {
      val k = rnd.nextInt(n)
      swap(n - 1, k)
    }
    buf.take(size).toList
  }
}
