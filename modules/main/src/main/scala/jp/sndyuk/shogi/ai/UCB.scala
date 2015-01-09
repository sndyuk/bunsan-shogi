package jp.sndyuk.shogi.ai

import java.util.ArrayDeque
import java.util.Queue

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.language.higherKinds
import scala.util.Random

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn

class UCB extends AI {
  private val rnd = new Random(666667)

  private val size = 1
  private var maxDepth = 71
  private val depthOverMaxDepth = 10

  // overflowStart: 30, k: 10
  // 5,000,000: 1.3G MEM, 11.5sec
  // 1,000,000: 12sec
  private val maxBoardCount = 10000000

  // 最後のn手を探索
  private val overflowEnd = 3
  private val overflowEndK = 4

  // 最初の探索
  private val overflowStart = 30
  private val overflowStartK = 40

  private val tsumeroMaxDepth = 3

  private val validate = false

  // Board, Transition, Depth, Index of acc
  type BS = (Board, State, Transition, Int, Int)

  // _1 = [ loose: 0 | win: 1 ]
  // _2 = branch size
  private def simulate(board: Board, state: State, player: Turn, plans: List[Transition]): Transition = {
    val start = System.currentTimeMillis

    val acc = plans.map((0, 0, _)).toArray
    val queue = new ArrayDeque[BS](Math.min(Math.pow(4, overflowStart), maxBoardCount).toInt) 
    plans.zipWithIndex.foreach { p =>
      queue.addLast((board, state, p._1, 0, p._2))
    }
    val checkedSet = HashSet(ID(board))
    val totalBoardCount = simulate(player, checkedSet, queue, acc, 1, true)

    val score = acc.map { result =>
      (result._1.toDouble / Math.max(result._2.toDouble, 1), result._2, result._3)
    }.sortBy(_._1)

    val playoutCount = score.foldLeft(0) { (a, b) => a + b._2 }
    val last = score.last
    val max = score.collect { case x if x._1 == last._1 => x }.sortBy(_._2).head

    score.foreach { println _ }
    val msec = System.currentTimeMillis - start
    println(s"Selected: $max, playout: $playoutCount(${(playoutCount.toDouble / msec * 1000).toInt}/sec), board: $totalBoardCount(${(totalBoardCount.toDouble / msec * 1000).toInt}/sec), $msec")

    max._3
  }

  //  @tailrec private def judgeTsumi(player: Turn, checkedSet: Set[ID], acc: (Int, Int, Transition)): (Int, Int, Transition) = {
  //    acc
  //  }

  @tailrec private def simulate(player: Turn, checkedSet: Set[ID], q: ArrayDeque[BS], acc: Array[(Int, Int, Transition)], count: Int, more: Boolean): Int = {
    val (board, state, transition, depth, i) = q.pollFirst()

    val boardCp = board.copy()
    val nextState = boardCp.move(state, transition.oldPos, transition.newPos, false, transition.nari)
    if (validate) {
      val id = ID(boardCp)
      if (checkedSet.contains(id)) {
        throw new Error(s"(!) conflict: $id")
      } else {
        checkedSet + id
      }
    }
    val score = acc(i)
    if (boardCp.isFinish(player)) {
      acc(i) = (score._1 + 1, score._2 + 1, score._3)
    } else if (boardCp.isFinish(player.change)) {
      acc(i) = (score._1, score._2 + 1, score._3)
    } else if (depth == maxDepth) {
      acc(i) = (score._1, score._2 + 1, score._3)
    }
    if (count < maxBoardCount && !q.isEmpty) {
      if (more && depth != maxDepth) {
        val sizeK = if (depth < overflowStart) {
          size * overflowStartK
        } else size

        val plans = Utils.plans(boardCp, nextState, true)
        var s = 0
        for (_ <- 0 until sizeK if plans.hasNext) {
          s += 1
          q.addLast((boardCp, nextState, plans.next, depth + 1, i))
        }
        s
      }
      simulate(player, checkedSet, q, acc, count + 1, !(!more || q.size() >= maxBoardCount))
    } else count
  }

  def next(board: Board, state: State): Transition = {
    val plans = Utils.plans(board, state, true).toList
    next(board, state, plans)
  }

  private[ai] def next(board: Board, state: State, plans: List[Transition]): Transition = {
    if ((state.history.length + depthOverMaxDepth) > maxDepth) {
      maxDepth += depthOverMaxDepth
    }
    val ou = Utils.findTransitionCaputuringOu(plans, state, board)
    if (ou.isDefined) {
      return ou.get
    }

    simulate(board, state, PlayerB, plans)
  }
}
