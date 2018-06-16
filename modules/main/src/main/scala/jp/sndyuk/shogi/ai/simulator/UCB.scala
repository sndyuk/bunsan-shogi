package jp.sndyuk.shogi.ai.simulator

import java.util.ArrayDeque

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.util.Random

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import scala.collection.mutable.PriorityQueue

case class BS(board: Board, state: State, transition: Transition, depth: Int, index: Int)

class UCB(private val priority: Ordering[BS], private val maxQueueSize: Int = 1000) extends AI {
  private var maxDepth = 100

  private def simulate(board: Board, state: State, player: Turn, plans: List[Transition]): Transition = {
    val start = System.currentTimeMillis

    // _1 = [ loose: 0 | win: 1 ]
    // _2 = branch size
    // _3 = Transition
    val acc = plans.map((0, 0, _)).toArray
    val queue = new PriorityQueue[BS]()(priority)
    plans.zipWithIndex.foreach { p =>
      queue += BS(board, state, p._1, 0, p._2)
    }
    val totalBoardCount = simulate(player, queue, acc, 1)

    val score = acc.map { result =>
      (result._1.toDouble / Math.max(result._2.toDouble, 1), result._2, result._3)
    }.sortBy(_._1)

    val last = score.last
    val max = score.collect { case x if x._1 == last._1 => x }.sortBy(_._2).head

    score.foreach { println _ }
    val msec = System.currentTimeMillis - start
    println(s"Selected: $max, total playouts: $totalBoardCount(${(totalBoardCount.toDouble / msec * 1000).toInt}/sec), Elapsed: ${msec / 1000} sec")

    max._3
  }

  @tailrec private def simulate(player: Turn, q: PriorityQueue[BS], acc: Array[(Int, Int, Transition)], boardCount: Int): Int = {
    if (q.isEmpty) {
      return boardCount
    }
    val BS(board, state, transition, depth, i) = q.dequeue()

    val boardCp = board.copy()
    val nextState = boardCp.move(state, transition.oldPos, transition.newPos, false, transition.nari)

    val score = acc(i)
    if (boardCp.isFinish(nextState.turn)) {
      acc(i) = (if (player == nextState.turn) score._1 + 1 else score._1, score._2, score._3)
    } else {
      acc(i) = (score._1, score._2 + 1, score._3)

      if (depth < maxDepth) {
        val plans = Utils.plans(boardCp, nextState)
        while (plans.hasNext) {
          q += BS(boardCp, nextState, plans.next, depth + 1, i)
        }
        if (q.size >= maxQueueSize) {
          q.dropRight(maxQueueSize - q.size);
        }
      }
    }

    return simulate(player, q, acc, boardCount + 1)
  }

  def next(board: Board, state: State): Transition = {
    val plans = Utils.plans(board, state).toList
    next(board, state, plans)
  }

  private[ai] def next(board: Board, state: State, plans: List[Transition]): Transition = {
    // Get OU if it can.
    val ou = Utils.findTransitionCaputuringOu(plans, state, board)
    if (ou.isDefined) {
      return ou.get
    }

    if ((state.history.length + 1) > maxDepth) {
      println("Unexpected depth")
      maxDepth += 1
    }
    simulate(board.copy, state, state.turn.change, plans)
  }
}
