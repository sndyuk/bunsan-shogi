package jp.sndyuk.shogi.ai.simulator

import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.Queue
import java.util.LinkedList
import java.util.concurrent.SynchronousQueue
import jp.sndyuk.shogi.player.Utils

case class Config(maxDepth: Int, maxQueueSize: Int, thread: Int, select: List[Plan] => List[Score])

case class BoardState(board: Board, state: State, transition: Transition, depth: Int, index: Int)

case class Plan(win: Int, loose: Int, playouts: Int, transition: Transition)

case class Score(score: Double, playouts: Int, transition: Transition)

object Simulator {

  private def simulate(board: Board, state: State, player: Turn, plans: List[Transition], conf: Config): (List[Score], Int) = {
    val start = System.currentTimeMillis

    val scoresByPlan = plans.map(Plan(0, 0, 0, _)).toArray
    val queue = new LinkedList[BoardState]() // Use Java's Queue since Scala's Queue is slow.
    val depth = state.history.size
    plans.zipWithIndex.foreach { p =>
      queue.add(BoardState(board, state, p._1, depth, p._2))
    }
    val totalBoardCount = simulate(player, queue, scoresByPlan, 1, conf)

    val scores = conf.select(scoresByPlan.toList)

    val msec = System.currentTimeMillis - start
    println(s"Done thread ${Thread.currentThread().getName}: ${msec / 1000} sec")
    scores.foreach(println)
    (scores, totalBoardCount)
  }

  @tailrec private def simulate(player: Turn, q: Queue[BoardState], plans: Array[Plan], boardCount: Int, conf: Config): Int = {
    if (q.isEmpty) {
      return boardCount
    }
    val BoardState(board, state, transition, depth, i) = q.poll

    val boardCp = board.copy()
    val nextState = boardCp.move(state, transition.oldPos, transition.newPos, false, transition.nari)

    val plan = plans(i)
    if (depth == conf.maxDepth) {
      plans(i) = plan.copy(playouts = plan.playouts + 1)
    } else if (boardCp.isFinish(state.turn)) {
      if (player == state.turn) {
        plans(i) = Plan(plan.win + 1, plan.loose, plan.playouts, plan.transition)
      } else {
        plans(i) = Plan(plan.win, plan.loose - 1, plan.playouts, plan.transition)
      }
    } else if (q.size < conf.maxQueueSize) {
      val plans = Utils.plans(boardCp, nextState)
      plans.takeWhile(_ => q.size <= conf.maxQueueSize).foreach { p =>
        q.add(BoardState(boardCp, nextState, p, depth + 1, i))
      }
    }

    return simulate(player, q, plans, boardCount + 1, conf)
  }

  def run(board: Board, state: State, conf: Config): Transition = {
    val plans = Utils.plans(board, state).toList
    run(board, state, plans, conf)
  }

  private[ai] def run(board: Board, state: State, plans: List[Transition], conf: Config): Transition = {
    // Get OU if it can.
    val ou = Utils.findTransitionCaputuringOu(plans, state, board)
    if (ou.isDefined) {
      return ou.get
    }

    if ((state.history.length + 1) > conf.maxDepth) {
      throw new RuntimeException("Unexpected depth")
    }

    val start = System.currentTimeMillis

    val sizeParThread = plans.size / conf.thread
    val (scoresByPlan, totalBoardCount) = plans.sliding(sizeParThread, sizeParThread).toList.par.map { xs =>
      simulate(board.copy, state, state.turn, xs, conf)
    }.reduce((x, acc) => (x._1 ::: acc._1, x._2 + acc._2))
    val playouts = scoresByPlan.foldLeft(0)(_ + _.playouts)
    val max = scoresByPlan.max(Ordering.by { s: Score => s.score })
    val msec = System.currentTimeMillis - start
    println(s"Selected: $max, simulations: $totalBoardCount(${(totalBoardCount.toDouble / msec).toInt}K/sec), playouts: $playouts(${(playouts.toDouble / msec * 1000).toInt}/sec), Elapsed: ${msec / 1000} sec")
    max.transition
  }
}
