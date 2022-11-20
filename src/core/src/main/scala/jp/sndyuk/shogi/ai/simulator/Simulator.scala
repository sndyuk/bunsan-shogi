package jp.sndyuk.shogi.ai.simulator

import java.util.LinkedList
import java.util.Queue

import scala.annotation.tailrec
import scala.reflect.ClassTag

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.FixedFifoArray
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.player.Utils

trait ScoreHolder {
  def playouts: Int
  def transition: Transition
}

case class Config[T<:ScoreHolder](maxDepth: Int, maxQueueSize: Int, thread: Int,
    onInit: Transition => T,
    onPlayout: (T, Boolean, Int) => T,
    onMaxDepth: T => T,
    select: List[T] => List[Score])
case class BoardState(board: Board, state: State, transition: Transition, depth: Int, index: Int)
case class Score(score: Double, playouts: Int, transition: Transition)

object Simulator {

  private def simulate[T<:ScoreHolder:ClassTag](board: Board, state: State, player: Turn, plans: List[Transition], conf: Config[T]): (List[Score], Int) = {
    val start = System.currentTimeMillis

    val scoresByPlan = plans.map(conf.onInit(_)).toArray
    val queue = new LinkedList[BoardState]() // Use Java's Queue since Scala's Queue is slow.
    val depth = state.history.size
    plans.zipWithIndex.foreach { p =>
      val boardCp = board.copy()
      val nextState = boardCp.move(state, p._1.oldPos, p._1.newPos, false, p._1.nari)
      queue.add(BoardState(boardCp, nextState, p._1, depth, p._2))
    }
    val totalBoardCount = simulate(player, queue, scoresByPlan, FixedFifoArray(conf.maxQueueSize / 2), 1, conf)

    val scores = conf.select(scoresByPlan.toList)

    val msec = System.currentTimeMillis - start
    println(s"Done thread ${Thread.currentThread().getName}: ${msec / 1000} sec")
    scores.foreach(println)
    (scores, totalBoardCount)
  }

  @tailrec private def simulate[T<:ScoreHolder:ClassTag](player: Turn, q: Queue[BoardState], plans: Array[T], lastNHash: FixedFifoArray[Long], boardCount: Int, conf: Config[T]): Int = {
    if (q.isEmpty) {
      return boardCount
    }
    val BoardState(board, state, transition, depth, i) = q.poll

    val plan = plans(i)
    if (depth == conf.maxDepth) {
      plans(i) = conf.onMaxDepth(plan)
    } else if (board.isFinish(state.turn)) {
      if (player == state.turn) {
        plans(i) = conf.onPlayout(plan, true, depth)
      } else {
        plans(i) = conf.onPlayout(plan, false, depth)
      }
    } else if (q.size < conf.maxQueueSize) {
      val plans = Utils.plans(board, state)

      plans.takeWhile(_ => q.size <= conf.maxQueueSize).foreach { p =>
        val boardCp = board.copy()
        val nextState = boardCp.move(state, p.oldPos, p.newPos, false, p.nari)
        if (lastNHash.contains(boardCp.id.hashLong)) {
          // Skip. It's already visited.
        } else {
          lastNHash.add(boardCp.id.hashLong)
          q.add(BoardState(boardCp, nextState, p, depth + 1, i))
        }
      }
    }

    return simulate(player, q, plans, lastNHash, boardCount + 1, conf)
  }

  def run[T<:ScoreHolder:ClassTag](board: Board, state: State, conf: Config[T]): Transition = {
    val plans = Utils.plans(board, state).toList
    run(board, state, plans, conf)
  }

  private[ai] def run[T<:ScoreHolder:ClassTag](board: Board, state: State, plans: List[Transition], conf: Config[T]): Transition = {
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
