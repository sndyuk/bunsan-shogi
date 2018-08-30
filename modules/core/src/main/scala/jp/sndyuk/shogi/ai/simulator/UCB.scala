package jp.sndyuk.shogi.ai.simulator



import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap



import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import scala.collection.mutable.Queue

case class BS(board: Board, state: State, transition: Transition, depth: Int, index: Int)

class UCB(private val maxQueueSize: Int = 30000) extends AI {

  private var maxDepth = 125
  private var maxBoard = 3000000

  private def simulate(board: Board, state: State, player: Turn, plans: List[Transition]): Transition = {
    val start = System.currentTimeMillis

    // _1 = number of wins
    // _2 = number of playouts
    // _3 = Transition
    val acc = plans.map((0d, 0, _)).toArray
    val queue = new Queue[BS]()
    plans.zipWithIndex.foreach { p =>
      queue += BS(board, state, p._1, 0, p._2)
    }
    val totalBoardCount = simulate(player, queue, acc, 1)

    val score = acc.map { result =>
      (result._1 / Math.max(result._2, 1), result._2, result._3)
    }.sortBy(_._1)

    val max = score.last

    val msec = System.currentTimeMillis - start
    score.foreach { s => println(f"${s._1}%1.5f, ${s._2}, ${s._3}") }
    val playouts = acc.foldLeft(0)(_ + _._2)
    println(s"Selected: $max, simulations: $totalBoardCount(${(totalBoardCount.toDouble / msec * 1000).toInt}/sec), playouts: $playouts(${(playouts.toDouble / msec * 1000).toInt}/sec), Elapsed: ${msec / 1000} sec")
    max._3
  }

  @tailrec private def simulate(player: Turn, q: Queue[BS], acc: Array[(Double, Int, Transition)], boardCount: Int): Int = {
    if (q.isEmpty || boardCount >= maxBoard) {
      return boardCount
    }
    val BS(board, state, transition, depth, i) = q.dequeue

    val boardCp = board.copy()
    val nextState = boardCp.move(state, transition.oldPos, transition.newPos, false, transition.nari)

    val score = acc(i)
    if (depth == maxDepth) {
      acc(i) = (score._1 - 1, score._2 + 1, score._3)
    } else if (boardCp.isFinish(state.turn)) {
      val vote = if (player == state.turn) maxDepth - depth else -(maxDepth - depth)
      val voted = score._1 + vote
      acc(i) = (voted, score._2 + 1, score._3)
    } else {
      acc(i) = (score._1, score._2, score._3)

      val plans = Utils.plans(boardCp, nextState)
      plans.takeWhile(_ => q.size <= maxQueueSize).foreach { p =>
         q += BS(boardCp, nextState, p, depth + 1, i)
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
    simulate(board.copy, state, state.turn, plans)
  }
}
