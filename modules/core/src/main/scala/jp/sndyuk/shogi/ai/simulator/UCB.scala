package jp.sndyuk.shogi.ai.simulator

import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import scala.collection.mutable.Queue

// Upper Confidence Bound
class UCB extends AI {

  private val ucbConst = 0.5d
  private val maxDepth = 125

  private val conf = Config(125, 30000, (scores: List[Plan]) => {
    scores.map { result =>
      val rate = result.win / Math.max(result.playouts, 1)
      val ucb = rate + ucbConst * Math.sqrt((2 * Math.log(result.playouts)) / result.playouts)
      Score(ucb, result.playouts, result.transition)
    }.sortBy(_.score)
  })

  def next(board: Board, state: State): Transition = {
    Simulator.run(board, state, conf)
  }
}
