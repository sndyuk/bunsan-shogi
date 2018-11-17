package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.ai.simulator.Config
import jp.sndyuk.shogi.ai.simulator.Score
import jp.sndyuk.shogi.ai.simulator.Simulator
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.ai.simulator.ScoreHolder

// Upper Confidence Bound
class UCB extends AI {

  case class Plan(var score: Double, var playouts: Int, transition: Transition) extends ScoreHolder

  def reward(depth: Int, won: Boolean): Double = {
    // Lesser depth, more rewards.

    // depth 25 : r = 0.5074
    // depth 50 : r = 0.3955
    // depth 100: r = 0.3034
    val weight = Math.sqrt((2 * Math.log(depth)) / depth)
    return if (won) weight else -weight
  }

  private val conf = Config(maxDepth = 150, maxQueueSize = 50000, thread = 2,
      onInit = (transition: Transition) => Plan(0d, 0, transition),
      onMaxDepth = (plan: Plan) => {
        plan.playouts += 1
        plan
      },
      onPlayout = (plan: Plan, won: Boolean, depth: Int) => {
        plan.playouts += 1
        plan.score += reward(depth, won)
        plan
      },
      select = (scores: List[Plan]) => {
        scores.map { result =>
          val playouts = Math.max(result.playouts, 1)
          // Lesser playouts, more rewards.
          val additionalReward = Math.sqrt(Math.log(playouts) / playouts)
          // Average reward + additional reward.
          val score = ((result.score + additionalReward) / playouts)
          Score(score, result.playouts, result.transition)
        }.sortBy(_.score)
      })

  def next(board: Board, state: State): Transition = {
    Simulator.run(board, state, conf)
  }
}
