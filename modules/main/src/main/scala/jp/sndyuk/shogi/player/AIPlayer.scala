package jp.sndyuk.shogi.player

import scala.concurrent.duration.DurationInt
import jp.sndyuk.shogi.ai.simulator.AI
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.config
import jp.sndyuk.shogi.core.Transition

object AIPlayer {
  val aiName = config.getString("shogi.ai")

  lazy val ai: AI = Class.forName(aiName).newInstance.asInstanceOf[AI]
}

class AIPlayer extends Player {
  import AIPlayer._

  def next(board: Board, state: State): Transition = ai.next(board, state)

  override def needValidation = false
  override def toString(): String = aiName
}
