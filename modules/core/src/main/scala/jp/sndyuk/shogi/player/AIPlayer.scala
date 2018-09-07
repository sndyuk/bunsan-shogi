package jp.sndyuk.shogi.player

import jp.sndyuk.shogi.ai.AI
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.config

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
