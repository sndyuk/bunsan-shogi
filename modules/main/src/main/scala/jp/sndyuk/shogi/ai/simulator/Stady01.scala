package jp.sndyuk.shogi.ai.simulator

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.KifuStrage
import jp.sndyuk.shogi.core.ID
import scala.language.reflectiveCalls

class Stady01 extends AI {

  private val ucb = new UCB(Ordering.fromLessThan(_.board.id.hashCode < _.board.id.hashCode))

  def next(board: Board, state: State): Transition = {
    ucb.next(board, state)
  }
}
