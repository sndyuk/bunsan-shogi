package jp.sndyuk.shogi.ai.simulator

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.KifuStrage
import jp.sndyuk.shogi.core.ID
import scala.language.reflectiveCalls
import jp.sndyuk.shogi.algorithm.core.HashStorage

class AI01 extends AI {

  HashStorage.start

  def next(board: Board, state: State): Transition = {
    val id = ID(board)
    val transitions = KifuStrage.nextTransitions.getEffectiveTransitions(id, 1)
    val plans = Utils.plans(board, state).toList
    transitions match {
      case x :: xs => {
        println(s"Found effective transition: $x")
        x
      }
//      case _ => ucb.next(board, state, plans)
    }
  }
}
