package jp.sndyuk.shogi.algorithm

import jp.sndyuk.shogi.core._

trait Algorithm extends Serializable {
  def execute(board: Board, state: State): Transition
}
