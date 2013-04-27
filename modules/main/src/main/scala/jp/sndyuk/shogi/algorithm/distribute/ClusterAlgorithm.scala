package jp.sndyuk.shogi.algorithm.distribute

import jp.sndyuk.shogi.core._
import Piece._
import jp.sndyuk.shogi.algorithm.Algorithm

class ClusterAlgorithm extends Algorithm {
  def execute(board: Board, state: State): Transition = {
    Transition((1, 1), (1, 1), false, None)
  }
}