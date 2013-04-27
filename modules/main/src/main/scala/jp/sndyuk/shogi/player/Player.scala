package jp.sndyuk.shogi.player

import jp.sndyuk.shogi.core._

trait Player {

  def next(board: Board, state: State): Transition

  // true: 移動可能判定, 成駒判定をする, falseの場合は自分でする
  def needValidation = true
}
