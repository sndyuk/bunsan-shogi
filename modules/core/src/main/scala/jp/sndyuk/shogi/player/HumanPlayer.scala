package jp.sndyuk.shogi.player

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition

trait CommandReader {
  
  def read(state: State): Transition
}

class HumanPlayer(name: String, board: Board, reder: CommandReader, validation: Boolean) extends Player {

  def next(board: Board, state: State): Transition = { 
    reder.read(state)
  }
  
  override def needValidation = validation

  override def toString(): String = name
}