package jp.sndyuk.shogi.core

case class State(history: List[Transition] = Nil, turn: Turn = PlayerA) {
  def next(transition: Transition) = State(transition :: history, turn.change)
  def previous = State(history.tail, if (history.isEmpty) turn else turn.change)
}
