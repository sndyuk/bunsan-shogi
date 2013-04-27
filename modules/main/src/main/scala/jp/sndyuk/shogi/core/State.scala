package jp.sndyuk.shogi.core

case class State(history: List[Transition] = Nil, turn: Turn = PlayerA) extends Serializable {
  def previous = State(history.tail, if (history.isEmpty) turn else turn.change)
}
