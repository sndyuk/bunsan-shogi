package jp.sndyuk.shogi.core

trait Turn {
  def change: Turn
}

// 先手
case object PlayerA extends Turn {
  def change = PlayerB
  override def toString() = "▲"
}

// 後手
case object PlayerB extends Turn {
  def change = PlayerA
  override def toString() = "△"
}
