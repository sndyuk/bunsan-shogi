package jp.sndyuk.shogi.core

object Transition {
  def apply(v: Int): Transition = {
    Transition(Point((v & 960) >>> 6, (v & 15360) >>> 10), Point((v & 245760) >>> 14, (v & 3932160) >>> 18), ((v & 32) >>> 5) == 1, if ((v & 31) > 0) Some(v & 31) else None)
  }
}

case class Transition(oldPos: Point, newPos: Point, nari: Boolean, captured: Option[Piece]) extends Serializable {
  override def toString(): String = {
    s"$oldPos->$newPos${if (nari) " 成" else ""}"
  }

  // 22bit
  def toInt(): Int = {
    // 1111 1111 1111 1111 1 11111
    newPos.x << 18 | newPos.y << 14 | oldPos.x << 10 | oldPos.y << 6 | (if (nari) 1 << 5 else 0) | captured.getOrElse(0)
  }
}
