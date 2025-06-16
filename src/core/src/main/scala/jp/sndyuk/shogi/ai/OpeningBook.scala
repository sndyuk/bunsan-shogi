package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{Board, ID, Transition}

/**
 * Very small built-in opening book.  It currently only
 * contains moves from the standard initial position.
 * Keys are board position hashes and values are the
 * recommended Transition from that position.
 */
object OpeningBook {
  private val book: Map[Long, Transition] = {
    val initBoard = Board()
    val firstMove = Transition(
      Board.humanReadableToPoint(7, 7),
      Board.humanReadableToPoint(7, 6),
      nari = false,
      captured = None
    )
    Map(ID(initBoard).hashLong -> firstMove)
  }

  /**
   * Returns an opening book move for the given board, if available.
   */
  def get(board: Board): Option[Transition] =
    book.get(ID(board).hashLong)
}
