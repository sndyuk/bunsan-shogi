package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition}

/**
 * AlphaBetaAI_V7 extends V6 by consulting a small
 * built-in opening book before running the main search.
 * If the current board position is found in the book,
 * the recommended move is played immediately.
 */
class AlphaBetaAI_V7(val name: String = "AlphaBetaAI_V7", searchDepth: Int) extends ShogiAI {

  private val coreAI = new AlphaBetaAI_V6(name + "_core", searchDepth)

  override def findBestMove(
      state: State,
      board: Board,
      turn: Turn,
      currentSearchDepth: Int
  ): (Option[Transition], Long) = {
    OpeningBook.get(board) match {
      case Some(bookMove) => (Some(bookMove), 0L)
      case None => coreAI.findBestMove(state, board, turn, currentSearchDepth)
    }
  }

  override def toString: String = s"$name(depth=$searchDepth)"
}
