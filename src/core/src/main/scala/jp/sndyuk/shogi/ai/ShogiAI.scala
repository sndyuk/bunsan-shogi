package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition}

trait ShogiAI {
  /**
   * Finds the best move for the given game state.
   *
   * @param state The current game state.
   * @param board The current board configuration (derived from state, or passed for efficiency).
   * @param turn The player whose turn it is.
   * @param currentSearchDepth The depth for the current search iteration.
   * @return A tuple containing an Option[Transition] representing the best move found (or None) and a Long representing the number of nodes visited.
   */
  def findBestMove(state: State, board: Board, turn: Turn, currentSearchDepth: Int): (Option[Transition], Long)
}
