package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition}

class AlphaBetaAI_V1(val name: String = "AlphaBetaAI_V1", searchDepth: Int) extends ShogiAI {

  override def findBestMove(
      state: State,          // Current game state
      board: Board,          // Current board configuration
      turn: Turn,            // The player whose turn it is (should be this AI's turn)
      currentSearchDepth: Int // Search depth to use for this call (can override default)
  ): Option[Transition] = {

    // The 'turn' parameter here is the turn for which the AI is being asked to find a move.
    // This should align with the AI's own configured turn if it's a game play situation.
    // The AIPlayer passes its own 'this.turn' as the 'turn' argument here.
    // And it also passes it as rootPlayerTurn to AlphaBetaSearch.search.

    // Initial alpha and beta values
    // Using slightly offset values from MinValue/MaxValue can sometimes help avoid issues
    // if MATE_SCORE calculations result in exactly MinValue/MaxValue.
    val alpha = Int.MinValue + MATE_SCORE_GUARD // Guard against Int.MinValue - MATE_SCORE overflow if MATE_SCORE is large
    val beta = Int.MaxValue - MATE_SCORE_GUARD  // Guard against Int.MaxValue + MATE_SCORE overflow

    // Call the search. 'maximizingPlayer' is true because this AI (root player) wants to maximize its score.
    // 'turn' is passed as 'rootPlayerTurn' to ensure evaluation is from this AI's perspective.
    val (_, bestMoveOpt) = AlphaBetaSearch.search(
      currentState = state,
      currentBoard = board, // Pass the current board from argument
      depth = currentSearchDepth, // Use the passed-in depth from AIPlayer
      alpha = alpha,
      beta = beta,
      maximizingPlayer = true, // AI is the maximizing player at the root
      rootPlayerTurn = turn,   // Evaluate relative to this AI's turn (the player this AI instance plays for)
      evalFunc = EvaluationV1.evaluate // Use EvaluationV1
    )

    bestMoveOpt
  }

  override def toString: String = s"$name(depth=$searchDepth)"

  // Companion object to define constants or utility if needed
  // private object AlphaBetaAI_V1 { // Not strictly needed for MATE_SCORE_GUARD if it's a private val
  private val MATE_SCORE_GUARD = 100000 // Value to ensure alpha/beta don't immediately clip with MATE_SCORE
  // }
}
