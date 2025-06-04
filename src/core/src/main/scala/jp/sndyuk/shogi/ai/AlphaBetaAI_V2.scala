package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID} // Added ID

class AlphaBetaAI_V2(val name: String = "AlphaBetaAI_V2", searchDepth: Int) extends ShogiAI {

  // Guard value used for initializing alpha/beta to be slightly offset from Int.MinValue/MaxValue
  // to prevent issues if MATE_SCORE is exactly Int.MinValue/MaxValue or if eval functions return these.
  private val MATE_SCORE_GUARD = 100 // Can be small, just to provide buffer

  override def findBestMove(
      state: State,
      board: Board,
      turn: Turn,
      currentSearchDepth: Int // Use this depth for the search
  ): (Option[Transition], Long) = { // Changed return type

    // Initial alpha/beta for the root search.
    // AlphaBetaSearch itself will initialize its internal currentMaxEval/currentMinEval
    // to Int.MinValue/MaxValue respectively.
    val alpha = Int.MinValue + MATE_SCORE_GUARD
    val beta = Int.MaxValue - MATE_SCORE_GUARD

    val initialBoardID = ID(board) // Generate ID for the initial board state

    // Destructure to get nodesVisited
    val (_, bestMoveOpt, nodesVisited) = AlphaBetaSearch.search(
      currentState = state,
      currentBoard = board,
      currentBoardID = initialBoardID,
      gamePathHistoryIDs = Nil, // Initial call, path history is empty
      depth = currentSearchDepth,
      alpha = alpha,
      beta = beta,
      maximizingPlayer = true,
      rootPlayerTurn = turn,
      evalFunc = EvaluationV2.evaluate
    )

    (bestMoveOpt, nodesVisited) // Return nodesVisited
  }

  override def toString: String = s"$name(depth=$searchDepth)"
}
