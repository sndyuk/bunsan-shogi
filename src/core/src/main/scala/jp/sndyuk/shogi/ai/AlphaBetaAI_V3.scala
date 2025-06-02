package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

class AlphaBetaAI_V3(
    val name: String = "AlphaBetaAI_V3",
    searchDepth: Int,
    quiescenceSearchDepth: Int // Added quiescenceDepth parameter
) extends ShogiAI {

  override def findBestMove(
      state: State,
      board: Board,
      turn: Turn,
      currentSearchDepth: Int // Can override default from constructor if needed, or use this.searchDepth
  ): Option[Transition] = {

    val alpha = Int.MinValue + MATE_SCORE_GUARD
    val beta = Int.MaxValue - MATE_SCORE_GUARD
    val initialBoardID = ID(board)

    // Use the searchDepth provided in the constructor for this AI instance.
    // If currentSearchDepth from parameter is meant to override, that logic can be added.
    // For now, assume this.searchDepth is the primary depth.
    val depthToUse = if (currentSearchDepth > 0) currentSearchDepth else this.searchDepth

    val (_, bestMoveOpt) = AlphaBetaSearch.search(
      currentState = state,
      currentBoard = board,
      currentBoardID = initialBoardID,
      gamePathHistoryIDs = Nil,
      depth = depthToUse,
      quiescenceDepth = this.quiescenceSearchDepth, // Pass quiescenceDepth
      alpha = alpha,
      beta = beta,
      maximizingPlayer = true,
      rootPlayerTurn = turn,
      evalFunc = EvaluationV3.evaluate // Use EvaluationV3
    )

    bestMoveOpt
  }

  override def toString: String = s"$name(depth=$searchDepth, qDepth=$quiescenceSearchDepth)"

  // MATE_SCORE_GUARD can be a companion object constant or a private val.
  // For simplicity, keeping as private val like in V1.
  private val MATE_SCORE_GUARD = 100000
}
