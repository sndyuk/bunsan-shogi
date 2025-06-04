package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

/**
 * AlphaBetaAI_V3 uses iterative deepening on top of the basic
 * alpha–beta search with EvaluationV2. The idea is that searching
 * shallow depths first helps move ordering, allowing a slightly
 * deeper effective search which results in stronger play.
 */
class AlphaBetaAI_V3(val name: String = "AlphaBetaAI_V3", searchDepth: Int) extends ShogiAI {

  private val MATE_SCORE_GUARD = 100

  override def findBestMove(
      state: State,
      board: Board,
      turn: Turn,
      currentSearchDepth: Int
  ): (Option[Transition], Long) = {
    var bestMove: Option[Transition] = None
    var nodesVisitedTotal: Long = 0L

    var depth = 1
    while (depth <= currentSearchDepth) {
      val alpha = Int.MinValue + MATE_SCORE_GUARD
      val beta = Int.MaxValue - MATE_SCORE_GUARD
      val initialBoardID = ID(board)

      val (_, moveOpt, nodesVisited) = AlphaBetaSearch.search(
        currentState = state,
        currentBoard = board,
        currentBoardID = initialBoardID,
        gamePathHistoryIDs = Nil,
        depth = depth,
        alpha = alpha,
        beta = beta,
        maximizingPlayer = true,
        rootPlayerTurn = turn,
        evalFunc = EvaluationV2.evaluate
      )

      nodesVisitedTotal += nodesVisited
      if (moveOpt.isDefined) {
        bestMove = moveOpt
      }

      depth += 1
    }

    (bestMove, nodesVisitedTotal)
  }

  override def toString: String = s"$name(depth=$searchDepth)"
}
