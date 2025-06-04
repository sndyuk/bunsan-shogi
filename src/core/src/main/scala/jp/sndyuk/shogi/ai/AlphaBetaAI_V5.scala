package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

/**
 * AlphaBetaAI_V5 adds Null Move Pruning and killer move heuristics on top of
 * the V4 transposition table search, allowing deeper searches with better
 * pruning and move ordering.
 */
class AlphaBetaAI_V5(val name: String = "AlphaBetaAI_V5", searchDepth: Int) extends ShogiAI {

  private val MATE_SCORE_GUARD = 100

  override def findBestMove(
      state: State,
      board: Board,
      turn: Turn,
      currentSearchDepth: Int
  ): (Option[Transition], Long) = {
    var bestMove: Option[Transition] = None
    var nodesVisitedTotal: Long = 0L

    TranspositionTable.clear()
    AlphaBetaSearchNMP.clearKillers()

    var depth = 1
    while (depth <= currentSearchDepth) {
      val alpha = Int.MinValue + MATE_SCORE_GUARD
      val beta = Int.MaxValue - MATE_SCORE_GUARD
      val initialBoardID = ID(board)

      val (_, moveOpt, nodesVisited) = AlphaBetaSearchNMP.search(
        currentState = state,
        currentBoard = board,
        currentBoardID = initialBoardID,
        gamePathHistoryIDs = Nil,
        depth = depth,
        alpha = alpha,
        beta = beta,
        maximizingPlayer = true,
        rootPlayerTurn = turn,
        evalFunc = EvaluationV2.evaluate,
        transpositionTable = TranspositionTable
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
