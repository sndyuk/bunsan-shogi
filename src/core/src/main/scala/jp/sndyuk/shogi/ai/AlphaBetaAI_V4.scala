package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

/**
 * AlphaBetaAI_V4 uses iterative deepening and a transposition table to search
 * deeper while reusing computations across branches. This typically yields
 * stronger play compared to previous versions.
 */
class AlphaBetaAI_V4(val name: String = "AlphaBetaAI_V4", searchDepth: Int) extends ShogiAI {

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

    var depth = 1
    while (depth <= currentSearchDepth) {
      val alpha = Int.MinValue + MATE_SCORE_GUARD
      val beta = Int.MaxValue - MATE_SCORE_GUARD
      val initialBoardID = ID(board)

      val (_, moveOpt, nodesVisited) = AlphaBetaSearchTT.search(
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
