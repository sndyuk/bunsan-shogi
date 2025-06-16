package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

/**
 * AlphaBetaAI_V5 builds upon V4 by integrating EvaluationV3 for improved heuristics
 * and enabling killer-move support in AlphaBetaSearchTT.
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
    AlphaBetaSearchTT.clearKillerMoves()

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
        evalFunc = EvaluationV3.evaluate,
        transpositionTable = TranspositionTable
      )

      nodesVisitedTotal += nodesVisited
      if (moveOpt.isDefined) bestMove = moveOpt

      depth += 1
    }

    (bestMove, nodesVisitedTotal)
  }

  override def toString: String = s"AlphaBetaAI_V5(name=$name, depth=$searchDepth)"
}
