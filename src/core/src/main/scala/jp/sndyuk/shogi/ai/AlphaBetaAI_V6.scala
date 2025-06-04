package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID}

/**
 * AlphaBetaAI_V6 builds on V5 by adding a quiescence search at leaf nodes
 * of the Null Move Pruning search. This helps avoid the horizon effect
 * and typically results in stronger play.
 */
class AlphaBetaAI_V6(val name: String = "AlphaBetaAI_V6", searchDepth: Int) extends ShogiAI {

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
    AlphaBetaSearchNMPQ.clearKillers()

    var depth = 1
    while (depth <= currentSearchDepth) {
      val alpha = Int.MinValue + MATE_SCORE_GUARD
      val beta = Int.MaxValue - MATE_SCORE_GUARD
      val initialBoardID = ID(board)

      val (_, moveOpt, nodesVisited) = AlphaBetaSearchNMPQ.search(
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
