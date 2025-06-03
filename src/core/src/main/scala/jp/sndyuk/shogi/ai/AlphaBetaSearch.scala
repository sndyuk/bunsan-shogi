package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID} // Added ID
import jp.sndyuk.shogi.player.Utils // For Utils.plans

object AlphaBetaSearch {

  // Defines a large value for checkmate, can be adjusted
  private val MATE_SCORE = 1000000
  // Quiescence search depth for checkmate checks, can be small
  // For now, no quiescence search, just checkmate at leaf if depth allows.

  def search(
      currentState: State, // Contains whose turn it is (currentState.turn)
      currentBoard: Board,
      currentBoardID: ID, // ID of currentBoard
      gamePathHistoryIDs: List[ID], // IDs of states in the current search path from root
      depth: Int,
      alpha: Int, // Alpha: best score for maximizer found so far along the path to the root
      beta: Int,  // Beta: best score for minimizer found so far along the path to the root
      maximizingPlayer: Boolean, // Is the current node/depth for the maximizing player?
      rootPlayerTurn: Turn, // The AI player for whom we are searching at the root
      evalFunc: (Board, Turn) => Int // Evaluates from the perspective of rootPlayerTurn
  ): (Int, Option[Transition], Long) = { // Added Long for node count

    var nodesVisitedAccumulator: Long = 1L // Initialize node counter

    // Repetition check
    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2) {
      // This position (currentBoardID) has appeared at least twice before in the current path.
      // This means the current occurrence is the 3rd (or more) time.
      // Return a draw score (0) to discourage loops.
      return (0, None, nodesVisitedAccumulator) // Added nodesVisitedAccumulator
    }

    // TODO: Add checkmate detection if Rule.isCheckmate is available. For now, rely on depth and no moves.
    // A proper checkmate detection (e.g. using Rule.isTsumero or a similar function)
    // should be called here or when legalMoves.isEmpty is true.
    // If current player is checkmated, return MATE_SCORE for opponent.

    if (depth == 0) {
      val score = evalFunc(currentBoard, rootPlayerTurn)
      return (score, None, nodesVisitedAccumulator) // Added nodesVisitedAccumulator
    }

    // Utils.plans uses currentState.turn to determine whose moves to generate
    val legalMoves = Utils.plans(currentBoard, currentState).toList

    if (legalMoves.isEmpty) {
      // No legal moves. This is effectively a terminal node (checkmate or stalemate).
      // If maximizingPlayer is true, it means rootPlayerTurn (or the player whose turn it is at this node, if maximizingPlayer aligns with them) has no moves.
      // This is a checkmate against the player whose turn it is (currentState.turn).
      // The score should be from the perspective of rootPlayerTurn.
      // If currentState.turn == rootPlayerTurn (i.e., maximizingPlayer is true at this leaf for rootPlayerTurn),
      // and no moves are found, it's a checkmate against rootPlayerTurn. Score should be -MATE_SCORE.
      // If currentState.turn != rootPlayerTurn (i.e., maximizingPlayer is false at this leaf for rootPlayerTurn),
      // and no moves are found, it's a checkmate against the opponent. Score should be +MATE_SCORE.
      // The depth term is added to prefer faster checkmates.
      val score = if (currentState.turn == rootPlayerTurn) { // Current player to move is the one we are maximizing for at root
        -MATE_SCORE - depth // Checkmated, bad for rootPlayerTurn
      } else {
        MATE_SCORE + depth  // Opponent is checkmated, good for rootPlayerTurn
      }
      return (score, None, nodesVisitedAccumulator) // No move to make, Added nodesVisitedAccumulator
    }


    if (maximizingPlayer) { // Current player at this node is the same as rootPlayerTurn
      var currentMaxEval = Int.MinValue
      var bestMoveForThisNode: Option[Transition] = None
      var currentAlpha = alpha // currentAlpha should be initialized with the passed-in alpha

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        val nextBoardID = ID(tempBoard) // Generate ID for the new board state
        val (eval, returnedMoveOpt, childNodesVisited) = search(nextState, tempBoard, nextBoardID, // Capture childNodesVisited
                                             currentBoardID :: gamePathHistoryIDs, // Prepend current ID to history for child
                                             depth - 1, currentAlpha, beta, false, rootPlayerTurn, evalFunc)
        nodesVisitedAccumulator += childNodesVisited // Accumulate child nodes

        if (eval > currentMaxEval) {
          currentMaxEval = eval
          bestMoveForThisNode = Some(move)
        }

        currentAlpha = Math.max(currentAlpha, eval)

        if (beta <= currentAlpha) {
          // Ensure a move is returned if pruning.
          if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
             if (move == legalMoves.head) {
                bestMoveForThisNode = Some(move)
             } else {
                if (bestMoveForThisNode.isEmpty) bestMoveForThisNode = Some(legalMoves.head)
             }
          }
          return (currentMaxEval, bestMoveForThisNode, nodesVisitedAccumulator) // Beta cut-off, Added nodesVisitedAccumulator
        }
      } // end for loop

      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
      }
      return (currentMaxEval, bestMoveForThisNode, nodesVisitedAccumulator) // Added nodesVisitedAccumulator
    } else { // MINIMIZING PLAYER
      var currentMinEval = Int.MaxValue
      var bestMoveForThisNode: Option[Transition] = None
      var currentBeta = beta

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        val nextBoardID = ID(tempBoard) // Generate ID for the new board state
        val (eval, returnedMoveOpt, childNodesVisited) = search(nextState, tempBoard, nextBoardID, // Capture childNodesVisited
                                             currentBoardID :: gamePathHistoryIDs, // Prepend current ID to history for child
                                             depth - 1, alpha, currentBeta, true, rootPlayerTurn, evalFunc)
        nodesVisitedAccumulator += childNodesVisited // Accumulate child nodes

        if (eval < currentMinEval) {
            currentMinEval = eval
            bestMoveForThisNode = Some(move)
        }
        currentBeta = Math.min(currentBeta, eval)

        if (currentBeta <= alpha) {
            if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) { // Similar pruning fallback
                if (move == legalMoves.head) {
                    bestMoveForThisNode = Some(move)
                } else if (bestMoveForThisNode.isEmpty) {
                    bestMoveForThisNode = Some(legalMoves.head)
                }
            }
            return (currentMinEval, bestMoveForThisNode, nodesVisitedAccumulator) // Alpha cut-off, Added nodesVisitedAccumulator
        }
      }
      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
      }
      return (currentMinEval, bestMoveForThisNode, nodesVisitedAccumulator) // Added nodesVisitedAccumulator
    }
  }
}
