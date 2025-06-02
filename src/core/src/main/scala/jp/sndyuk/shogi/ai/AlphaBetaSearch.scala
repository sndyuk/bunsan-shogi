package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, Piece} // Added Piece back
import jp.sndyuk.shogi.player.Utils // For Utils.plans

object AlphaBetaSearch {

  // Defines a large value for checkmate, can be adjusted
  private val MATE_SCORE = 1000000
  // Quiescence search depth for checkmate checks, can be small
  // For now, no quiescence search, just checkmate at leaf if depth allows.

  def search(
      currentState: State, // Contains whose turn it is (currentState.turn)
      currentBoard: Board,
      depth: Int,
      alpha: Int, // Alpha: best score for maximizer found so far along the path to the root
      beta: Int,  // Beta: best score for minimizer found so far along the path to the root
      maximizingPlayer: Boolean, // Is the current node/depth for the maximizing player?
      rootPlayerTurn: Turn, // The AI player for whom we are searching at the root
      evalFunc: (Board, Turn) => Int // Evaluates from the perspective of rootPlayerTurn
  ): (Int, Option[Transition]) = {

    // TODO: Add checkmate detection if Rule.isCheckmate is available. For now, rely on depth and no moves.
    // A proper checkmate detection (e.g. using Rule.isTsumero or a similar function)
    // should be called here or when legalMoves.isEmpty is true.
    // If current player is checkmated, return MATE_SCORE for opponent.

    if (depth == 0) {
      val score = evalFunc(currentBoard, rootPlayerTurn)
      println(s"AlphaBeta DEBUG (depth 0, eval for $rootPlayerTurn): Evaluated score = $score")
      return (score, None)
    }

    // Utils.plans uses currentState.turn to determine whose moves to generate
    val legalMoves = Utils.plans(currentBoard, currentState).toList

    // ==== AlphaBeta DEBUGGING START ====
    println(s"AlphaBeta DEBUG (depth $depth, maximizing: $maximizingPlayer, turn: ${currentState.turn}): Found ${legalMoves.size} legal moves:")
    legalMoves.zipWithIndex.foreach { case (mv, idx) =>
      // Assuming Piece object has a name method for logging
      println(s"AlphaBeta DEBUG: Move $idx: ${mv.oldPos} -> ${mv.newPos}, Nari: ${mv.nari}, Captured: ${mv.captured.map(Piece.name)}")
    }
    if (legalMoves.exists(m => m.oldPos == m.newPos)) {
      println(s"AlphaBeta WARNING: 'move-to-self' present in legalMoves at depth $depth for turn ${currentState.turn}!")
    }
    // ==== AlphaBeta DEBUGGING END ====

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
      return (score, None) // No move to make
    }


    if (maximizingPlayer) { // Current player at this node is the same as rootPlayerTurn
      var currentMaxEval = Int.MinValue
      var bestMoveForThisNode: Option[Transition] = None
      var currentAlpha = alpha // currentAlpha should be initialized with the passed-in alpha

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        val (eval, _) = search(nextState, tempBoard, depth - 1, currentAlpha, beta, false, rootPlayerTurn, evalFunc)
        // Log before comparison
        println(s"AlphaBeta DEBUG (depth $depth, MAX): Move ${move.oldPos}->${move.newPos} got eval $eval. currentMaxEval was $currentMaxEval. currentAlpha was $currentAlpha, beta was $beta.")
        if (eval > currentMaxEval) {
          println(s"AlphaBeta DEBUG (depth $depth, MAX): Eval $eval > currentMaxEval $currentMaxEval. Updating best move.")
          currentMaxEval = eval
          bestMoveForThisNode = Some(move)
          println(s"AlphaBeta DEBUG (depth $depth, MAX): New bestMove: ${move.oldPos} -> ${move.newPos} with score $currentMaxEval")
        } else {
          println(s"AlphaBeta DEBUG (depth $depth, MAX): Eval $eval <= currentMaxEval $currentMaxEval. Not updating best move from ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)}.")
        }
        currentAlpha = Math.max(currentAlpha, eval)
        if (beta <= currentAlpha) {
          println(s"AlphaBeta DEBUG (depth $depth, MAX): BETA CUTOFF: beta ($beta) <= currentAlpha ($currentAlpha).")
          if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
            bestMoveForThisNode = Some(move)
            println(s"AlphaBeta DEBUG (depth $depth, MAX): BETA CUTOFF: bestMove was None, setting to current move ${move.oldPos} -> ${move.newPos}")
          }
          return (currentMaxEval, bestMoveForThisNode) // Beta cut-off
        }
      }
      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
        // If currentMaxEval is still MinValue, it means all moves resulted in MinValue or were pruned without setting a higher score.
        // In such a case, the score associated with legalMoves.head might not be currentMaxEval.
        // However, the problem asks to pick legalMoves.head if bestMoveForThisNode is None.
        // The score returned would be currentMaxEval, which could be MinValue.
        println(s"AlphaBeta DEBUG (depth $depth, MAX): Fallback: Chose legalMoves.head: ${legalMoves.head.oldPos} -> ${legalMoves.head.newPos} because no better move was found (score $currentMaxEval).")
      }
      println(s"AlphaBeta DEBUG (depth $depth, MAX): Loop finished. Returning score $currentMaxEval, move: ${bestMoveForThisNode.map(m => m.oldPos + "->" + m.newPos)}")
      return (currentMaxEval, bestMoveForThisNode)
    } else { // Current player at this node is the opponent of rootPlayerTurn
      var currentMinEval = Int.MaxValue
      var bestMoveForThisNode: Option[Transition] = None
      var currentBeta = beta // currentBeta should be initialized with the passed-in beta

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        val (eval, _) = search(nextState, tempBoard, depth - 1, alpha, currentBeta, true, rootPlayerTurn, evalFunc)
        // Log before comparison (for minimizer)
        println(s"AlphaBeta DEBUG (depth $depth, MIN): Move ${move.oldPos}->${move.newPos} got eval $eval. currentMinEval was $currentMinEval. alpha was $alpha, currentBeta was $currentBeta.")
        if (eval < currentMinEval) {
          currentMinEval = eval
          bestMoveForThisNode = Some(move)
          println(s"AlphaBeta DEBUG (depth $depth, MIN): New bestMove for opponent (move by ${currentState.turn}): ${move.oldPos} -> ${move.newPos} with score $eval (from root perspective)")
        } else {
            println(s"AlphaBeta DEBUG (depth $depth, MIN): Eval $eval >= currentMinEval $currentMinEval. Not updating best move from ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)}.")
        }
        currentBeta = Math.min(currentBeta, eval)
        if (currentBeta <= alpha) {
          println(s"AlphaBeta DEBUG (depth $depth, MIN): ALPHA CUTOFF: currentBeta ($currentBeta) <= alpha ($alpha).")
          if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
            bestMoveForThisNode = Some(move)
            println(s"AlphaBeta DEBUG (depth $depth, MIN): ALPHA CUTOFF: bestMove was None, setting to current move ${move.oldPos} -> ${move.newPos}")
          }
          return (currentMinEval, bestMoveForThisNode) // Alpha cut-off
        }
      }
      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
        // Similar to MAX node, if currentMinEval is MaxValue, this sets a move but score might be MaxValue.
        println(s"AlphaBeta DEBUG (depth $depth, MIN): Fallback: Chose legalMoves.head: ${legalMoves.head.oldPos} -> ${legalMoves.head.newPos} because no better move was found (score $currentMinEval).")
      }
      println(s"AlphaBeta DEBUG (depth $depth, MIN): Loop finished. Returning score $currentMinEval, move: ${bestMoveForThisNode.map(m => m.oldPos + "->" + m.newPos)}")
      return (currentMinEval, bestMoveForThisNode)
    }
  }
}
