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
      quiescenceDepth: Int, // Added quiescenceDepth
      alpha: Int, // Alpha: best score for maximizer found so far along the path to the root
      beta: Int,  // Beta: best score for minimizer found so far along the path to the root
      maximizingPlayer: Boolean, // Is the current node/depth for the maximizing player?
      rootPlayerTurn: Turn, // The AI player for whom we are searching at the root
      evalFunc: (Board, Turn) => Int // Evaluates from the perspective of rootPlayerTurn
  ): (Int, Option[Transition]) = {

    // Repetition check
    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2) {
      // This position (currentBoardID) has appeared at least twice before in the current path.
      // This means the current occurrence is the 3rd (or more) time.
      // Return a draw score (0) to discourage loops.
      // println(s"AlphaBeta DEBUG (depth $depth): Repetition detected for ID ${currentBoardID.toString.take(6)}... Draw score 0.")
      return (0, None)
    }

    // TODO: Add checkmate detection if Rule.isCheckmate is available. For now, rely on depth and no moves.
    // A proper checkmate detection (e.g. using Rule.isTsumero or a similar function)
    // should be called here or when legalMoves.isEmpty is true.
    // If current player is checkmated, return MATE_SCORE for opponent.

    if (depth == 0) {
      // Depth 0, start quiescence search
      return quiescenceSearch(currentState, currentBoard, currentBoardID, gamePathHistoryIDs, quiescenceDepth, alpha, beta, maximizingPlayer, rootPlayerTurn, evalFunc)
    }

    // Utils.plans uses currentState.turn to determine whose moves to generate
    val legalMoves = Utils.plans(currentBoard, currentState).toList

    // ==== AlphaBeta DEBUGGING START ====
    // println(s"AlphaBeta DEBUG (depth $depth, maximizing: $maximizingPlayer, turn: ${currentState.turn}): Found ${legalMoves.size} legal moves:") // Restored
    // legalMoves.zipWithIndex.foreach { case (mv, idx) =>
      // Assuming Piece object has a name method for logging
      // println(s"AlphaBeta DEBUG: Move $idx: ${mv.oldPos} -> ${mv.newPos}, Nari: ${mv.nari}, Captured: ${mv.captured.map(Piece.name)}") // Restored
    // }
    // if (legalMoves.exists(m => m.oldPos == m.newPos)) {
      // println(s"AlphaBeta WARNING: 'move-to-self' present in legalMoves at depth $depth for turn ${currentState.turn}!") // Restored
    // }
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

      // Log initial state for this node
      // if (depth == 2) { // Assuming root call for this test is depth 2 // Restored
          // println(s"ROOT MAX NODE (depth $depth, turn ${currentState.turn}): Initial alpha=$currentAlpha, beta=$beta") // Restored
          // println(s"ROOT MAX NODE: Legal moves count: ${legalMoves.size}") // Restored
          // legalMoves.zipWithIndex.foreach { case (mv, idx) => // Restored
            // println(s"ROOT MAX NODE: Legal move $idx: ${mv.oldPos} -> ${mv.newPos}, Nari: ${mv.nari}") // Restored
          // } // Restored
      // } // Restored

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        // if (depth == 2) { // Restored
            // println(s"ROOT MAX NODE: Simulating move ${move.oldPos} -> ${move.newPos}") // Restored
        // } // Restored

        val nextBoardID = ID(tempBoard) // Generate ID for the new board state
        val (eval, returnedMoveOpt) = search(nextState, tempBoard, nextBoardID,
                                             currentBoardID :: gamePathHistoryIDs, // Prepend current ID to history for child
                                             depth - 1, quiescenceDepth, currentAlpha, beta, false, rootPlayerTurn, evalFunc)

        // if (depth == 2) { // Logging for root node's decision process // Restored
            // println(s"ROOT MAX NODE: Move ${move.oldPos}->${move.newPos} (child chose ${returnedMoveOpt.map(m=>m.oldPos+"->"+m.newPos)}) resulted in eval $eval.") // Restored
            // println(s"ROOT MAX NODE: Comparing eval $eval with currentMaxEval $currentMaxEval.") // Restored
        // } // Restored

        if (eval > currentMaxEval) {
          // if (depth == 2) { // Restored
            // println(s"ROOT MAX NODE: New best! eval $eval > currentMaxEval $currentMaxEval. Updating best move to ${move.oldPos} -> ${move.newPos}") // Restored
          // } // Restored
          currentMaxEval = eval
          bestMoveForThisNode = Some(move)
        } else {
          // if (depth == 2) { // Restored
            // println(s"ROOT MAX NODE: No update. eval $eval <= currentMaxEval $currentMaxEval. Best still ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)} (score $currentMaxEval)") // Restored
          // } // Restored
        }

        currentAlpha = Math.max(currentAlpha, eval)
        // if (depth == 2) { // Restored
            // println(s"ROOT MAX NODE: Updated currentAlpha to $currentAlpha.") // Restored
        // } // Restored

        if (beta <= currentAlpha) {
          // if (depth == 2) { // Restored
            // println(s"ROOT MAX NODE: BETA CUTOFF! beta ($beta) <= currentAlpha ($currentAlpha). Returning $currentMaxEval for move ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)}") // Restored
          // } // Restored
          // Ensure a move is returned if pruning.
          if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
             if (move == legalMoves.head) {
                bestMoveForThisNode = Some(move)
             } else {
                if (bestMoveForThisNode.isEmpty) bestMoveForThisNode = Some(legalMoves.head)
             }
            //  if (depth == 2 && bestMoveForThisNode.contains(move)) { // Restored
                // println(s"ROOT MAX NODE: BETA CUTOFF: Best move was None or not better than current, set to current move ${move.oldPos} -> ${move.newPos}") // Restored
            //  } else if (depth == 2 && bestMoveForThisNode.contains(legalMoves.head)) { // Restored
                //  println(s"ROOT MAX NODE: BETA CUTOFF: Best move was None, set to legalMoves.head ${legalMoves.head.oldPos} -> ${legalMoves.head.newPos}") // Restored
            //  } // Restored
          }
          return (currentMaxEval, bestMoveForThisNode) // Beta cut-off
        }
      } // end for loop

      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
        // if (depth == 2) { // Corrected from (depth $depth, MAX) to (depth == 2) // Restored
          // println(s"ROOT MAX NODE (depth $depth): Fallback post-loop: Chose legalMoves.head: ${legalMoves.head.oldPos} -> ${legalMoves.head.newPos} because bestMove was None (score $currentMaxEval).") // Restored
        // } // Restored
      }
      // if (depth == 2) { // Restored
        // println(s"ROOT MAX NODE (depth $depth): Loop finished. Returning score $currentMaxEval, move: ${bestMoveForThisNode.map(m => m.oldPos + "->" + m.newPos)}") // Restored
      // } // Restored
      return (currentMaxEval, bestMoveForThisNode)
    } else { // MINIMIZING PLAYER (Gote's turn at depth 1 for this test)
      var currentMinEval = Int.MaxValue
      var bestMoveForThisNode: Option[Transition] = None
      var currentBeta = beta

      // if (depth == 1) { // Logging for Gote's decision node // Restored
           // println(s"MIN NODE (depth $depth, turn ${currentState.turn}, rootTurn $rootPlayerTurn): Initial alpha=$alpha, currentBeta=$currentBeta") // Restored
           // println(s"MIN NODE: Legal moves count: ${legalMoves.size}") // Restored
           // legalMoves.zipWithIndex.foreach { case (mv, idx) => // Restored
              // println(s"MIN NODE: Legal move $idx: ${mv.oldPos} -> ${mv.newPos}, Nari: ${mv.nari}, Captured: ${mv.captured.map(Piece.name)}") // Restored
           // } // Restored
      // } // Restored

      for (move <- legalMoves) { // Iterate ALL legal moves
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)

        // if (depth == 1) { // Restored
            // println(s"MIN NODE: Simulating Gote move ${move.oldPos} -> ${move.newPos}") // Restored
        // } // Restored

        val nextBoardID = ID(tempBoard) // Generate ID for the new board state
        val (eval, returnedMoveOpt) = search(nextState, tempBoard, nextBoardID,
                                             currentBoardID :: gamePathHistoryIDs, // Prepend current ID to history for child
                                             depth - 1, quiescenceDepth, alpha, currentBeta, true, rootPlayerTurn, evalFunc)

        // if (depth == 1) { // Restored
            // println(s"MIN NODE: Gote Move ${move.oldPos}->${move.newPos} (child Sente MAX node chose ${returnedMoveOpt.map(m=>m.oldPos+"->"+m.newPos)}) resulted in eval $eval (Sente's perspective).") // Restored
            // println(s"MIN NODE: Comparing eval $eval with currentMinEval $currentMinEval.") // Restored
        // } // Restored
        if (eval < currentMinEval) {
            // if (depth == 1) println(s"MIN NODE: New best for Gote! eval $eval < currentMinEval $currentMinEval. Updating Gote's choice to ${move.oldPos} -> ${move.newPos} (score for Sente will be $eval)") // Restored
            currentMinEval = eval
            bestMoveForThisNode = Some(move)
        } else {
            //  if (depth == 1) println(s"MIN NODE: No update for Gote. eval $eval >= currentMinEval $currentMinEval. Gote's best choice for Sente is still ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)} (score $currentMinEval for Sente)") // Restored
        }
        currentBeta = Math.min(currentBeta, eval)
        // if (depth == 1) println(s"MIN NODE: Updated currentBeta to $currentBeta.") // Restored

        if (currentBeta <= alpha) {
            // if (depth == 1) println(s"MIN NODE: ALPHA CUTOFF! currentBeta ($currentBeta) <= alpha ($alpha). Returning $currentMinEval for Gote move ${bestMoveForThisNode.map(m=>m.oldPos+"->"+m.newPos)}") // Restored
            if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) { // Similar pruning fallback
                if (move == legalMoves.head) {
                    bestMoveForThisNode = Some(move)
                } else if (bestMoveForThisNode.isEmpty) {
                    bestMoveForThisNode = Some(legalMoves.head)
                }
            }
            return (currentMinEval, bestMoveForThisNode) // Alpha cut-off
        }
      }
      if (bestMoveForThisNode.isEmpty && legalMoves.nonEmpty) {
        bestMoveForThisNode = Some(legalMoves.head)
        // if (depth == 1) println(s"MIN NODE (depth $depth): Fallback post-loop: Gote chose legalMoves.head: ${legalMoves.head.oldPos} -> ${legalMoves.head.newPos} because bestMove was None (resulting Sente score $currentMinEval).") // Restored
      }
      // if (depth == 1) { // Restored
        // println(s"MIN NODE (depth $depth): Loop finished. Gote returns score $currentMinEval (for Sente), Gote's chosen move: ${bestMoveForThisNode.map(m => m.oldPos + "->" + m.newPos)}") // Restored
      // } // Restored
      return (currentMinEval, bestMoveForThisNode)
    }
  }

  // Quiescence Search Implementation
  private def quiescenceSearch(
      currentState: State,
      currentBoard: Board,
      currentBoardID: ID,
      gamePathHistoryIDs: List[ID],
      quiescenceDepth: Int,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean,
      rootPlayerTurn: Turn,
      evalFunc: (Board, Turn) => Int
  ): (Int, Option[Transition]) = {

    // Repetition check (important for quiescence too, though less likely with only captures)
    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2) {
      return (0, None) // Draw score for repetitions
    }

    // Check for checkmate before quiescence depth check, as mate is a terminal state.
    // This also handles the case where quiescenceDepth might be > 0 but no moves are possible.
    val allLegalMoves = Utils.plans(currentBoard, currentState).toList
    if (allLegalMoves.isEmpty) {
      // No legal moves at all (checkmate or stalemate)
      // Score from the perspective of rootPlayerTurn
      val score = if (currentState.turn == rootPlayerTurn) {
        -MATE_SCORE - quiescenceDepth // Current player (root) is checkmated
      } else {
        MATE_SCORE + quiescenceDepth  // Opponent is checkmated
      }
      return (score, None) // No move to make
    }

    // Base case for quiescence: depth limit reached (and not a checkmate)
    if (quiescenceDepth == 0) {
      return (evalFunc(currentBoard, rootPlayerTurn), None)
    }

    // Filter for capture moves from the already fetched allLegalMoves
    val captureMoves = allLegalMoves.filter(_.captured.isDefined)

    // If no capture moves (but other non-capture moves exist), evaluate the position statically
    // This is the true "quiet" position.
    if (captureMoves.isEmpty) {
      return (evalFunc(currentBoard, rootPlayerTurn), None)
    }

    // Similar logic to the main search, but only for capture moves
    if (maximizingPlayer) {
      var currentMaxEval = Int.MinValue
      var currentAlpha = alpha

      // Initial evaluation of the standing position (score if no capture is made or if all captures are bad)
      val standingPatScore = evalFunc(currentBoard, rootPlayerTurn)
      currentMaxEval = standingPatScore // Initialize with standing pat score

      currentAlpha = Math.max(currentAlpha, currentMaxEval)
      if (beta <= currentAlpha) {
          return (currentMaxEval, None)
      }

      for (move <- captureMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextBoardID = ID(tempBoard)

        val (eval, _) = quiescenceSearch(nextState, tempBoard, nextBoardID,
                                         currentBoardID :: gamePathHistoryIDs,
                                         quiescenceDepth - 1, currentAlpha, beta, false, rootPlayerTurn, evalFunc)

        if (eval > currentMaxEval) {
          currentMaxEval = eval
        }
        currentAlpha = Math.max(currentAlpha, eval)
        if (beta <= currentAlpha) {
          return (currentMaxEval, None) // Beta cut-off, move itself is not propagated up
        }
      }
      return (currentMaxEval, None) // Return best score found, move itself is not propagated up
    } else { // Minimizing player
      var currentMinEval = Int.MaxValue
      var currentBeta = beta

      // Initial evaluation of the standing position
      val standingPatScore = evalFunc(currentBoard, rootPlayerTurn)
      currentMinEval = standingPatScore // Initialize with standing pat score

      currentBeta = Math.min(currentBeta, currentMinEval)
      if (currentBeta <= alpha) {
          return (currentMinEval, None)
      }

      for (move <- captureMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextBoardID = ID(tempBoard)

        val (eval, _) = quiescenceSearch(nextState, tempBoard, nextBoardID,
                                         currentBoardID :: gamePathHistoryIDs,
                                         quiescenceDepth - 1, alpha, currentBeta, true, rootPlayerTurn, evalFunc)

        if (eval < currentMinEval) {
          currentMinEval = eval
        }
        currentBeta = Math.min(currentBeta, eval)
        if (currentBeta <= alpha) {
          return (currentMinEval, None) // Alpha cut-off, move itself is not propagated up
        }
      }
      return (currentMinEval, None) // Return best score found, move itself is not propagated up
    }
  }
}
