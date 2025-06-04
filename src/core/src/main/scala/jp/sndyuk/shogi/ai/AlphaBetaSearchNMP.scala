package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID, PlayerA, Rule}
import jp.sndyuk.shogi.player.Utils

/**
 * AlphaBeta search with transposition table and Null Move Pruning.
 */
object AlphaBetaSearchNMP {
  private val MATE_SCORE = 1000000
  private val NULL_REDUCTION = 2

  def search(
      currentState: State,
      currentBoard: Board,
      currentBoardID: ID,
      gamePathHistoryIDs: List[ID],
      depth: Int,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean,
      rootPlayerTurn: Turn,
      evalFunc: (Board, Turn) => Int,
      transpositionTable: TranspositionTable.type
  ): (Int, Option[Transition], Long) = {

    var nodesVisited: Long = 1L

    val ttKey = currentBoardID.hashLong ^ (if (rootPlayerTurn == PlayerA) 0L else 1L)
    transpositionTable.get(ttKey, depth) match {
      case Some(entry) => return (entry.value, entry.bestMove, nodesVisited)
      case None        => // continue
    }

    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2) {
      return (0, None, nodesVisited)
    }

    if (depth == 0) {
      val score = evalFunc(currentBoard, rootPlayerTurn)
      transpositionTable.put(ttKey, score, depth, None)
      return (score, None, nodesVisited)
    }

    val legalMoves = Utils.plans(currentBoard, currentState).toList
    if (legalMoves.isEmpty) {
      val score = if (currentState.turn == rootPlayerTurn) {
        -MATE_SCORE - depth
      } else {
        MATE_SCORE + depth
      }
      transpositionTable.put(ttKey, score, depth, None)
      return (score, None, nodesVisited)
    }

    // Null Move Pruning
    if (depth >= NULL_REDUCTION + 1 && !Rule.isInCheck(currentBoard, currentState.turn)) {
      val nullState = currentState.copy(turn = currentState.turn.change)
      val (nullEval, _, nullNodes) = search(
        nullState,
        currentBoard,
        currentBoardID,
        currentBoardID :: gamePathHistoryIDs,
        depth - 1 - NULL_REDUCTION,
        alpha,
        beta,
        !maximizingPlayer,
        rootPlayerTurn,
        evalFunc,
        transpositionTable
      )
      nodesVisited += nullNodes
      if (maximizingPlayer && nullEval >= beta) {
        transpositionTable.put(ttKey, nullEval, depth, None)
        return (nullEval, None, nodesVisited)
      } else if (!maximizingPlayer && nullEval <= alpha) {
        transpositionTable.put(ttKey, nullEval, depth, None)
        return (nullEval, None, nodesVisited)
      }
    }

    if (maximizingPlayer) {
      var bestEval = Int.MinValue
      var bestMove: Option[Transition] = None
      var a = alpha
      for (move <- legalMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (eval, _, childNodes) = search(nextState, tempBoard, nextID,
          currentBoardID :: gamePathHistoryIDs,
          depth - 1, a, beta, maximizingPlayer = false,
          rootPlayerTurn, evalFunc, transpositionTable)
        nodesVisited += childNodes
        if (eval > bestEval) {
          bestEval = eval
          bestMove = Some(move)
        }
        a = math.max(a, eval)
        if (beta <= a) {
          transpositionTable.put(ttKey, bestEval, depth, bestMove)
          return (bestEval, bestMove, nodesVisited)
        }
      }
      transpositionTable.put(ttKey, bestEval, depth, bestMove)
      (bestEval, bestMove, nodesVisited)
    } else {
      var bestEval = Int.MaxValue
      var bestMove: Option[Transition] = None
      var b = beta
      for (move <- legalMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (eval, _, childNodes) = search(nextState, tempBoard, nextID,
          currentBoardID :: gamePathHistoryIDs,
          depth - 1, alpha, b, maximizingPlayer = true,
          rootPlayerTurn, evalFunc, transpositionTable)
        nodesVisited += childNodes
        if (eval < bestEval) {
          bestEval = eval
          bestMove = Some(move)
        }
        b = math.min(b, eval)
        if (b <= alpha) {
          transpositionTable.put(ttKey, bestEval, depth, bestMove)
          return (bestEval, bestMove, nodesVisited)
        }
      }
      transpositionTable.put(ttKey, bestEval, depth, bestMove)
      (bestEval, bestMove, nodesVisited)
    }
  }
}
