package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, ID}
import jp.sndyuk.shogi.player.Utils

/**
 * Simple quiescence search exploring only capturing moves.
 * Used by AlphaBetaAI_V6 to reduce the horizon effect.
 */
object QuiescenceSearch {
  private val MAX_DEPTH = 16

  def search(
      currentState: State,
      currentBoard: Board,
      currentBoardID: ID,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean,
      rootPlayerTurn: Turn,
      evalFunc: (Board, Turn) => Int,
      depth: Int = 0
  ): (Int, Long) = {
    var nodesVisited: Long = 1L

    if (depth >= MAX_DEPTH) {
      val standPatEval = evalFunc(currentBoard, rootPlayerTurn)
      return (standPatEval, nodesVisited)
    }

    // Stand pat evaluation from the perspective of rootPlayerTurn
    val standPat = evalFunc(currentBoard, rootPlayerTurn)
    var a = alpha
    var b = beta

    if (maximizingPlayer) {
      if (standPat > a) a = standPat
      if (a >= b) return (standPat, nodesVisited)
    } else {
      if (standPat < b) b = standPat
      if (b <= a) return (standPat, nodesVisited)
    }

    val captureMoves = Utils.plans(currentBoard, currentState).filter(_.captured.isDefined).toList

    if (maximizingPlayer) {
      var bestEval = standPat
      var alphaVar = a
      for (move <- captureMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (score, childNodes) = search(nextState, tempBoard, nextID, alphaVar, b, maximizingPlayer = false, rootPlayerTurn, evalFunc, depth + 1)
        nodesVisited += childNodes
        if (score > bestEval) bestEval = score
        if (score > alphaVar) alphaVar = score
        if (alphaVar >= b) return (bestEval, nodesVisited)
      }
      (bestEval, nodesVisited)
    } else {
      var bestEval = standPat
      var betaVar = b
      for (move <- captureMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (score, childNodes) = search(nextState, tempBoard, nextID, a, betaVar, maximizingPlayer = true, rootPlayerTurn, evalFunc, depth + 1)
        nodesVisited += childNodes
        if (score < bestEval) bestEval = score
        if (score < betaVar) betaVar = score
        if (betaVar <= a) return (bestEval, nodesVisited)
      }
      (bestEval, nodesVisited)
    }
  }
}
