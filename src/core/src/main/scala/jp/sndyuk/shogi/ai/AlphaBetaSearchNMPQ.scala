package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID, PlayerA, Rule, Piece}
import jp.sndyuk.shogi.player.Utils

/**
 * AlphaBeta search with transposition table, Null Move Pruning,
 * killer move heuristics and quiescence search at leaf nodes.
 */
object AlphaBetaSearchNMPQ {
  private val MATE_SCORE = 1000000
  private val NULL_REDUCTION = 2
  private val MAX_DEPTH = 64

  // Killer move heuristic: store up to two killer moves per depth
  private val killerMoves1 = Array.fill[Option[Transition]](MAX_DEPTH)(None)
  private val killerMoves2 = Array.fill[Option[Transition]](MAX_DEPTH)(None)

  private val pieceOrderingValue: Map[Int, Int] = Map(
    Piece.◯.FU -> 100,
    Piece.◯.KY -> 300,
    Piece.◯.KE -> 320,
    Piece.◯.GI -> 450,
    Piece.◯.KI -> 500,
    Piece.◯.KA -> 800,
    Piece.◯.HI -> 900,
    Piece.◯.OU -> Int.MaxValue
  ).withDefaultValue(0)

  def clearKillers(): Unit = {
    var i = 0
    while (i < MAX_DEPTH) {
      killerMoves1(i) = None
      killerMoves2(i) = None
      i += 1
    }
  }

  private def updateKillers(depth: Int, move: Transition): Unit = {
    if (!killerMoves1(depth).contains(move)) {
      killerMoves2(depth) = killerMoves1(depth)
      killerMoves1(depth) = Some(move)
    }
  }

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
    val ttEntryOpt = transpositionTable.get(ttKey, depth)
    ttEntryOpt match {
      case Some(entry) if entry.depth >= depth => return (entry.value, entry.bestMove, nodesVisited)
      case _ =>
    }
    val ttMove = ttEntryOpt.flatMap(_.bestMove)

    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2) {
      return (0, None, nodesVisited)
    }

    if (depth == 0) {
      val (score, qNodes) = QuiescenceSearch.search(currentState, currentBoard, currentBoardID, alpha, beta, maximizingPlayer, rootPlayerTurn, evalFunc, 0)
      nodesVisited += qNodes - 1
      transpositionTable.put(ttKey, score, depth, None)
      return (score, None, nodesVisited)
    }

    val legalMovesRaw = Utils.plans(currentBoard, currentState).toList
    val legalMoves = legalMovesRaw.sortBy { m =>
      var score = 0
      if (ttMove.contains(m)) score -= 2000
      if (killerMoves1(depth).contains(m)) score -= 1500
      if (killerMoves2(depth).contains(m)) score -= 1400
      m.captured match {
        case Some(pc) => score -= pieceOrderingValue(Piece.generalize(pc))
        case None =>
      }
      score
    }
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
          updateKillers(depth, move)
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
          updateKillers(depth, move)
          transpositionTable.put(ttKey, bestEval, depth, bestMove)
          return (bestEval, bestMove, nodesVisited)
        }
      }
      transpositionTable.put(ttKey, bestEval, depth, bestMove)
      (bestEval, bestMove, nodesVisited)
    }
  }
}
