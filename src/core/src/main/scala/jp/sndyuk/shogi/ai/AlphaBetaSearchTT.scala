package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{State, Board, Turn, Transition, ID, PlayerA, Piece}
import jp.sndyuk.shogi.player.Utils

/**
 * AlphaBeta search with a transposition table for caching results.
 * Includes a simple quiescence search and killer move heuristics
 * for improved move ordering.
 */
object AlphaBetaSearchTT {
  private val MATE_SCORE = 1000000
  private val MAX_QUIESCENCE_DEPTH = 5       // Max depth for quiescence search
  private val MAX_NOMINAL_SEARCH_DEPTH = 32   // Depth size for killer move table

  // Killer Moves: store two killer moves per depth
  private val killerMoves: Array[Array[Option[Transition]]] =
    Array.fill(MAX_NOMINAL_SEARCH_DEPTH, 2)(None)

  /** Clears the killer move table. */
  def clearKillerMoves(): Unit = {
    for (i <- killerMoves.indices) {
      killerMoves(i)(0) = None
      killerMoves(i)(1) = None
    }
  }

  // Score a move for ordering purposes (MVV-LVA for captures)
  private def scoreMoveForOrdering(move: Transition, board: Board, isCapture: Boolean): Int = {
    if (isCapture) {
      val capturedPiece = board.squares.get(move.newPos)
      val movingPiece   = board.squares.get(move.oldPos)
      if (capturedPiece != Piece.❏ && movingPiece != Piece.❏) {
        getNominalPieceValue(capturedPiece) * 100 - getNominalPieceValue(movingPiece)
      } else 0
    } else 0
  }

  // Nominal material values used for MVV-LVA ordering
  private def getNominalPieceValue(p: Piece): Int = {
    val pieceTypeOnly = p & Piece.bitsPiece
    pieceTypeOnly match {
      case Piece.▲.FU => 10
      case Piece.▲.KY => 30
      case Piece.▲.KE => 30
      case Piece.▲.GI => 40
      case Piece.▲.KI => 50
      case Piece.▲.KA => 80
      case Piece.▲.HI => 100
      case Piece.▲.OU => 10000
      case Piece.▲.TO => 50
      case Piece.▲.NY => 50
      case Piece.▲.NK => 50
      case Piece.▲.NG => 50
      case Piece.▲.UM => 120
      case Piece.▲.RY => 140
      case _ => 0
    }
  }

  /**
   * Quiescence search exploring only capture moves to avoid horizon effects.
   * Returns the evaluated score and the number of nodes visited.
   */
  private def quiescenceSearch(
      currentState: State,
      currentBoard: Board,
      alpha: Int,
      beta: Int,
      maximizingPlayer: Boolean,
      rootPlayerTurn: Turn,
      evalFunc: (Board, Turn) => Int,
      currentDepth: Int
  ): (Int, Long) = {
    var nodesVisited: Long = 1L

    // depth cutoff
    if (currentDepth >= MAX_QUIESCENCE_DEPTH)
      return (evalFunc(currentBoard, rootPlayerTurn), nodesVisited)

    // stand pat
    val standPat = evalFunc(currentBoard, rootPlayerTurn)
    nodesVisited += 1

    var a = alpha
    var b = beta
    if (maximizingPlayer) {
      if (standPat >= b) return (standPat, nodesVisited)
      a = math.max(a, standPat)
    } else {
      if (standPat <= a) return (standPat, nodesVisited)
      b = math.min(b, standPat)
    }

    val captureMovesRaw = Utils.plans(currentBoard, currentState).toList.filter { m =>
      val capturedPiece = currentBoard.squares.get(m.newPos)
      capturedPiece != Piece.❏ && Piece.▲△(capturedPiece, currentState.turn.change)
    }
    if (captureMovesRaw.isEmpty) return (standPat, nodesVisited)

    val orderedCaptures = captureMovesRaw
      .map(m => (m, scoreMoveForOrdering(m, currentBoard, isCapture = true)))
      .sortBy(-_._2)
      .map(_._1)

    if (maximizingPlayer) {
      var bestEval = standPat
      for (move <- orderedCaptures) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val (eval, child) = quiescenceSearch(nextState, tempBoard, a, b,
          maximizingPlayer = false, rootPlayerTurn, evalFunc, currentDepth + 1)
        nodesVisited += child
        bestEval = math.max(bestEval, eval)
        a = math.max(a, bestEval)
        if (b <= a) return (bestEval, nodesVisited)
      }
      (bestEval, nodesVisited)
    } else {
      var bestEval = standPat
      for (move <- orderedCaptures) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val (eval, child) = quiescenceSearch(nextState, tempBoard, a, b,
          maximizingPlayer = true, rootPlayerTurn, evalFunc, currentDepth + 1)
        nodesVisited += child
        bestEval = math.min(bestEval, eval)
        b = math.min(b, bestEval)
        if (b <= a) return (bestEval, nodesVisited)
      }
      (bestEval, nodesVisited)
    }
  }

  /**
   * Alpha-beta search with transposition table, killer moves, and quiescence at the leaf.
   */
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
      case Some(entry) =>
        return (entry.value, entry.bestMove, nodesVisited)
      case None => // continue
    }

    if (gamePathHistoryIDs.count(_ == currentBoardID) >= 2)
      return (0, None, nodesVisited)

    if (depth == 0) {
      val (qScore, qNodes) = quiescenceSearch(currentState, currentBoard,
                                              alpha, beta, maximizingPlayer,
                                              rootPlayerTurn, evalFunc, 0)
      nodesVisited += qNodes - 1
      return (qScore, None, nodesVisited)
    }

    val allMoves = Utils.plans(currentBoard, currentState).toList
    if (allMoves.isEmpty) {
      val score = if (currentState.turn == rootPlayerTurn) {
        -MATE_SCORE - depth
      } else {
        MATE_SCORE + depth
      }
      transpositionTable.put(ttKey, score, depth, None)
      return (score, None, nodesVisited)
    }

    // Score moves (for MVV-LVA ordering)
    val scoredMoves = allMoves.map { m =>
      val isCapture = currentBoard.squares.get(m.newPos) != Piece.❏ &&
        Piece.▲△(currentBoard.squares.get(m.newPos), currentState.turn.change)
      (m, isCapture, scoreMoveForOrdering(m, currentBoard, isCapture))
    }
    val mvvLvaSorted = scoredMoves.sortBy(-_._3).map(_._1)

    // Integrate killer moves
    var orderedMoves: List[Transition] = Nil
    val km1 = if (depth < MAX_NOMINAL_SEARCH_DEPTH) killerMoves(depth)(0) else None
    val km2 = if (depth < MAX_NOMINAL_SEARCH_DEPTH) killerMoves(depth)(1) else None
    km1.foreach { k => if (mvvLvaSorted.contains(k)) orderedMoves = k :: orderedMoves }
    km2.foreach { k => if (mvvLvaSorted.contains(k) && !orderedMoves.contains(k)) orderedMoves = k :: orderedMoves }
    orderedMoves = orderedMoves.reverse ++ mvvLvaSorted.filter(m => !orderedMoves.contains(m))

    if (maximizingPlayer) {
      var bestEval = Int.MinValue
      var bestMove: Option[Transition] = None
      var a = alpha
      for (move <- orderedMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (eval, _, child) = search(nextState, tempBoard, nextID,
          currentBoardID :: gamePathHistoryIDs, depth - 1, a, beta,
          maximizingPlayer = false, rootPlayerTurn, evalFunc, transpositionTable)
        nodesVisited += child
        if (eval > bestEval) {
          bestEval = eval
          bestMove = Some(move)
        }
        a = math.max(a, eval)
        if (beta <= a) {
          val isCap = currentBoard.squares.get(move.newPos) != Piece.❏ &&
            Piece.▲△(currentBoard.squares.get(move.newPos), currentState.turn.change)
          if (!isCap && depth < MAX_NOMINAL_SEARCH_DEPTH) {
            if (killerMoves(depth)(0).forall(_ != move)) {
              killerMoves(depth)(1) = killerMoves(depth)(0)
              killerMoves(depth)(0) = Some(move)
            }
          }
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
      for (move <- orderedMoves) {
        val tempBoard = currentBoard.copy()
        val nextState = tempBoard.move(currentState, move.oldPos, move.newPos, false, move.nari)
        val nextID = ID(tempBoard)
        val (eval, _, child) = search(nextState, tempBoard, nextID,
          currentBoardID :: gamePathHistoryIDs, depth - 1, alpha, b,
          maximizingPlayer = true, rootPlayerTurn, evalFunc, transpositionTable)
        nodesVisited += child
        if (eval < bestEval) {
          bestEval = eval
          bestMove = Some(move)
        }
        b = math.min(b, eval)
        if (b <= alpha) {
          val isCap = currentBoard.squares.get(move.newPos) != Piece.❏ &&
            Piece.▲△(currentBoard.squares.get(move.newPos), currentState.turn.change)
          if (!isCap && depth < MAX_NOMINAL_SEARCH_DEPTH) {
            if (killerMoves(depth)(0).forall(_ != move)) {
              killerMoves(depth)(1) = killerMoves(depth)(0)
              killerMoves(depth)(0) = Some(move)
            }
          }
          transpositionTable.put(ttKey, bestEval, depth, bestMove)
          return (bestEval, bestMove, nodesVisited)
        }
      }
      transpositionTable.put(ttKey, bestEval, depth, bestMove)
      (bestEval, bestMove, nodesVisited)
    }
  }
}
