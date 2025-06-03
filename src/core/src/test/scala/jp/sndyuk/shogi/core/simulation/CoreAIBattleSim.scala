package jp.sndyuk.shogi.core.simulation // Changed package

import jp.sndyuk.shogi.core._ // Wildcard for core types
import jp.sndyuk.shogi.ai._    // For ShogiAI, AlphaBetaAI_V1, AlphaBetaAI_V2
import jp.sndyuk.shogi.player.Utils // For jp.sndyuk.shogi.player.Utils.plans

object CoreAIBattleSim extends App { // Changed object name

  val MAX_MOVES = 200
  val SEARCH_DEPTH_AI1 = 2
  val SEARCH_DEPTH_AI2 = 2

  println("Shogi AI Battle Simulation (Core Version): AlphaBetaAI_V2 vs AlphaBetaAI_V2")

  var board = Board()
  board.init()
  var currentState = State(Nil, PlayerA)

  val ai1 = new AlphaBetaAI_V2(name = "AIv2_Sente_CoreSim", searchDepth = SEARCH_DEPTH_AI1)
  val ai2 = new AlphaBetaAI_V2(name = "AIv2_Gote_CoreSim", searchDepth = SEARCH_DEPTH_AI2)

  var gamePositionHistory: List[(String, String, Turn)] = List()
  var gameRunning = true
  var moveCount = 0

  gamePositionHistory = (board.squares.id(), board.capturedPieces.id(), currentState.turn) :: gamePositionHistory

  while (gameRunning && moveCount < MAX_MOVES) {
    moveCount += 1
    println(s"\n--- Turn ${currentState.turn}, Move #${moveCount} ---")
    println(board.toString)

    val currentAi: ShogiAI = if (currentState.turn == PlayerA) ai1 else ai2
    val currentAiPlayer = currentState.turn

    println(s"${currentAi.toString} is thinking...")

    val startTime = System.currentTimeMillis()
    val searchDepthForThisTurn = if (currentAiPlayer == PlayerA) SEARCH_DEPTH_AI1 else SEARCH_DEPTH_AI2
    val (bestMoveOpt, nodesVisited) = currentAi.findBestMove(currentState, board, currentAiPlayer, searchDepthForThisTurn)
    val endTime = System.currentTimeMillis()
    val thinkingTime = endTime - startTime
    println(s"Thinking time: ${thinkingTime}ms")
    val thinkingTimeSeconds = thinkingTime / 1000.0
    val nps = if (thinkingTimeSeconds > 0) nodesVisited / thinkingTimeSeconds else 0.0
    println(s"Nodes visited: $nodesVisited")
    println(s"NPS: ${"%.2f".format(nps)}")

    bestMoveOpt match {
      case Some(move) =>
        val pieceToMove = board.piece(move.oldPos, currentAiPlayer)

        println(s"${currentAiPlayer} (${Piece.name(pieceToMove)}) moves ${move.oldPos} -> ${move.newPos}" + (if(move.nari)" Nari" else ""))
        if (!Point.isCaptured(move.newPos)) {
            val pieceBeingCaptured = board.squares.get(move.newPos)
            if (pieceBeingCaptured != Piece.❏) {
                 println(s"Captured: ${Piece.name(pieceBeingCaptured)}")
            }
        }

        currentState = board.move(currentState, move.oldPos, move.newPos, false, move.nari)

        val newBoardSquaresId = board.squares.id()
        val newCapturedPiecesId = board.capturedPieces.id()
        val newPositionKey = (newBoardSquaresId, newCapturedPiecesId, currentState.turn)

        gamePositionHistory = newPositionKey :: gamePositionHistory
        val occurrences = gamePositionHistory.count(_ == newPositionKey)

        if (occurrences >= 4) {
          println(s"\nSENNICHITE! Position repeated 4 times. Game is a draw.")
          println(s"Board ID: $newBoardSquaresId, Captured ID: $newCapturedPiecesId, Turn: ${currentState.turn}")
          gameRunning = false
        } else {
          val playerWhoMadeTheMove = currentAiPlayer
          val nextPlayer = currentState.turn

          if (board.isFinish(playerWhoMadeTheMove)) {
            println(s"\nKING CAPTURED! Player ${playerWhoMadeTheMove} wins!")
            gameRunning = false
          } else {
            val nextPlayerLegalMoves = Utils.plans(board, currentState).toList // Use imported Utils
            if (nextPlayerLegalMoves.isEmpty) {
              if (Rule.isInCheck(board, nextPlayer)) {
                println(s"\nCHECKMATE! Player ${playerWhoMadeTheMove} wins! (Opponent ${nextPlayer} is in check and has no moves)")
              } else {
                println(s"\nSTALEMATE! Player ${nextPlayer} has no legal moves. Player ${playerWhoMadeTheMove} wins!")
              }
              gameRunning = false
            }
          }
        }
      case None =>
        println(s"\nNo moves for ${currentAiPlayer}! ${currentAiPlayer.change} wins by checkmate/stalemate!")
        gameRunning = false
    }
  }

  if (gameRunning && moveCount >= MAX_MOVES) {
    println(s"\nGame ended: Max moves ($MAX_MOVES) reached. Declaring draw or by score.")
    val finalEvalV1ForSente = EvaluationV1.evaluate(board, PlayerA)
    val finalEvalV2ForSente = EvaluationV2.evaluate(board, PlayerA)
    println(s"Final board eval for PlayerA (using AIv1 logic - V1 eval): $finalEvalV1ForSente")
    println(s"Final board eval for PlayerA (using AIv2 logic - V2 eval): $finalEvalV2ForSente")
  }

  println("\nSimulation finished.")
}
