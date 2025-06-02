package jp.sndyuk.shogi.ui // Assuming this is a suitable package for sample apps

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.ai._ // For ShogiAI, AlphaBetaAI_V1, AlphaBetaAI_V2

object AIBattleSim extends App {

  val MAX_MOVES = 200 // Game ends after this many half-moves if no mate
  val SEARCH_DEPTH_AI1 = 3 // Increased depth
  val SEARCH_DEPTH_AI2 = 3 // Increased depth

  println("Shogi AI Battle Simulation: AlphaBetaAI_V1 vs AlphaBetaAI_V2")

  // Initialize board and state
  var board = Board() // Creates a standard initial board
  board.init() // Ensure board is initialized with pieces
  var currentState = State(Nil, PlayerA) // Player A (Sente) starts

  // Instantiate AIs
  val ai1 = new AlphaBetaAI_V1(name = "AIv1_Sente", searchDepth = SEARCH_DEPTH_AI1)
  val ai2 = new AlphaBetaAI_V2(name = "AIv2_Gote", searchDepth = SEARCH_DEPTH_AI2)

  var gamePositionHistory: List[(ID, Turn)] = List() // For Sennichite detection
  var gameRunning = true
  var moveCount = 0

  while (gameRunning && moveCount < MAX_MOVES) {
    moveCount += 1
    println(s"\n--- Turn ${currentState.turn}, Move #${moveCount} ---")
    println(board.toString) // Print current board state

    val currentAi: ShogiAI = if (currentState.turn == PlayerA) ai1 else ai2
    val currentAiPlayer = currentState.turn

    println(s"${currentAi.toString} is thinking...")

    val startTime = System.currentTimeMillis()
    // AIPlayer passes its own turn to findBestMove's 'turn' param.
    // The 'currentSearchDepth' for findBestMove will be AI's configured depth.
    val searchDepthForThisTurn = if (currentAiPlayer == PlayerA) SEARCH_DEPTH_AI1 else SEARCH_DEPTH_AI2
    val bestMoveOpt = currentAi.findBestMove(currentState, board, currentAiPlayer, searchDepthForThisTurn)
    val endTime = System.currentTimeMillis()
    println(s"Thinking time: ${endTime - startTime}ms")

    bestMoveOpt match {
      case Some(move) =>
        // Get piece before it moves for logging
        val pieceToMove = board.piece(move.oldPos, currentAiPlayer) // Use board.piece to handle board or hand

        println(s"${currentAiPlayer} (${Piece.name(pieceToMove)}) moves ${move.oldPos} -> ${move.newPos}" + (if(move.nari)" Nari" else ""))
        // Logging for captured piece on target square (if any)
        if (!Point.isCaptured(move.newPos)) { // Check only if the target is on the board
            val pieceBeingCaptured = board.squares.get(move.newPos) // Piece on target square BEFORE this move is made
            if (pieceBeingCaptured != Piece.❏) {
                 println(s"Captured: ${Piece.name(pieceBeingCaptured)}")
            }
        }
        // Removed the redundant/problematic second block of capture logging.

        // Apply move
        // currentAiPlayer is the player who is making the move.
        currentState = board.move(currentState, move.oldPos, move.newPos, false, move.nari) // validation=false as AI provides validated moves
        // After this, currentState.turn is the *next* player.

        val playerWhoMadeTheMove = currentAiPlayer // Player whose turn it just was
        val nextPlayer = currentState.turn      // Player whose turn it is now

        // Check if currentAiPlayer (who just moved) has now captured the opponent's King
        if (board.isFinish(playerWhoMadeTheMove)) {
          // board.isFinish(P) means: "Does player P have a King (necessarily opponent's) in hand?"
          println(s"\nKING CAPTURED! Player ${playerWhoMadeTheMove} wins!")
          gameRunning = false
        } else {
          // If no King was captured by playerWhoMadeTheMove, then check if the NEXT player (nextPlayer) has any moves.
          val nextPlayerLegalMoves = jp.sndyuk.shogi.player.Utils.plans(board, currentState).toList
          if (nextPlayerLegalMoves.isEmpty) {
            // If nextPlayer has no moves, check if they are in check.
            if (Rule.isInCheck(board, nextPlayer)) {
              println(s"\nCHECKMATE! Player ${playerWhoMadeTheMove} wins! (Opponent ${nextPlayer} is in check and has no moves)")
            } else {
              // No legal moves, but not in check: Stalemate.
              // In Shogi, this is typically a loss for the player with no moves.
              println(s"\nSTALEMATE! Player ${nextPlayer} has no legal moves. Player ${playerWhoMadeTheMove} wins!")
            }
            gameRunning = false
          }
        }

      case None =>
        // Current AI found no legal moves, implies it's checkmated or stalemated.
        println(s"\nNo moves for ${currentAiPlayer}! ${currentAiPlayer.change} wins by checkmate/stalemate!")
        gameRunning = false
    }
  } // end while loop

  if (gameRunning && moveCount >= MAX_MOVES) { // gameRunning check ensures we don't print this after a mate
    println(s"\nGame ended: Max moves ($MAX_MOVES) reached. Declaring draw or by score.")
    // Optionally, evaluate final board position here
    // Evaluation should be from a consistent perspective, e.g., PlayerA's
    val finalEvalV1ForSente = EvaluationV1.evaluate(board, PlayerA)
    val finalEvalV2ForSente = EvaluationV2.evaluate(board, PlayerA)
    println(s"Final board eval for PlayerA (using AIv1 logic - V1 eval): $finalEvalV1ForSente")
    println(s"Final board eval for PlayerA (using AIv2 logic - V2 eval): $finalEvalV2ForSente")
  }

  println("\nSimulation finished.")
}
