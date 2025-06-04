package jp.sndyuk.shogi.ui // Assuming this is a suitable package for sample apps

// This object runs a simulation of a Shogi game between two AI players.
// It demonstrates how to set up a game, run the game loop, handle AI moves,
// and check for game end conditions such as checkmate, stalemate, Sennichite (four-fold repetition),
// or reaching a maximum move limit.

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.ai._ // For ShogiAI, AlphaBetaAI_V1, AlphaBetaAI_V2

object AIBattleSim extends App {

  // MAX_MOVES defines the maximum number of half-moves the game will run for.
  // If the game reaches this limit without a checkmate, stalemate, or king capture,
  // it's typically considered a draw or decided by evaluating the final board position.
  val MAX_MOVES = 200 // Game ends after this many half-moves if no mate

  // SEARCH_DEPTH_AI1 defines the search depth for the first AI player (Sente).
  // A higher value generally means stronger play but requires more computation time.
  val SEARCH_DEPTH_AI1 = 2 // Depth 2 for faster simulation

  // SEARCH_DEPTH_AI2 defines the search depth for the second AI player (Gote).
  val SEARCH_DEPTH_AI2 = 2 // Depth 2 for faster simulation

  // Instantiate AIs
  // You can choose different AI implementations here, for example:
  // val ai1 = new AlphaBetaAI_V1(name = "AIv1_Sente", searchDepth = SEARCH_DEPTH_AI1)
  // val ai2 = new AlphaBetaAI_V1(name = "AIv1_Gote", searchDepth = SEARCH_DEPTH_AI2)
  // Or mix them:
  // val ai1 = new AlphaBetaAI_V2(name = "AIv2_Sente", searchDepth = SEARCH_DEPTH_AI1)
  // val ai2 = new AlphaBetaAI_V1(name = "AIv1_Gote", searchDepth = SEARCH_DEPTH_AI2)
  // The search depths (SEARCH_DEPTH_AI1, SEARCH_DEPTH_AI2) can be adjusted to change AI strength.
  val ai1 = new AlphaBetaAI_V2(name = "AIv2_Sente", searchDepth = SEARCH_DEPTH_AI1)
  val ai2 = new AlphaBetaAI_V2(name = "AIv2_Gote", searchDepth = SEARCH_DEPTH_AI2)

  println(s"Simulating: ${ai1.toString} (Depth ${SEARCH_DEPTH_AI1}) vs ${ai2.toString} (Depth ${SEARCH_DEPTH_AI2})")

  // Initialize board and state
  // A new Board() object is created, representing the Shogi board.
  var board = Board() // Creates a standard initial board
  // board.init() places all the pieces in their standard starting positions.
  board.init() // Ensure board is initialized with pieces
  // currentState holds the game's state, including whose turn it is and captured pieces (initially empty).
  // PlayerA (Sente) is set to make the first move.
  var currentState = State(Nil, PlayerA) // Player A (Sente) starts

  // gamePositionHistory is used to detect Sennichite (four-fold repetition).
  // It stores a history of board states (pieces on board, pieces in hand, and current turn).
  // Each element is a tuple: (squares.id, capturedPieces.id, turnToPlay).
  var gamePositionHistory: List[(String, String, Turn)] = List()
  var gameRunning = true
  var moveCount = 0

  // Record initial position for Sennichite check
  gamePositionHistory = (board.squares.id(), board.capturedPieces.id(), currentState.turn) :: gamePositionHistory

  // Main game loop: continues as long as 'gameRunning' is true and 'moveCount' is less than 'MAX_MOVES'.
  while (gameRunning && moveCount < MAX_MOVES) {
    moveCount += 1
    println(s"\n--- Turn ${currentState.turn}, Move #${moveCount} ---")
    println(board.toString) // Print current board state

    // Determine which AI's turn it is.
    val currentAi: ShogiAI = if (currentState.turn == PlayerA) ai1 else ai2
    val currentAiPlayer = currentState.turn // The player whose turn it is.

    println(s"${currentAi.toString} is thinking...")

    // AI's turn:
    // 1. Get the best move from the current AI.
    // 2. Measure thinking time and calculate Nodes Per Second (NPS).
    val startTime = System.currentTimeMillis()
    // AIPlayer passes its own turn to findBestMove's 'turn' param.
    // The 'currentSearchDepth' for findBestMove will be AI's configured depth.
    val searchDepthForThisTurn = if (currentAiPlayer == PlayerA) SEARCH_DEPTH_AI1 else SEARCH_DEPTH_AI2
    val (bestMoveOpt, nodesVisited) = currentAi.findBestMove(currentState, board, currentAiPlayer, searchDepthForThisTurn)
    val endTime = System.currentTimeMillis()
    val thinkingTime = endTime - startTime // Time taken by AI to find the move.
    println(s"Thinking time: ${thinkingTime}ms") // Clear output for thinking time.
    val thinkingTimeSeconds = thinkingTime / 1000.0
    val nps = if (thinkingTimeSeconds > 0) nodesVisited / thinkingTimeSeconds else 0.0 // Nodes Per Second.
    println(s"Nodes visited: $nodesVisited") // Clear output for nodes visited.
    println(s"NPS: ${"%.2f".format(nps)}") // Clear output for NPS.

    bestMoveOpt match {
      case Some(move) =>
        // Get piece before it moves for logging purposes.
        val pieceToMove = board.piece(move.oldPos, currentAiPlayer) // Use board.piece to handle board or hand.

        // Clearly print the move being made.
        println(s"${currentAiPlayer} (${Piece.name(pieceToMove)}) moves ${move.oldPos} -> ${move.newPos}" + (if(move.nari)" Nari" else ""))
        // Logging for captured piece on target square (if any).
        if (!Point.isCaptured(move.newPos)) { // Check only if the target is on the board.
            val pieceBeingCaptured = board.squares.get(move.newPos) // Piece on target square BEFORE this move is made.
            if (pieceBeingCaptured != Piece.❏) { // If there was a piece on the target square.
                 println(s"Captured: ${Piece.name(pieceBeingCaptured)}") // Clearly print captured piece.
            }
        }
        // Removed the redundant/problematic second block of capture logging.

        // Apply the move to the board.
        // board.move updates the board state (piece positions, captured pieces) and returns the new game state (updated turn, new captured pieces list for the state).
        // currentAiPlayer is the player who is making the move.
        // validation=false because the AI is expected to provide already validated moves.
        currentState = board.move(currentState, move.oldPos, move.newPos, false, move.nari)
        // After board.move, currentState.turn is updated to be the *next* player's turn.
        // The board object itself has been mutated by the move.

        // Sennichite (Four-fold repetition) check:
        // A game position is defined by the placement of pieces on the board, the pieces in each player's hand, and whose turn it is to move.
        // If the exact same game position occurs four times, the game is a draw due to Sennichite.
        val newBoardSquaresId = board.squares.id() // Unique identifier for current piece positions on the board.
        val newCapturedPiecesId = board.capturedPieces.id() // Unique identifier for pieces in hand for both players.
        val newPositionKey = (newBoardSquaresId, newCapturedPiecesId, currentState.turn) // The key representing the current game position.

        // Add the new position to the history.
        gamePositionHistory = newPositionKey :: gamePositionHistory

        // Count how many times this exact position has occurred.
        val occurrences = gamePositionHistory.count(_ == newPositionKey)

        if (occurrences >= 4) {
          println(s"\nSENNICHITE! Position repeated 4 times. Game is a draw.") // Clear Sennichite message.
          println(s"Board ID: $newBoardSquaresId, Captured ID: $newCapturedPiecesId, Turn: ${currentState.turn}")
          gameRunning = false // End the game.
        } else {
          // Game End Conditions Check (if not Sennichite):
          val playerWhoMadeTheMove = currentAiPlayer // The player whose turn it just was.
          val nextPlayer = currentState.turn      // The player whose turn it is now.

          // 1. King Capture Check:
          // Check if the playerWhoMadeTheMove has captured the opponent's King with their last move.
          // board.isFinish(P) checks if player P has the opponent's King in their hand.
          if (board.isFinish(playerWhoMadeTheMove)) {
            println(s"\nKING CAPTURED! Player ${playerWhoMadeTheMove} wins!") // Clear King capture message.
            gameRunning = false // End the game.
          } else {
            // 2. Checkmate or Stalemate Check:
            // If no King was captured, check if the *next* player (whose turn it is now) has any legal moves.
            val nextPlayerLegalMoves = jp.sndyuk.shogi.player.Utils.plans(board, currentState).toList
            if (nextPlayerLegalMoves.isEmpty) {
              // If the nextPlayer has no legal moves, it's either Checkmate or Stalemate.
              if (Rule.isInCheck(board, nextPlayer)) {
                // Checkmate: nextPlayer is in check and has no legal moves to escape the check.
                println(s"\nCHECKMATE! Player ${playerWhoMadeTheMove} wins! (Opponent ${nextPlayer} is in check and has no moves)") // Clear Checkmate message.
              } else {
                // Stalemate: nextPlayer is not in check but has no legal moves.
                // In Shogi, this is typically a loss for the player who is stalemated.
                println(s"\nSTALEMATE! Player ${nextPlayer} has no legal moves. Player ${playerWhoMadeTheMove} wins!") // Clear Stalemate message.
              }
              gameRunning = false // End the game.
            }
          }
        }
      case None =>
        // This case means the current AI (currentAiPlayer) could not find any legal moves.
        // This implies currentAiPlayer is either checkmated or stalemated.
        // The opponent (currentAiPlayer.change) wins.
        println(s"\nNo moves for ${currentAiPlayer}! ${currentAiPlayer.change} wins by checkmate/stalemate!") // Clear message for no moves.
        gameRunning = false // End the game.
    }
  } // end while loop (main game loop)

  // This block executes if the game loop ended due to MAX_MOVES being reached,
  // and not by a checkmate, stalemate, king capture, or Sennichite.
  if (gameRunning && moveCount >= MAX_MOVES) { // gameRunning check ensures we don't print this after a mate/draw
    println(s"\nGame ended: Max moves ($MAX_MOVES) reached. Declaring draw or by score.")
    // Optionally, print a final evaluation of the board position.
    // This evaluation can be from the perspective of one player (e.g., PlayerA/Sente).
    // Different evaluation functions (like EvaluationV1, EvaluationV2) can give different scores.
    val finalEvalV1ForSente = EvaluationV1.evaluate(board, PlayerA)
    val finalEvalV2ForSente = EvaluationV2.evaluate(board, PlayerA)
    println(s"Final board eval for PlayerA (using AIv1 logic - V1 eval): $finalEvalV1ForSente")
    println(s"Final board eval for PlayerA (using AIv2 logic - V2 eval): $finalEvalV2ForSente")
  }

  println("\nSimulation finished.") // Final message indicating the simulation has ended.
}
