package jp.sndyuk.shogi.ui // Assuming this is a suitable package for sample apps

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.ai._ // For ShogiAI, AlphaBetaAI_V1, AlphaBetaAI_V2, AlphaBetaAI_V3

object AIBattleSim extends App {

  // --- Configuration ---
  val NUM_GAMES = 5 // Number of games to run in the batch
  val MAX_MOVES = 200 // Game ends after this many half-moves if no mate

  // Player 1 (Sente) Configuration - SET FOR CURRENT PAIRING
  var AI1_TYPE = "V3" // Options: "V1", "V2", "V3"
  var SEARCH_DEPTH_AI1 = 2
  var Q_DEPTH_AI1 = 2 // Only used if AI1_TYPE is "V3"

  // Player 2 (Gote) Configuration - SET FOR CURRENT PAIRING
  var AI2_TYPE = "V1" // Options: "V1", "V2", "V3"
  var SEARCH_DEPTH_AI2 = 2
  var Q_DEPTH_AI2 = 0 // Only used if AI2_TYPE is "V3" (or relevant AI)
  // --- End Configuration ---

  // Store results
  var ai1Wins = 0
  var ai2Wins = 0
  var draws = 0

  println(s"Starting AI Battle Simulation Series: $NUM_GAMES games.")
  // Corrected Q_DEPTH_AI2 printing logic for the initial message
  println(s"Pairing: ${AI1_TYPE}(d${SEARCH_DEPTH_AI1}${if(AI1_TYPE == "V3") s",q${Q_DEPTH_AI1}" else ""}) [Sente] vs ${AI2_TYPE}(d${SEARCH_DEPTH_AI2}${if(AI2_TYPE == "V3") s",q${Q_DEPTH_AI2}" else ""}) [Gote]")

for (gameNum <- 1 to NUM_GAMES) {
  println(s"\n<<<<< Starting Game $gameNum of $NUM_GAMES >>>>>")

  // Initialize board and state for each game
  var board = Board()
  board.init()
  var currentState = State(Nil, PlayerA)
  var gameRunning = true
  var moveCount = 0
  // var gamePositionHistory: List[(ID, Turn)] = List() // For Sennichite - not fully implemented

  // Instantiate AIs based on configuration for each game
  val ai1: ShogiAI = AI1_TYPE match {
    case "V1" => new AlphaBetaAI_V1(name = s"AIv1_Sente(d$SEARCH_DEPTH_AI1)", searchDepth = SEARCH_DEPTH_AI1)
    case "V2" => new AlphaBetaAI_V2(name = s"AIv2_Sente(d$SEARCH_DEPTH_AI1)", searchDepth = SEARCH_DEPTH_AI1)
    case "V3" => new AlphaBetaAI_V3(name = s"AIv3_Sente(d$SEARCH_DEPTH_AI1,q$Q_DEPTH_AI1)", searchDepth = SEARCH_DEPTH_AI1, quiescenceSearchDepth = Q_DEPTH_AI1)
    case _ => throw new IllegalArgumentException(s"Unknown AI1_TYPE: $AI1_TYPE")
  }

  val ai2: ShogiAI = AI2_TYPE match {
    case "V1" => new AlphaBetaAI_V1(name = s"AIv1_Gote(d$SEARCH_DEPTH_AI2)", searchDepth = SEARCH_DEPTH_AI2)
    case "V2" => new AlphaBetaAI_V2(name = s"AIv2_Gote(d$SEARCH_DEPTH_AI2)", searchDepth = SEARCH_DEPTH_AI2)
    case "V3" => new AlphaBetaAI_V3(name = s"AIv3_Gote(d$SEARCH_DEPTH_AI2,q$Q_DEPTH_AI2)", searchDepth = SEARCH_DEPTH_AI2, quiescenceSearchDepth = Q_DEPTH_AI2)
    case _ => throw new IllegalArgumentException(s"Unknown AI2_TYPE: $AI2_TYPE")
  }

  var winner: Option[Turn] = None

  while (gameRunning && moveCount < MAX_MOVES) {
    moveCount += 1
    println(s"\n--- Game $gameNum, Turn ${currentState.turn}, Move #${moveCount} ---")
    // Only print board for first few moves to keep log shorter for simulation
    if (moveCount <= 10 || moveCount % 50 == 0 ) { // Print board for first 10 moves, then every 50 moves
        println(board.toString)
    } else if (moveCount == 11) {
        println("... (board printing suppressed for brevity) ...")
    }


    val currentAi: ShogiAI = if (currentState.turn == PlayerA) ai1 else ai2
    val currentAiPlayer = currentState.turn

    println(s"${currentAi.toString} is thinking...")

    val startTime = System.currentTimeMillis()
    val searchDepthForThisTurn = if (currentAiPlayer == PlayerA) SEARCH_DEPTH_AI1 else SEARCH_DEPTH_AI2
    val bestMoveOpt = currentAi.findBestMove(currentState, board, currentAiPlayer, searchDepthForThisTurn)
    val endTime = System.currentTimeMillis()
    println(s"Thinking time: ${endTime - startTime}ms")

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

        val playerWhoMadeTheMove = currentAiPlayer
        val nextPlayer = currentState.turn

        if (board.isFinish(playerWhoMadeTheMove)) {
          println(s"\nKING CAPTURED! Player ${playerWhoMadeTheMove} wins Game $gameNum!")
          winner = Some(playerWhoMadeTheMove)
          gameRunning = false
        } else {
          val nextPlayerLegalMoves = jp.sndyuk.shogi.player.Utils.plans(board, currentState).toList
          if (nextPlayerLegalMoves.isEmpty) {
            if (Rule.isInCheck(board, nextPlayer)) {
              println(s"\nCHECKMATE! Player ${playerWhoMadeTheMove} wins Game $gameNum! (Opponent ${nextPlayer} is in check and has no moves)")
            } else {
              println(s"\nSTALEMATE! Player ${nextPlayer} has no legal moves. Player ${playerWhoMadeTheMove} wins Game $gameNum!")
            }
            winner = Some(playerWhoMadeTheMove)
            gameRunning = false
          }
        }

      case None =>
        println(s"\nNo moves for ${currentAiPlayer}! ${currentAiPlayer.change} wins Game $gameNum by checkmate/stalemate!")
        winner = Some(currentAiPlayer.change)
        gameRunning = false
    }
  }

  if (winner.isDefined) {
    if (winner.get == PlayerA) ai1Wins += 1 else ai2Wins +=1
  } else if (moveCount >= MAX_MOVES) {
    println(s"\nGame $gameNum ended: Max moves ($MAX_MOVES) reached. Declaring draw or by score.")
    draws +=1
    // Print final board for draws
    println(board.toString)
    val finalEvalV1ForSente = EvaluationV1.evaluate(board, PlayerA)
    val finalEvalV2ForSente = EvaluationV2.evaluate(board, PlayerA)
    val finalEvalV3ForSente = EvaluationV3.evaluate(board, PlayerA)
    println(s"Final board eval for PlayerA (V1 - material): $finalEvalV1ForSente")
    println(s"Final board eval for PlayerA (V2 - V1 + basic heuristics): $finalEvalV2ForSente")
    println(s"Final board eval for PlayerA (V3 - V2 + PST): $finalEvalV3ForSente")
  }
  println(s"\n<<<<< Game $gameNum Finished. Score: Sente $ai1Wins - Gote $ai2Wins - Draws $draws (current series) >>>>>")
} // End of NUM_GAMES loop

  println(s"\n<<<<< Series of $NUM_GAMES Games Finished >>>>>")
  // Corrected Q_DEPTH_AI2 printing logic for the final summary
  println(s"Final Results for ${AI1_TYPE}(d${SEARCH_DEPTH_AI1}${if(AI1_TYPE == "V3") s",q${Q_DEPTH_AI1}" else ""}) [Sente] vs ${AI2_TYPE}(d${SEARCH_DEPTH_AI2}${if(AI2_TYPE == "V3") s",q${Q_DEPTH_AI2}" else ""}) [Gote]:")
  println(s"Sente (${AI1_TYPE}) wins: $ai1Wins")
  println(s"Gote (${AI2_TYPE}) wins: $ai2Wins")
  println(s"Draws (max moves reached): $draws")
  println("\nSimulation finished.")
}
