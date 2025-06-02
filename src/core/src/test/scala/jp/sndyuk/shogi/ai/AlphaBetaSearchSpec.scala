package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AlphaBetaSearchSpec extends AnyFlatSpec with Matchers {

  // Helper to create a board from a list of pieces and their positions
  def setupBoard(pieces: List[(Piece, Point)], turn: Turn = PlayerA, hands: Map[Turn, List[Piece]] = Map.empty): (State, Board) = {
    val board = Board() // Creates a board with standard initial setup
    // Clear the board first
    for (y <- 0 to 8; x <- 0 to 8) {
      board.squares.setAndGet(Piece.❏, Point(y, x))
    }
    board.capturedPieces.playerA = 0 // Clear hands
    board.capturedPieces.playerB = 0

    pieces.foreach { case (piece, pos) =>
      board.squares.setAndGet(piece, pos)
    }

    hands.get(PlayerA).foreach { handPieces =>
      handPieces.foreach(p => board.capturedPieces.put(Piece.convert(p, PlayerB))) // Captured by A means B's piece
    }
    hands.get(PlayerB).foreach { handPieces =>
      handPieces.foreach(p => board.capturedPieces.put(Piece.convert(p, PlayerA))) // Captured by B means A's piece
    }
    (State(Nil, turn), board)
  }

  // Helper to call AlphaBetaSearch.search
  def getSearchScore(
      state: State,
      board: Board,
      depth: Int,
      quiescenceDepth: Int,
      turn: Turn // This is the rootPlayerTurn for the search
  ): Int = {
    val (score, _) = AlphaBetaSearch.search(
      currentState = state,
      currentBoard = board,
      currentBoardID = ID(board),
      gamePathHistoryIDs = Nil,
      depth = depth,
      quiescenceDepth = quiescenceDepth,
      alpha = Int.MinValue + 10000, // AlphaBetaSearch.MATE_SCORE_GUARD might not be visible, use a large enough margin
      beta = Int.MaxValue - 10000,
      maximizingPlayer = true, // Score is from the perspective of the 'turn' player
      rootPlayerTurn = turn,
      evalFunc = EvaluationV1.evaluate
    )
    score
  }

  // Piece values from EvaluationV1 for reference in tests
  val PAWN_VALUE = 100
  val LANCE_VALUE = 300
  val KNIGHT_VALUE = 320
  val SILVER_VALUE = 450
  val GOLD_VALUE = 500
  val BISHOP_VALUE = 800
  val ROOK_VALUE = 900
  val PROMOTED_PAWN_VALUE = 550 // TOKIN

  behavior of "AlphaBetaSearch with Quiescence"

  // Test 1: Simple Capture Chain
  it should "evaluate a simple capture chain correctly with quiescence" in {
    // Scenario: Player A (Sente) has a Rook at (4,4) (5e). Player B (Gote) has a Pawn at (4,3) (5d).
    // If Sente plays RxPawn at (4,3), Gote can recapture with a Gold from (3,3) (6d) if available.
    // For this test, let's make it simpler:
    // Sente's Rook at (4,4) can capture Gote's unprotected Pawn at (4,3). This is a simple material gain.
    // Quiescence is not strictly needed here but tests the pathway.
    // A better test: Sente Rook attacks Gote Knight. Gote Knight is defended by Gote Pawn.
    // Sente (PlayerA) Rook at (1,1) (8h), Gote (PlayerB) Knight at (1,2) (8g), Gote Pawn at (1,3) (8f)
    // Sente's turn.
    // Sente moves Rook to capture Knight at (1,2). Board state for quiescence: Sente Rook at (1,2), Gote Pawn at (1,3).
    // Quiescence for Sente (maximizing, but it's after Gote's turn if we consider Gote's recapture)
    // Let's test the evaluation of a position *after* Sente's RxN capture.
    // Board: Sente Rook at (1,2), Gote Pawn at (1,3), Sente King (0,4), Gote King (8,4)
    // It's Gote's turn to move.
    val pieces = List(
      (Piece.▲.HI, Point(1,2)), // Sente Rook that just captured
      (Piece.▲.OU, Point(0,4)), // Sente King
      (Piece.△.FU, Point(1,3)), // Gote Pawn that can recapture Rook
      (Piece.△.OU, Point(8,4))  // Gote King
    )
    val (state, board) = setupBoard(pieces, turn = PlayerB) // Gote's turn

    // Evaluation from Gote's perspective (maximizingPlayer = true for Gote)
    // Gote wants to maximize its score. Capturing Rook with Pawn is good for Gote.
    // Score without quiescence (depth 0, qDepth 0): Gote is down a Knight (-KNIGHT_VALUE for Gote if Knight was on board before)
    // Let's make the evalFunc always from Sente's perspective for simplicity in test assertions.
    // So, rootPlayerTurn = PlayerA. maximizingPlayer depends on whose effective turn it is in search.

    // After Sente RxN, it's Gote's turn. Search is called for Gote.
    // If Gote is maximizingPlayer in search:
    // Q-depth 0 (for Gote's turn): Gote sees current board. Score for Gote: ROOK_VALUE (material balance)
    // Q-depth 1 (for Gote's turn): Gote Pawn takes Sente Rook. Gote is up ROOK_VALUE - PAWN_VALUE. Score for Gote: ROOK_VALUE - PAWN_VALUE (material gained)
    // This is a bit complex to set up the "before" and "after" state for the main search vs quiescence.

    // Let's test a state that *would be evaluated by quiescence search*.
    // Sente (PlayerA) has just made a capture, e.g. Rook takes Knight.
    // The board state: Sente Rook at (1,2), Gote Pawn at (1,3) can recapture.
    // It's Sente's turn at depth 0 of main search, now quiescence search starts.
    // Sente is maximizingPlayer.

    val currentPieces = List(
      (Piece.▲.HI, Point(1,2)), // Sente Rook
      (Piece.▲.OU, Point(0,4)), // Sente King
      (Piece.△.FU, Point(1,3)), // Gote Pawn
      (Piece.△.OU, Point(8,4))  // Gote King
    )
    // This is the state *after* Sente played some move and it's Sente's turn again at depth -1 (effectively, Gote's turn in search).
    // Let's make it Sente's turn, currentDepth = 0, so quiescence kicks in for Sente.
    val (senteState, senteBoard) = setupBoard(currentPieces, turn = PlayerA)

    // Sente is player A. Quiescence search for Sente (maximizing).
    // Sente looks at Gote's possible captures. Gote can play FUxHI.
    // Score for Sente.
    // Initial material: Sente has Rook (900), Gote has Pawn (100). Balance for Sente = 900 - 100 = 800.
    val score_q0 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 0, turn = PlayerA)
    // Q0: Sente is maximizing. Sees current board. No captures for Sente.
    // Gote's turn in quiescence. Gote is minimizing for Sente. Gote FUxHI.
    // Board after Gote FUxHI: Sente King, Gote Pawn, Gote King. Sente material = 0. Gote material = Pawn (100).
    // Score for Sente = -100.
    // This is the score the quiescence search (called for Sente) should find if Gote captures.

    // Standing pat score for Sente: Sente Rook (900) - Gote Pawn (100) = 800.
    score_q0 should be (800) // Quiescence depth 0, just static eval.

    val score_q1 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 1, turn = PlayerA)
    // Q1 for Sente (maximizing):
    //   Standing pat: 800. Alpha = 800.
    //   Sente considers Gote's capture moves (minimizing for Sente).
    //   Gote plays FUxHI. Board: Sente King, Gote Pawn (now at (1,2)), Gote King.
    //   Next state for Sente (recursive call, qdepth=0, Sente is maximizing):
    //     Static eval: Sente material 0, Gote material Pawn (100). Score for Sente = -100.
    //   So Gote's capture leads to -100 for Sente.
    //   Sente (maximizing) chose standing pat (800) vs. outcome of Gote's capture (-100). Wait, this is wrong.
    //   Quiescence search for Sente (player A, maximizing):
    //   Alpha = -Inf, Beta = +Inf
    //   Standing pat score = 800. currentMaxEval = 800. Alpha = 800.
    //   Now, consider moves for the *other* player (Gote, player B, minimizing for Sente)
    //   Gote's capture: FU x HI at (1,2).
    //     New board: Sente OU, Gote FU at (1,2), Gote OU.
    //     Recursive call to quiescenceSearch (qDepth=0, for Gote, but eval is from Sente's perspective, call is with maximizingPlayer=false for Sente).
    //       evalFunc returns: Sente material 0 - Gote material 100 = -100.
    //     This eval (-100) is compared with currentMinEval for Gote (which would be initialized with standing pat from Gote's view, or just +Inf).
    //     Gote wants to minimize Sente's score. So Gote chooses -100.
    //   The main Sente quiescence loop: currentMaxEval was 800 (standing pat). The result of Gote's capture sequence is -100.
    //   Sente is maximizing. Is -100 > 800? No. So Sente would stick to 800 if it could choose not to allow Gote to capture.
    //   This means the capture by Gote is forced if Sente is in this state.
    //   The quiescence search should return the score *after* captures settle.
    //   So if Gote *can* capture, that sequence's score (-100) should be returned.
    score_q1 should be (-PAWN_VALUE) // After Gote's Pawn captures Sente's Rook, Sente is left with nothing, Gote with Pawn. Score = 0 - 100 = -100.

    // Test 1 variant B: Rook is threatened, can recapture
    // Sente Rook at (4,4) (5e). Gote Pawn at (4,3) (5d). Sente Bishop at (3,3) (6d).
    // It's Sente's turn.
    // If Sente does nothing (e.g. king move), Gote will Pawn x Rook.
    // Consider the state *after* Gote plays Pawn x Rook. Sente's turn.
    // Board: Gote Pawn at (4,4), Sente Bishop at (3,3). Sente King, Gote King.
    val pieces_varB = List(
      (Piece.△.FU, Point(4,4)), // Gote Pawn that just captured a Rook
      (Piece.▲.KA, Point(3,3)), // Sente Bishop that can recapture Pawn
      (Piece.▲.OU, Point(0,0)),
      (Piece.△.OU, Point(8,8))
    )
    val (state_b, board_b) = setupBoard(pieces_varB, turn = PlayerA)
    // Sente's turn. PlayerA is root. Maximizing.
    // Material balance before Sente's recapture: Gote Pawn (100). Sente Bishop (800).
    // Sente score = Bishop - Pawn = 800 - 100 = 700. (This is if we assume Rook was already lost)
    // Let's consider the value of pieces on board for Sente: Bishop (800). For Gote: Pawn (100). Net for Sente = 700.

    val score_b_q0 = getSearchScore(state_b, board_b, depth = 0, quiescenceDepth = 0, turn = PlayerA)
    // Q0 for Sente: Standing pat. Score = Sente Bishop (800) - Gote Pawn (100) = 700.
    score_b_q0 should be (BISHOP_VALUE - PAWN_VALUE)

    val score_b_q1 = getSearchScore(state_b, board_b, depth = 0, quiescenceDepth = 1, turn = PlayerA)
    // Q1 for Sente (maximizing):
    //   Standing pat score = 700. currentMaxEval = 700, alpha = 700.
    //   Sente's capture moves: Bishop x Pawn at (4,4).
    //     New board: Sente Bishop at (4,4). Sente King, Gote King.
    //     Recursive call to quiescenceSearch (qDepth=0, for Sente, but it's Gote's turn in search, so maximizingPlayer=false for Sente).
    //       Eval for Sente: Sente Bishop (800) - Gote material (0) = 800.
    //     This eval (800) is compared with currentMaxEval (700). 800 > 700. So currentMaxEval becomes 800.
    //   No Gote captures to consider from this state if Sente just captured.
    score_b_q1 should be (BISHOP_VALUE) // Sente recaptures Pawn. Sente has Bishop. Gote has nothing. Score = 800.

    // Test 1, Variant 1 (Original): Sente Rook at (1,2), Gote Pawn at (1,3). Sente's turn.
    // Sente has no captures. Gote can capture Sente's Rook.
    // Quiescence search for Sente should return the standing pat score, as Sente has no captures to improve upon it.
    val score_q1_fixed = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 1, turn = PlayerA)
    score_q1_fixed should be (ROOK_VALUE - PAWN_VALUE) // Should be 800
  }

  // Test 2: Avoiding a Bad Exchange due to opponent's quiescence capture
  it should "avoid a line if opponent has a devastating capture seen by quiescence" in {
    // Sente's turn.
    // Sente FU at (2,1) (can promote to (2,0)). Sente Rook at (1,1) (unprotected).
    // Gote Silver at (3,0) (can take Rook if FU moves away from guarding, or if Rook becomes exposed).
    // Let's simplify: Sente FU (Pawn) at P(6,1) (can move to P(6,0) to promote)
    // Sente Rook at P(5,1) (unprotected).
    // Gote Silver at P(5,0) (can take Rook P(5,1) if Sente makes a non-Rook move).
    // Sente King P(0,4), Gote King P(8,4).
    val pieces = List(
      (Piece.▲.FU, Point(6,1)), (Piece.▲.HI, Point(5,1)), (Piece.▲.OU, Point(0,4)),
      (Piece.△.GI, Point(5,0)), (Piece.△.OU, Point(8,4))
    )
    val (initialState, initialBoard) = setupBoard(pieces, turn = PlayerA)

    // We need to find the best move. AlphaBetaSearch.search returns (score, Option[Transition])
    // Let's make a wrapper for that.
    def getSearchScoreAndMove(
      state: State, board: Board, depth: Int, quiescenceDepth: Int, turn: Turn
    ): (Int, Option[Transition]) = {
      AlphaBetaSearch.search(
        currentState = state, currentBoard = board, currentBoardID = ID(board), gamePathHistoryIDs = Nil,
        depth = depth, quiescenceDepth = quiescenceDepth,
        alpha = Int.MinValue + 10000, beta = Int.MaxValue - 10000,
        maximizingPlayer = true, rootPlayerTurn = turn, evalFunc = EvaluationV1.evaluate
      )
    }

    // Scenario 1: Quiescence Depth = 0 for opponent replies
    // Sente searches at depth 1 (to consider its own moves).
    // Opponent replies are evaluated at depth 0, qDepth 0.
    val (score_d1_q0, move_d1_q0) = getSearchScoreAndMove(initialState, initialBoard, depth = 1, quiescenceDepth = 0, turn = PlayerA)

    // If Sente promotes FU: ▲FU (6,1) -> (6,0)NARI.
    //   Board after promote: ▲TO@(6,0), ▲HI@(5,1). Opponent: △GI@(5,0).
    //   Static eval (Sente's perspective): TO(550) + HI(900) - GI(450) = 1450 - 450 = 1000.
    // This move looks good (score 1000) if Gote's devastating capture is not seen.
    // Other moves for Sente: e.g. HI (5,1) -> (5,2) (safe).
    //   Board: ▲FU@(6,1), ▲HI@(5,2). Opponent: △GI@(5,0).
    //   Static eval: FU(100) + HI(900) - GI(450) = 1000-450 = 550.
    // So, without quiescence on Gote's reply, Sente might choose FU promotion.
    // move_d1_q0.get.newPos should be (Point(6,0)) // Promotion if score is 1000
    // We expect score_d1_q0 to be high, possibly leading to FU promotion.

    // Scenario 2: Quiescence Depth > 0 for opponent replies
    val (score_d1_q1, move_d1_q1) = getSearchScoreAndMove(initialState, initialBoard, depth = 1, quiescenceDepth = 2, turn = PlayerA)

    // If Sente promotes FU: ▲FU (6,1) -> (6,0)NARI.
    //   Now, Gote's reply is evaluated with quiescence (depth=0, qDepth=2 for Gote's node).
    //   Gote (minimizing for Sente) sees △GI@(5,0) x ▲HI@(5,1).
    //   Board after Gote's capture: ▲TO@(6,0), △GI@(5,1) (has captured Rook).
    //   Eval (Sente's perspective): TO(550) - GI(450) = 100.
    // So, the FU promotion move, when evaluated with quiescence for Gote's reply, gets score 100 for Sente.
    // Sente should prefer a simple Rook move like HI(5,1)->(5,2) (score 550) over this.

    // Check that score_d1_q0 (no/low q-depth for replies) is higher than score_d1_q1 (q-depth for replies)
    // because q-search reveals the blunder.
    (score_d1_q0 > score_d1_q1) should be (true)
    // And the move chosen with quiescence should not be the pawn promotion if there's a safer alternative.
    // A safe rook move (e.g. HI to (5,2)) would give a score of FU(100)+HI(900) - GI(450) = 550.
    // The pawn promotion, if quiescence reveals the rook loss, scores TOKIN(550) - GI(450) = 100.
    // So, the AI should prefer the rook move.
    move_d1_q1.get.oldPos should be (Point(5,1)) // Expecting Rook move
    score_d1_q1 should be (PAWN_VALUE + ROOK_VALUE - SILVER_VALUE) // 100 + 900 - 450 = 550
  }

  // Test 3: Quiescence Depth Limit
  it should "limit search depth in quiescence according to quiescenceDepth" in {
    // Sente R(2,2), B(1,0), G(0,0). Gote N(2,1), P(2,0). Kings far.
    // Chain: Sente R(2,2)xN(2,1); Gote P(2,0)xR(2,1); Sente B(1,0)xP(2,0)
    // Values: N=320, P=100, R=900, B=800
    val pieces = List(
      (Piece.▲.HI, Point(2,2)), (Piece.▲.KA, Point(1,0)), (Piece.▲.KI, Point(0,0)), (Piece.▲.OU, Point(4,4)),
      (Piece.△.KE, Point(2,1)), (Piece.△.FU, Point(2,0)), (Piece.△.OU, Point(6,6)) // KE for Knight, FU for Pawn
    )
    val (senteState, senteBoard) = setupBoard(pieces, turn = PlayerA)

    val initialMaterialSente = ROOK_VALUE + BISHOP_VALUE + GOLD_VALUE
    val initialMaterialGote = KNIGHT_VALUE + PAWN_VALUE
    val score_static = initialMaterialSente - initialMaterialGote // 900+800+500 - 320-100 = 2200 - 420 = 1780

    val score_q0 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 0, turn = PlayerA)
    score_q0 should be (score_static) // 1780

    // Q-Depth 1: Sente RxN. Board: Sente R,B,G. Gote P. Sente up N.
    // Score: 1780 + KNIGHT_VALUE = 1780 + 320 = 2100
    val score_q1 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 1, turn = PlayerA)
    score_q1 should be (score_static + KNIGHT_VALUE) // 2100

    // Q-Depth 2: Sente RxN; Gote PxR. Board: Sente B,G. Gote P. Sente up N, down R.
    // Score: 1780 + KNIGHT_VALUE - ROOK_VALUE = 2100 - 900 = 1200
    // Sente (max) chooses max(standing_pat=1780, outcome_of_RxN_then_PxR=1200) = 1780
    val score_q2 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 2, turn = PlayerA)
    score_q2 should be (score_static) // 1780

    // Q-Depth 3: Sente RxN; Gote PxR; Sente BxP. Board: Sente B,G. Gote nothing from these. Sente up N, up P, down R.
    // Score: 1780 + KNIGHT_VALUE - ROOK_VALUE + PAWN_VALUE = 1200 + 100 = 1300
    // Sente (max) chooses max(standing_pat=1780, outcome_of_chain=1300) = 1780
    val score_q3 = getSearchScore(senteState, senteBoard, depth = 0, quiescenceDepth = 3, turn = PlayerA)
    score_q3 should be (score_static) // 1780
  }

  // Test 4: No Captures Available
  it should "return static evaluation if no captures are available" in {
    val pieces = List(
      (Piece.▲.OU, Point(0,4)), (Piece.△.OU, Point(8,4)),
      (Piece.▲.FU, Point(2,4)) // A Sente pawn, not attacking anything
    )
    val (state, board) = setupBoard(pieces, turn = PlayerA)
    val staticScore = EvaluationV1.evaluate(board, PlayerA) // Sente FU (100)

    staticScore should be (PAWN_VALUE)

    val qScore = getSearchScore(state, board, depth = 0, quiescenceDepth = 3, turn = PlayerA)
    qScore should be (staticScore)
  }
}
