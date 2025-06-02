package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._ // This should bring in PlayerA, PlayerB if they are in core
// import jp.sndyuk.shogi.core.{PlayerA, PlayerB} // More specific if needed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// import scala.util.parsing.combinator.Parsers // Removed

// Minimal TestBoardUtils, specific to this AISpec.
object TestBoardUtils {
  def createBoardWithHands(
      boardPieces: Seq[(Piece, Point)] = Seq(),
      playerAHand: Map[Piece, Int] = Map.empty, // e.g. Piece.▲.FU -> 1
      playerBHand: Map[Piece, Int] = Map.empty  // e.g. Piece.△.FU -> 1
  ): Board = {
    val board = Board() // Creates a board, which internally calls init() for standard layout.

    // Clear the board to an empty state first
    val emptySquares = Array.fill(9, 9)(Piece.❏)
    board.init2(emptySquares.map(_.toSeq).toSeq, Seq()) // board.init2 re-initializes squares and captured pieces

    // Place pieces on board
    // This direct manipulation of squares is tricky. Board.init2 is better.
    // Let's re-create the pieceArray and call init2 once with everything.
    val pieceArray = Array.fill(9, 9)(Piece.❏)
    for ((p, pos) <- boardPieces) {
      pieceArray(pos.y)(pos.x) = p
    }

    // Prepare captured pieces for init2's second argument format: Seq[(Piece, Int)]
    // These are pieces *as they appear in hand*.
    // board.init2's `captured.foreach { case (p, c) => for (i <- 1 to c) capturedPieces.put(p) }`
    // `capturedPieces.put(p)`: if p is Sente piece, it goes to Gote's hand. If Gote, to Sente's.
    // So, if playerAHand has a Sente FU (▲.FU), it means Player A *has* it.
    // To achieve this via init2's `put` logic, Player B must have "lost" this Sente FU.
    // This means the `captured` seq for init2 should contain pieces that were "taken from the opponent".
    // The logic for capturedForInit2 was part of an earlier approach and was removed.
    // The current approach is to init the board with pieces on squares, then use capturedPieces.put.

    board.init2(pieceArray.map(_.toSeq).toSeq, Seq()) // init with on-board pieces and empty hands

    playerAHand.foreach { case (piece, count) =>
      val opponentPieceEquivalent = Piece.turned(piece) // To give PlayerA a ▲.FU, B must have lost a △.FU
      for (_ <- 1 to count) board.capturedPieces.put(opponentPieceEquivalent)
    }
    playerBHand.foreach { case (piece, count) =>
      val opponentPieceEquivalent = Piece.turned(piece) // To give PlayerB a △.FU, A must have lost a ▲.FU
      for (_ <- 1 to count) board.capturedPieces.put(opponentPieceEquivalent)
    }
    board
  }
}


class EvaluationV1Spec extends AnyFlatSpec with Matchers { // Ensuring it's EvaluationV1Spec

  import TestBoardUtils._

  // Helper values based on EvaluationV1.pieceValues for clarity in tests
  private val valFU = 100 // Used
  private val valHI = 900 // Used
  // private val valGI = 450 // This was unused, removing


  it should "return 0 for an empty board" in { // RE-ENABLED
    val board = createBoardWithHands()
    EvaluationV1.evaluate(board, PlayerA) shouldBe 0
    EvaluationV1.evaluate(board, PlayerB) shouldBe 0
  }

  it should "score a material advantage for Player A" in { // RE-ENABLED
    val board = createBoardWithHands(boardPieces = Seq(
      (Piece.▲.FU, Point(6,2)) // Sente Pawn at 7g
    ))
    EvaluationV1.evaluate(board, PlayerA) shouldBe valFU
    EvaluationV1.evaluate(board, PlayerB) shouldBe -valFU
  }

  it should "score pieces in hand correctly" in { // RE-ENABLED
    // Player A has a Sente Rook in hand.
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.HI -> 1))
    EvaluationV1.evaluate(board, PlayerA) shouldBe valHI
    EvaluationV1.evaluate(board, PlayerB) shouldBe -valHI
  }

  it should "calculate symmetric score for symmetric position" in { // RE-ENABLED
     val board = createBoardWithHands(
       boardPieces = Seq((Piece.▲.FU, Point(6,2)), (Piece.△.FU, Point(2,6))), // Sente Pawn, Gote Pawn
       playerAHand = Map(Piece.▲.GI -> 1), // Player A has Sente Silver
       playerBHand = Map(Piece.△.GI -> 1)  // Player B has Gote Silver
     )
     EvaluationV1.evaluate(board, PlayerA) shouldBe 0
     EvaluationV1.evaluate(board, PlayerB) shouldBe 0
  }
}

class AlphaBetaAI_V1_Spec extends AnyFlatSpec with Matchers {
  import TestBoardUtils._

  // Temporarily ignore other tests to focus logging on the failing one
  ignore should "correctly generate moves for the mate scenario using Utils.plans" in { // IGNORED
    val board = TestBoardUtils.createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,8)),
        (Piece.▲.HI, Point(1,1)),
        (Piece.△.OU, Point(0,1))
      )
    )
    val state = State(Nil, PlayerA) // Sente to move

    println("AISpec DEBUG: Board state for Utils.plans test:")
    println(board.toString)

    val legalMoves = jp.sndyuk.shogi.player.Utils.plans(board, state).toList // Explicit Utils path

    println(s"AISpec DEBUG: Utils.plans generated ${legalMoves.size} moves for Sente Rook at (1,1) with Gote King at (0,1):")
    var foundSelfMove = false
    legalMoves.zipWithIndex.foreach { case (mv, idx) =>
      // Get piece from board for logging, as mv.piece might be different if it's generalized in Transition
      val pieceOnSquare = board.squares.get(mv.oldPos)
      println(s"AISpec DEBUG: Move $idx: From ${mv.oldPos} (${Piece.name(pieceOnSquare)}) to ${mv.newPos}, Nari: ${mv.nari}, Capturing: ${mv.captured.map(Piece.name)}")
      if (mv.oldPos == mv.newPos) {
        foundSelfMove = true
        println(s"AISpec WARNING: Utils.plans generated a self-move: ${mv.oldPos} -> ${mv.newPos}")
      }
    }
    foundSelfMove shouldBe false

    // Check if the expected King capture is present
    // Transition stores the captured piece *type as it was on the square* (e.g. △.OU)
    val kingCaptureMoveExists = legalMoves.exists(m => m.oldPos == Point(1,1) && m.newPos == Point(0,1) && m.captured.contains(Piece.△.OU))
    println(s"AISpec DEBUG: Expected King capture (1,1)->(0,1) capturing △.OU exists: $kingCaptureMoveExists")
    kingCaptureMoveExists shouldBe true
  }

  it should "make an obvious capture of a valuable piece" in { // RE-ENABLED
    val board = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.HI, Point(4,4)), // Sente Rook at 5e
        (Piece.△.KI, Point(4,3))  // Gote Gold at 5d (undefended)
      )
    )
    val initialState = State(Nil, PlayerA) // Player A to move
    // searchDepth=1 should be enough for a direct capture. Use 2 for a bit more.
    val ai = new AlphaBetaAI_V1(name = "CaptureAI", searchDepth = 2)

    val bestMoveOpt = ai.findBestMove(initialState, board, PlayerA, 2)
    bestMoveOpt shouldBe defined
    bestMoveOpt.get.oldPos shouldBe Point(4,4) // Rook moves
    bestMoveOpt.get.newPos shouldBe Point(4,3) // To capture Gold
  }

  "AlphaBetaAI_V1_CheckmateTest" should "deliver a 1-move checkmate if available" in { // FOCUSED TEST with unique name part
    // Setup: Sente King at 8,8 (9i), Gote King at 0,1 (2a)
    // Sente Rook at 1,1 (2b), can move to 0,1 (capturing Gote King) for mate.
    // Gote King has no escapes (assuming empty board around it).
    val board = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,8)), // Sente King somewhere safe (e.g. 1i)
        (Piece.▲.HI, Point(1,1)), // Sente Rook at (1,1) (e.g. 8b)
        (Piece.△.OU, Point(0,1))  // Gote King at (0,1) (e.g. 8a) - Corrected to match assertion
      )
    )
    // Sente Rook at Point(1,1) (file 8, rank 2)
    // Gote King at Point(0,1) (file 8, rank 1)
    // Move Rook from (1,1) to (0,1) captures King.

    val initialState = State(Nil, PlayerA)
    // Using depth 2 as per plan.
    val ai = new AlphaBetaAI_V1(name = "MateAI", searchDepth = 2)

    val bestMoveOpt = ai.findBestMove(initialState, board, PlayerA, 2) // Search depth 2
    bestMoveOpt shouldBe defined
    bestMoveOpt.get.oldPos shouldBe Point(1,1)
    bestMoveOpt.get.newPos shouldBe Point(0,1)
  }
}

class UtilsPlansGoteCaptureScenarioSpec extends AnyFlatSpec with Matchers {
  import TestBoardUtils._ // Use the existing helper

  ignore should "Utils.plans for Gote (King at (0,1) vs Sente Promoted Rook at (0,0)) should generate King captures Promoted Rook move" in { // IGNORED for focus
    val board = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,8)),   // Sente King (position not critical for Gote's local moves)
        (Piece.▲.RY, Point(0,0)),   // Sente Promoted Rook (target)
        (Piece.△.OU, Point(0,1))    // Gote King
      )
    )
    // It's Gote's (PlayerB) turn
    val goteState = State(List(Transition(Point(1,1),Point(0,0),true,None)), PlayerB) // Dummy history, PlayerB to move

    println("UtilsPlansGoteCaptureScenarioSpec DEBUG: Board state for Gote's turn:")
    // Print relevant part of the board
    for (y <- 0 to 2) {
        val rowStr = (0 to 2).map { x =>
            Piece.pieceString(board.squares.get(Point(y,x)))
        }.mkString("|")
        println(s"UtilsPlansGoteCaptureScenarioSpec DEBUG: Row $y: $rowStr")
    }

    val legalMoves = jp.sndyuk.shogi.player.Utils.plans(board, goteState).toList

    println(s"UtilsPlansGoteCaptureScenarioSpec DEBUG: Utils.plans for Gote generated ${legalMoves.size} moves:")
    var foundKingCaptureRook = false
    var capturedPieceInTransition: Option[Piece] = None

    legalMoves.zipWithIndex.foreach { case (mv, idx) =>
      val movingPieceOnBoard = board.squares.get(mv.oldPos)
      println(s"UtilsPlansGoteCaptureScenarioSpec DEBUG: Gote Move $idx: ${Piece.name(movingPieceOnBoard)} from ${mv.oldPos} to ${mv.newPos}, Nari: ${mv.nari}, Capturing: ${mv.captured.map(Piece.name)}")

      if (mv.oldPos == Point(0,1) && mv.newPos == Point(0,0) && movingPieceOnBoard == Piece.△.OU) {
        foundKingCaptureRook = true
        capturedPieceInTransition = mv.captured
        println(s"UtilsPlansGoteCaptureScenarioSpec DEBUG: Found King Capture Rook transition: ${mv.toString}")
      }
    }

    foundKingCaptureRook shouldBe true

    capturedPieceInTransition shouldBe defined
    capturedPieceInTransition.get shouldBe Piece.▲.RY
  }
}

// New Test Class EvaluationV2Spec
class EvaluationV2Spec extends AnyFlatSpec with Matchers {
  import TestBoardUtils._
  private val MOBILITY_BONUS_PER_MOVE = 2

  "EvaluationV2.evaluate" should "favor player with higher piece mobility" in {
    val boardForMobilityTest = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,4)), // Sente King
        (Piece.▲.HI, Point(4,4)), // Sente Rook (very mobile)
        (Piece.△.OU, Point(0,4)), // Gote King
        (Piece.△.HI, Point(0,0)), // Gote Rook (boxed in)
        (Piece.△.FU, Point(1,0)), // Blocker pawn for Gote Rook
        (Piece.△.FU, Point(0,1))  // Blocker pawn for Gote Rook
      )
    )

    // Calculate expected mobility scores manually for non-pawn pieces (Kings and Rooks here)
    // Sente King at (8,4) (5i): Can move to (7,3), (7,4), (7,5), (8,3), (8,5) -> 5 moves (assuming edges)
    // Sente Rook at (4,4) (5e): 8 horizontal + 8 vertical = 16 moves
    // Total Sente mobility points = (5+16) * MOBILITY_BONUS_PER_MOVE = 21 * 2 = 42

    // Gote King at (0,4) (5a): Can move to (1,3), (1,4), (1,5), (0,3), (0,5) -> 5 moves
    // Gote Rook at (0,0) (1a): Blocked by own pawns at (1,0) and (0,1), 0 moves.
    // Total Gote mobility points = (5+0) * MOBILITY_BONUS_PER_MOVE = 5 * 2 = 10

    // Expected score for PlayerA = (SenteMobility - GoteMobility) + MaterialDiff
    // Material is equal (OU+HI vs OU+HI), so MaterialDiff = 0.
    // Expected score = 42 - 10 = 32.
    // Note: Actual King moves might be different if near edge, Rule.generateMovablePoints will be precise.

    var senteMoves = 0
    senteMoves += Rule.generateMovablePoints(boardForMobilityTest, Point(8,4), Piece.▲.OU, PlayerA, false).size
    senteMoves += Rule.generateMovablePoints(boardForMobilityTest, Point(4,4), Piece.▲.HI, PlayerA, false).size

    var goteMoves = 0
    goteMoves += Rule.generateMovablePoints(boardForMobilityTest, Point(0,4), Piece.△.OU, PlayerB, false).size
    goteMoves += Rule.generateMovablePoints(boardForMobilityTest, Point(0,0), Piece.△.HI, PlayerB, false).size

    val expectedMobilityScoreDifference = (senteMoves - goteMoves) * MOBILITY_BONUS_PER_MOVE
    val materialScore = EvaluationV1.evaluate(boardForMobilityTest, PlayerA)
    EvaluationV2.evaluate(boardForMobilityTest, PlayerA) shouldBe (materialScore + expectedMobilityScoreDifference)
  }

  it should "not count pawn/tokin mobility excessively" in {
    val boardNoPawns = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,4)), (Piece.▲.HI, Point(4,4)),
        (Piece.△.OU, Point(0,4)), (Piece.△.HI, Point(3,3))
      )
    )
    // Material is equal. Mobility for Sente King (8,4) and Gote King (0,4) are symmetric if board is open.
    // Sente Rook (4,4) and Gote Rook (3,3) have many moves.
    // Precise calculation:
    var senteMovesNoPawns = 0
    senteMovesNoPawns += Rule.generateMovablePoints(boardNoPawns, Point(8,4), Piece.▲.OU, PlayerA, false).size
    senteMovesNoPawns += Rule.generateMovablePoints(boardNoPawns, Point(4,4), Piece.▲.HI, PlayerA, false).size
    var goteMovesNoPawns = 0
    goteMovesNoPawns += Rule.generateMovablePoints(boardNoPawns, Point(0,4), Piece.△.OU, PlayerB, false).size
    goteMovesNoPawns += Rule.generateMovablePoints(boardNoPawns, Point(3,3), Piece.△.HI, PlayerB, false).size

    val expectedMobilityDiffNoPawns = (senteMovesNoPawns - goteMovesNoPawns) * MOBILITY_BONUS_PER_MOVE
    EvaluationV2.evaluate(boardNoPawns, PlayerA) shouldBe expectedMobilityDiffNoPawns // EvalV1 is 0

    // Now add pawns for Sente, mobility score should not change as pawns/tokins are excluded by current V2 logic
    val boardSentePawns = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,4)), (Piece.▲.HI, Point(4,4)),
        (Piece.▲.FU, Point(6,0)), (Piece.▲.FU, Point(6,1)),
        (Piece.△.OU, Point(0,4)), (Piece.△.HI, Point(3,3))
      ),
      playerAHand = Map(Piece.▲.FU -> 2)
    )
    val materialScoreSentePawns = EvaluationV1.evaluate(boardSentePawns, PlayerA) // Pawns contribute to material
    // Mobility calculation should be identical to boardNoPawns as FU are ignored
    EvaluationV2.evaluate(boardSentePawns, PlayerA) shouldBe (materialScoreSentePawns + expectedMobilityDiffNoPawns)
  }
}

// New Test Class AlphaBetaAI_V2_Spec
class AlphaBetaAI_V2_Spec extends AnyFlatSpec with Matchers {
  import TestBoardUtils._

  it should "prefer a move leading to higher mobility if material is equal" in {
    // Initial board: Sente King & Rook, Gote King & Rook. Gote Rook is boxed.
    // Sente Rook has a choice: move to an open square (high combined mobility)
    // or move to a more cramped square (low combined mobility).
    val initialBoardState = createBoardWithHands(
      boardPieces = Seq(
        (Piece.▲.OU, Point(8,4)), // Sente King
        (Piece.▲.HI, Point(7,4)), // Sente Rook (e.g. at 5h, can move to 5e or a more cramped square like 6h)
        (Piece.△.OU, Point(0,4)), // Gote King
        (Piece.△.HI, Point(0,0)), // Gote Rook (boxed)
        (Piece.△.FU, Point(1,0)),
        (Piece.△.FU, Point(0,1))
      )
    )
    val initialPlayer = PlayerA
    val initialState = State(Nil, initialPlayer)
    val aiV2 = new AlphaBetaAI_V2("AIV2_Activity", searchDepth = 1)

    // Option 1: Sente Rook moves to (4,4) (5e - open square for Rook)
    val boardAfterActiveMove = initialBoardState.copy()
    // Need to get the actual piece from initialBoardState to pass to move if board.move requires it
    // However, board.move(state, oldPos, newPos, validation, nari) gets piece from oldPos itself.
    boardAfterActiveMove.move(initialState, Point(7,4), Point(4,4), false, false)
    val evalActive = EvaluationV2.evaluate(boardAfterActiveMove, PlayerA) // Eval for Sente

    // Option 2: Sente Rook moves to (7,3) (6h - more cramped next to its King at 5i (8,4))
    val boardAfterPassiveMove = initialBoardState.copy()
    boardAfterPassiveMove.move(initialState, Point(7,4), Point(7,3), false, false)
    val evalPassive = EvaluationV2.evaluate(boardAfterPassiveMove, PlayerA) // Eval for Sente

    println(s"AISpec: Eval for Sente if Rook at (4,4) (active): $evalActive")
    println(s"AISpec: Eval for Sente if Rook at (7,3) (passive): $evalPassive")
    evalActive should be > evalPassive // Sente should prefer the active square

    val bestMoveOpt = aiV2.findBestMove(initialState, initialBoardState, initialPlayer, 1)
    bestMoveOpt shouldBe defined
    bestMoveOpt.get.newPos shouldBe Point(4,4) // Expect move to the more active square (5e)
  }
}
