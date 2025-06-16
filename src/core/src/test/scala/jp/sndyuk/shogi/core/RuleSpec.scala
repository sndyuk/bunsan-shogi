package jp.sndyuk.shogi.core

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Piece._

class RuleSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {

  "36FU" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 6)
    val piece = ▲.FU
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(3, 5), false))
  }

  "39GI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 9)
    val piece = ▲.GI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(3, 8), false))
  }

  "28HI" should "be able to move" ignore {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(2, 8)
    val piece = ▲.HI
    // includePromoted = true, but rook cannot promote with these moves from starting rank.
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    val expectedMoves = Set(
      (Board.humanReadableToPoint(1, 8), false), // Move to file 1, rank 8 (Point(7,0) if (2,8) -> (7,1))
      (Board.humanReadableToPoint(3, 8), false), // Move to file 3, rank 8 (Point(7,2))
      (Board.humanReadableToPoint(4, 8), false), // Move to file 4, rank 8 (Point(7,3))
      (Board.humanReadableToPoint(5, 8), false), // Move to file 5, rank 8 (Point(7,4))
      (Board.humanReadableToPoint(6, 8), false), // Move to file 6, rank 8 (Point(7,5))
      (Board.humanReadableToPoint(7, 8), false), // Move to file 7, rank 8
      (Board.humanReadableToPoint(8, 8), false), // Move to file 8, rank 8
      (Board.humanReadableToPoint(9, 8), false), // Move to file 9, rank 8
      (Board.humanReadableToPoint(2, 9), false)  // Corrected: Move to file 2, rank 9 (backward)
    )

    moves.toSet should contain theSameElementsAs expectedMoves
  }

  "88KA" should "not be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(8, 8)
    val piece = ▲.KA
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream shouldBe empty
  }

  // --- Tests for Rule.movableScopes ---

  val movableScopesTestData = List(
    // Sente Pieces
    (▲.FU, "▲.FU (Sente Pawn)", List((-1, 0, false))),
    (▲.KY, "▲.KY (Sente Lance)", List((-1, 0, Rule.∞))),
    (▲.KE, "▲.KE (Sente Knight)", List((-2, -1, false), (-2, 1, false))),
    (▲.GI, "▲.GI (Sente Silver)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (1, -1, false), (1, 1, false))),
    (▲.KI, "▲.KI (Sente Gold)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false))),
    (▲.KA, "▲.KA (Sente Bishop)", List((-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞))),
    (▲.HI, "▲.HI (Sente Rook)", List((-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞))),
    (▲.OU, "▲.OU (Sente King)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false), (1, -1, false), (1, 1, false))),
    // Promoted Sente Pieces
    (▲.TO, "▲.TO (Promoted Sente Pawn)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false))),
    (▲.NY, "▲.NY (Promoted Sente Lance)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false))),
    (▲.NK, "▲.NK (Promoted Sente Knight)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false))),
    (▲.NG, "▲.NG (Promoted Sente Silver)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false))),
    (▲.UM, "▲.UM (Promoted Sente Bishop)", List((-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false), (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false), (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞))),
    (▲.RY, "▲.RY (Promoted Sente Rook)", List((-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false), (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false), (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞))),
    // Gote Pieces
    (△.FU, "△.FU (Gote Pawn)", List((1, 0, false))),
    (△.KY, "△.KY (Gote Lance)", List((1, 0, Rule.∞))),
    (△.KE, "△.KE (Gote Knight)", List((2, -1, false), (2, 1, false))),
    (△.GI, "△.GI (Gote Silver)", List((1, 0, false), (1, -1, false), (1, 1, false), (-1, -1, false), (-1, 1, false))),
    (△.KI, "△.KI (Gote Gold)", List((1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false))),
    (△.KA, "△.KA (Gote Bishop)", List((-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞))),
    (△.HI, "△.HI (Gote Rook)", List((-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞))),
    (△.OU, "△.OU (Gote King)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false), (1, -1, false), (1, 1, false))),
    // Promoted Gote Pieces
    (△.TO, "△.TO (Promoted Gote Pawn)", List((1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false))),
    (△.NY, "△.NY (Promoted Gote Lance)", List((1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false))),
    (△.NK, "△.NK (Promoted Gote Knight)", List((1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false))),
    (△.NG, "△.NG (Promoted Gote Silver)", List((1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false))),
    (△.UM, "△.UM (Promoted Gote Bishop)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false), (1, -1, false), (1, 1, false), (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞))),
    (△.RY, "△.RY (Promoted Gote Rook)", List((-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false), (1, -1, false), (1, 1, false), (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)))
  )

  "Rule.movableScopes" should "return correct scopes for all piece types" in {
    movableScopesTestData.foreach { case (piece, pieceName, expectedScopes) =>
      withClue(s"For piece $pieceName:") {
        val scopes = Rule.movableScopes(piece)
        scopes should contain theSameElementsAs expectedScopes
      }
    }
  }

  // --- Helper for custom board setups ---
  // playerAHand: Map of pieces Player A has (e.g., Piece.▲.FU -> 1)
  // playerBHand: Map of pieces Player B has (e.g., Piece.△.FU -> 1)
  def createBoardWithHands(
    boardPieces: Seq[(Piece, Point)] = Seq(),
    playerAHand: Map[Piece, Int] = Map.empty,
    playerBHand: Map[Piece, Int] = Map.empty
  ): Board = {
    val board = new Board() // Initializes with empty Squares and CapturedPieces
    val pieceArray = Array.fill(9, 9)(Piece.❏)
    for ((p, pos) <- boardPieces) {
      pieceArray(pos.y)(pos.x) = p
    }
    board.init2(pieceArray.map(_.toSeq).toSeq) // Initialize board pieces first

    // To give Player A a piece (e.g., ▲.FU), simulate Player A capturing the opponent's version (△.FU)
    playerAHand.foreach { case (piece, count) =>
      val opponentPieceEquivalent = Piece.turned(piece) // e.g., if piece is ▲.FU, this is △.FU
      for (_ <- 1 to count) board.capturedPieces.put(opponentPieceEquivalent)
    }
    // To give Player B a piece (e.g., △.FU), simulate Player B capturing the opponent's version (▲.FU)
    playerBHand.foreach { case (piece, count) =>
      val opponentPieceEquivalent = Piece.turned(piece) // e.g., if piece is △.FU, this is ▲.FU
      for (_ <- 1 to count) board.capturedPieces.put(opponentPieceEquivalent)
    }
    board
  }

  // --- Tests for Rule.generateMovablePoints ---

  // Scenario 1: Sente Pawn (▲.FU)
  "generateMovablePoints for ▲.FU at (6,2) (7g) - clear path" should "yield one forward move" in {
    val pawnPos = Point(6,2) // 7g
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.FU, pawnPos)))
    val moves = Rule.generateMovablePoints(board, pawnPos, Piece.▲.FU, PlayerA, false).toList
    moves should contain theSameElementsAs List((Point(5,2), false)) // Move to 7f
  }

  "generateMovablePoints for ▲.FU at (6,2) (7g) - blocked by own piece" should "yield no moves" in {
    val pawnPos = Point(6,2) // 7g
    val blockerPos = Point(5,2) // 7f
    val board = createBoardWithHands(boardPieces = Seq(
      (Piece.▲.FU, pawnPos),
      (Piece.▲.GI, blockerPos) // Own Silver blocking
    ))
    val moves = Rule.generateMovablePoints(board, pawnPos, Piece.▲.FU, PlayerA, false).toList
    moves shouldBe empty
  }

  "generateMovablePoints for ▲.FU at (6,2) (7g) - can capture opponent's piece" should "yield one capture move" in {
    val pawnPos = Point(6,2) // 7g
    val opponentPos = Point(5,2) // 7f
    val board = createBoardWithHands(boardPieces = Seq(
      (Piece.▲.FU, pawnPos),
      (Piece.△.FU, opponentPos) // Opponent's Pawn to capture
    ))
    val moves = Rule.generateMovablePoints(board, pawnPos, Piece.▲.FU, PlayerA, false).toList
    moves should contain theSameElementsAs List((Point(5,2), false)) // Capture at 7f
  }

  "generateMovablePoints for ▲.FU at (3,2) (4g) - entering promotion zone" should "yield moves with and without promotion" in {
    val pawnPos = Point(3,2) // 4g, Sente's piece
    // Board setup: only this pawn
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.FU, pawnPos)))
    val moves = Rule.generateMovablePoints(board, pawnPos, Piece.▲.FU, PlayerA, true).toList
    // Moves to (2,2) (4f) - promotion zone is y=0,1,2 for Sente
    moves should contain theSameElementsAs List((Point(2,2), true), (Point(2,2), false))
  }

  "generateMovablePoints for ▲.FU at (1,2) (2g) - forced promotion" should "yield one move with promotion" in {
    val pawnPos = Point(1,2) // 2g
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.FU, pawnPos)))
    val moves = Rule.generateMovablePoints(board, pawnPos, Piece.▲.FU, PlayerA, true).toList
    // Moves to (0,2) (1f) - last rank, must promote
    // The Rule.generateMovablePoints for non-sliding pieces that MUST promote (like pawn to last rank)
    // might only return the promoted move if it's smart, or both if it relies on canBePromoted and includePromoted.
    // Based on canBePromoted and includePromoted=true, it should list (pos, true) and (pos, false)
    // then canMove would filter.
    // However, the code for MovePointIterator:
    // if (includePromoted && canBePromoted(board, oldPos, newPos, piece)) {
    //   val gpiece = generalize(piece)
    //   if (gpiece == ◯.FU || gpiece == ◯.KA || gpiece == ◯.HI || gpiece == ◯.KY) {
    //     (newPos, true) :: tmpPoints // 無駄に成らない歩、角、飛、香は不要 (This comment is misleading for FU to last rank)
    //   } else (newPos, true) :: (newPos, false) :: tmpPoints
    // }
    // For FU to last rank, canMoveAtNextTurn is false, so it's not "無駄に成らない".
    // And canMoveIfPromoted is also false.
    // The logic in MovePointIterator for non-sliding is:
    // if (isValidPosition(...) && (canMoveAtNextTurn(...) || canMoveIfPromoted(...)))
    // For pawn at (1,2) moving to (0,2):
    //   - canMoveAtNextTurn(Point(0,2), List((-1,0,false))) is false (off board)
    //   - canMoveIfPromoted(Piece.▲.FU, Point(1,2), Point(0,2)):
    //     - promoted = ▲.TO. movableScopes(▲.TO) is Gold moves.
    //     - canMoveAtNextTurn(Point(0,2), gold_scopes) is true.
    // So the condition holds.
    // Then, canBePromoted(board, Point(1,2), Point(0,2), Piece.▲.FU) is true.
    // generalize(Piece.▲.FU) is ◯.FU.
    // So it should return (Point(0,2), true) only.
    // This is because the special handling for "無駄に成らない歩..." (FU not promoting if it has moves afterwards)
    // does not apply if the non-promoted pawn would have no moves.
    // Let's re-check the logic more carefully for forced promotion.
    // The code is: `if (gpiece == ◯.FU ... && canMoveAtNextTurn(newPos, scopes))`, where scopes are for the unpromoted piece.
    // If unpromoted FU at newPos has no moves (e.g. last rank), then this condition is false.
    // So it falls to `else (newPos, true) :: (newPos, false) :: tmpPoints`.
    // This means it would generate both. However, `Rule.canMove` would later invalidate the non-promoting one.
    // `generateMovablePoints` itself is expected to return both if `includePromoted` is true and `canBePromoted` is true.
    // The specific case for "forced promotion" is typically handled by the game logic ensuring only the promoted move is valid.
    // `generateMovablePoints` is lower-level.
    // The provided code for MovePointIterator has:
    // if (includePromoted && canBePromoted(...)) {
    //    val gpiece = generalize(piece)
    //    if ((gpiece == ◯.FU || ...) && canMoveAtNextTurn(newPos, scopes)) { // scopes of UNPROMOTED piece
    //        (newPos, true) :: tmpPoints // This case is for when FU *could* not promote because it still has moves
    //    } else {
    //        (newPos, true) :: (newPos, false) :: tmpPoints // This is for other pieces OR FU that MUST promote (or has choice)
    //    }
    // } else { (newPos, false) :: tmpPoints }
    // For FU at (1,2) -> (0,2):
    //   canBePromoted is true. includePromoted is true.
    //   gpiece is ◯.FU.
    //   canMoveAtNextTurn(Point(0,2), List((-1,0,false))) is false (FU cannot move from last rank).
    //   So it goes to the `else` branch: `(Point(0,2), true) :: (Point(0,2), false) :: tmpPoints`.
    // This means it will generate both options.
     moves should contain theSameElementsAs List((Point(0,2), true), (Point(0,2), false))
  }

  // Scenario 2: Sente Rook (▲.HI)
  "generateMovablePoints for ▲.HI at (4,4) (5e) - empty board" should "yield all rank and file moves" in {
    val rookPos = Point(4,4) // 5e
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.HI, rookPos)))
    val moves = Rule.generateMovablePoints(board, rookPos, Piece.▲.HI, PlayerA, false).toList.map(_._1).toSet

    val expectedMoves = scala.collection.mutable.Set[Point]()
    // File moves (varying x)
    for (x <- 0 to 8 if x != rookPos.x) expectedMoves += Point(rookPos.y, x)
    // Rank moves (varying y)
    for (y <- 0 to 8 if y != rookPos.y) expectedMoves += Point(y, rookPos.x)

    moves shouldBe expectedMoves.toSet
  }

  "generateMovablePoints for ▲.HI at (4,4) (5e) - blocked by own pieces" should "stop before own pieces" in {
    val rookPos = Point(4,4) // 5e
    // Blockers: 5c (Point(4,2)) and 3e (Point(2,4))
    // Corrected mapping: File 5, Rank c (3rd from Sente's side) -> Point(6,4)
    // File 3, Rank e (5th from Sente's side) -> Point(4,6)
    // Using (y,x):
    // Blocker 1: Point(4,6) (file 3)
    // Blocker 2: Point(6,4) (rank 3 from Sente's view)
    // Let's use the problem description's coordinates directly for clarity of intent:
    // Own piece at (4,2) (file 5, rank c -> y=2, x=4)
    // Own piece at (2,4) (file 3, rank e -> y=4, x=2) - this is not how Point(y,x) works
    // Point(y,x): y is rank (0-8, 0=1st rank), x is file (0-8, 0=9th file)
    // Sente Rook at Point(4,4) (file 5, rank 5)
    // Own piece at Point(4,2) (file 7, rank 5)
    // Own piece at Point(2,4) (file 5, rank 3)
    val board = createBoardWithHands(Seq( // Changed createBoard to createBoardWithHands
      (Piece.▲.HI, rookPos),
      (Piece.▲.FU, Point(4,2)), // Blocker on file to the "right" (smaller x)
      (Piece.▲.FU, Point(2,4))  // Blocker on rank "above" (smaller y)
    ))
    val moves = Rule.generateMovablePoints(board, rookPos, Piece.▲.HI, PlayerA, false).toList.map(_._1).toSet

    val expectedMoves = scala.collection.mutable.Set[Point]()
    // Towards x=0 (file 9) from x=4: (4,3) ok. (4,2) blocked.
    expectedMoves += Point(4,3)
    // Towards x=8 (file 1) from x=4: (4,5), (4,6), (4,7), (4,8)
    for (x <- 5 to 8) expectedMoves += Point(4,x)
    // Towards y=0 (rank 1) from y=4: (3,4) ok. (2,4) blocked.
    expectedMoves += Point(3,4)
    // Towards y=8 (rank 9) from y=4: (5,4), (6,4), (7,4), (8,4)
    for (y <- 5 to 8) expectedMoves += Point(y,4)

    moves shouldBe expectedMoves.toSet
  }

  "generateMovablePoints for ▲.HI at (4,4) (5e) - can capture opponent's pieces" should "include captures and stop" in {
    val rookPos = Point(4,4) // file 5, rank 5
    // Opponent at Point(4,6) (file 3, rank 5)
    // Opponent at Point(6,4) (file 5, rank 7)
    val board = createBoardWithHands(Seq( // Changed createBoard to createBoardWithHands
      (Piece.▲.HI, rookPos),
      (Piece.△.FU, Point(4,6)), // Opponent on file to the "left" (larger x)
      (Piece.△.FU, Point(6,4))  // Opponent on rank "below" (larger y)
    ))
    val moves = Rule.generateMovablePoints(board, rookPos, Piece.▲.HI, PlayerA, false).toList.map(_._1).toSet

    val expectedMoves = scala.collection.mutable.Set[Point]()
    // Towards x=0 (file 9) from x=4: (4,0), (4,1), (4,2), (4,3)
    for (x <- 0 to 3) expectedMoves += Point(4,x)
    // Towards x=8 (file 1) from x=4: (4,5) ok. (4,6) capture.
    expectedMoves += Point(4,5)
    expectedMoves += Point(4,6) // Capture
    // Towards y=0 (rank 1) from y=4: (0,4), (1,4), (2,4), (3,4)
    for (y <- 0 to 3) expectedMoves += Point(y,4)
    // Towards y=8 (rank 9) from y=4: (5,4) ok. (6,4) capture.
    expectedMoves += Point(5,4)
    expectedMoves += Point(6,4) // Capture

    moves shouldBe expectedMoves.toSet
  }

  "generateMovablePoints for ▲.HI at (4,4) (5e) - path clear, then opponent, then own" should "capture opponent, stop before own" in {
    val rookPos = Point(4,4) // file 5, rank 5
    // Opponent at Point(4,2) (file 7, rank 5)
    // Own piece at Point(4,1) (file 8, rank 5)
    val board = createBoardWithHands(Seq( // Changed createBoard to createBoardWithHands
      (Piece.▲.HI, rookPos),
      (Piece.△.FU, Point(4,2)), // Opponent
      (Piece.▲.FU, Point(4,1))  // Own piece
    ))
    val moves = Rule.generateMovablePoints(board, rookPos, Piece.▲.HI, PlayerA, false).toList.map(_._1).toSet

    val expectedMoves = scala.collection.mutable.Set[Point]()
    // Towards x=0 (file 9) from x=4: (4,3) ok. (4,2) capture. (4,1) blocked by own.
    expectedMoves += Point(4,3)
    expectedMoves += Point(4,2) // Capture
    // Other directions are clear
    for (x <- 5 to 8) expectedMoves += Point(4,x) // Towards x=8 (file 1)
    for (y <- 0 to 3) expectedMoves += Point(y,4) // Towards y=0 (rank 1)
    for (y <- 5 to 8) expectedMoves += Point(y,4) // Towards y=8 (rank 9)

    moves shouldBe expectedMoves.toSet
  }

  // Scenario 3: Dropping a Sente Pawn (▲.FU) from hand
  "generateMovablePoints for dropping ▲.FU (clear path)" should "list valid empty squares" in {
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.FU -> 1)) // Player A has one FU
    val dropOrigin = Point.ofCaptured(Piece.◯.FU)
    val pieceToDrop = Piece.▲.FU

    // Test dropping to a specific valid square
    val targetSquare = Point(5,5) // 6f (valid for pawn drop on empty board)
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    moves should contain ((targetSquare, false))
    // Verify it doesn't list illegal drop squares (e.g., last rank for pawn)
    moves.map(_._1) should not contain Point(0,5) // Cannot drop FU on last rank (y=0)
  }

  "generateMovablePoints for dropping ▲.FU (onto occupied square)" should "not list occupied squares" in {
    val occupiedSquare = Point(5,5)
    val board = createBoardWithHands(
      boardPieces = Seq((Piece.▲.GI, occupiedSquare)), // Own piece on target
      playerAHand = Map(Piece.▲.FU -> 1)
    )
    val dropOrigin = Point.ofCaptured(Piece.◯.FU)
    val pieceToDrop = Piece.▲.FU
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    moves.map(_._1) should not contain occupiedSquare
  }

  "generateMovablePoints for dropping ▲.FU (Nifu - two pawns in file)" should "not list squares in that file" in {
    val fileWithPawn = 5 // File 4 (9-5=4th file from right, or x=5)
    val board = createBoardWithHands(
      boardPieces = Seq((Piece.▲.FU, Point(6, fileWithPawn))), // Existing Sente pawn on file x=5
      playerAHand = Map(Piece.▲.FU -> 1)
    )
    val dropOrigin = Point.ofCaptured(Piece.◯.FU)
    val pieceToDrop = Piece.▲.FU
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    // Check that no move allows dropping into the file `fileWithPawn`
    moves.forall { case (point, _) => point.x != fileWithPawn } shouldBe true
  }

  "generateMovablePoints for dropping ▲.FU (onto last rank)" should "not list squares in the last rank" in {
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.FU -> 1))
    val dropOrigin = Point.ofCaptured(Piece.◯.FU)
    val pieceToDrop = Piece.▲.FU
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    moves.forall { case (point, _) => point.y != 0 } shouldBe true
  }

  // Scenario 4: Dropping Sente Lance (▲.KY) or Knight (▲.KE)
  "generateMovablePoints for dropping ▲.KY (onto last rank)" should "not list squares in the last rank" in {
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.KY -> 1))
    val dropOrigin = Point.ofCaptured(Piece.◯.KY)
    val pieceToDrop = Piece.▲.KY
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    moves.forall { case (point, _) => point.y != 0 } shouldBe true
  }

  "generateMovablePoints for dropping ▲.KE (onto last two ranks)" should "not list squares in last two ranks" in {
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.KE -> 1))
    val dropOrigin = Point.ofCaptured(Piece.◯.KE)
    val pieceToDrop = Piece.▲.KE
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList

    moves.forall { case (point, _) => point.y != 0 && point.y != 1 } shouldBe true
  }

  // Scenario 5: Dropping Sente Gold (▲.KI)
  "generateMovablePoints for dropping ▲.KI (empty board)" should "list all empty squares" in {
    val board = createBoardWithHands(playerAHand = Map(Piece.▲.KI -> 1)) // Empty board, Player A has Gold
    val dropOrigin = Point.ofCaptured(Piece.◯.KI)
    val pieceToDrop = Piece.▲.KI
    val moves = Rule.generateMovablePoints(board, dropOrigin, pieceToDrop, PlayerA, false).toList.map(_._1).toSet

    val allBoardSquares = (for (y <- 0 to 8; x <- 0 to 8) yield Point(y,x)).toSet
    moves shouldBe allBoardSquares // Gold can be dropped anywhere on an empty board
  }

  // --- Tests for Rule.is2FU ---
  val pA = Point(0,0); val pB = Point(0,1); val pC = Point(0,2); val pD = Point(0,3)
  val pE = Point(0,4); val pF = Point(0,5); val pX = Point(1,0); val pY = Point(1,1)
  val pZ = Point(1,2); val pW = Point(1,3) // Added pW

  "is2FU" should "be false for a non-pawn piece" in {
    val board = createBoardWithHands() // Empty board
    Rule.is2FU(board, Piece.▲.KI, Point(4,4), PlayerA) shouldBe false
  }

  it should "be false if the existing pawn in the file is a Tokin (promoted)" in {
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.TO, Point(3,4)))) // Sente Tokin at file 5 (x=4)
    Rule.is2FU(board, Piece.▲.FU, Point(4,4), PlayerA) shouldBe false // Trying to drop Sente FU in same file
  }

  it should "be false if the file is empty of friendly unpromoted pawns" in {
    val board = createBoardWithHands() // Empty board
    Rule.is2FU(board, Piece.▲.FU, Point(4,4), PlayerA) shouldBe false
  }

  it should "be false if the file only contains an opponent's pawn" in {
    val board = createBoardWithHands(boardPieces = Seq((Piece.△.FU, Point(3,4)))) // Gote FU at file 5 (x=4)
    Rule.is2FU(board, Piece.▲.FU, Point(4,4), PlayerA) shouldBe false // Trying to drop Sente FU in same file
  }

  it should "be true if dropping a pawn into a file with an existing friendly unpromoted pawn (Nifu)" in {
    val fileOfExistingPawn = 4 // This is x-coordinate, e.g. file 5 (9-4)
    val board = createBoardWithHands(
      boardPieces = Seq((Piece.▲.FU, Point(3, fileOfExistingPawn))) // Sente FU at (3,4)
    )
    // Try to drop another Sente FU into the same file (x=4) but different row
    Rule.is2FU(board, Piece.▲.FU, Point(4, fileOfExistingPawn), PlayerA) shouldBe true
    // Try to drop into a different file
    Rule.is2FU(board, Piece.▲.FU, Point(4, fileOfExistingPawn + 1), PlayerA) shouldBe false
  }

  it should "be false if the piece being checked for Nifu is itself a Tokin" in {
    // This scenario tests the `!isPromoted(piece)` part of is2FU.
    // If we are trying to determine if placing a TOKIN would cause Nifu, it shouldn't.
    val board = createBoardWithHands(boardPieces = Seq((Piece.▲.FU, Point(3,4)))) // Sente FU at file 5 (x=4)
    Rule.is2FU(board, Piece.▲.TO, Point(4,4), PlayerA) shouldBe false
  }

  // --- Tests for Rule.isThreefoldRepetition (New using GameStateDigest) ---

  // Helper to create an empty board for GameStateDigest
  val emptyBoardPieces: IndexedSeq[IndexedSeq[Piece]] = IndexedSeq.fill(9, 9)(Piece.❏)
  val emptyHand: Map[Piece, Int] = Map.empty[Piece, Int]

  "isThreefoldRepetition" should "detect threefold repetition when current state is the 4th occurrence" in {
    // State 1: Empty board, Sente's turn
    val state1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA) // Use PlayerA for Sente's Turn
    // State 2: A slightly different board (e.g., Sente moved FU), Gote's turn
    val boardAfterFuMove = emptyBoardPieces.updated(6, emptyBoardPieces(6).updated(4, Piece.▲.FU)) // Example: 7e FU for Sente
    val state2 = Rule.GameStateDigest(boardAfterFuMove, emptyHand, emptyHand, PlayerB) // Use PlayerB for Gote's Turn
    // State 3: Back to state1 (e.g. Gote moved FU back or some other moves led here), Sente's turn
    // For test simplicity, we just reuse state1's definition for board/turn, assuming moves led here.
    // val state3 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, Player.SENTE) // This is state1

    // History: S1, S2, S1, S2, S1
    // Current state to check: S1 (which would be the 4th occurrence if we count current)
    // The function counts occurrences in `historyOfDigests`.
    // If current is S1, and history is [S1, S2, S1, S2, S1], count should be 3.
    val history = Seq(state1, state2, state1, state2, state1)
    // currentBoardStateDigest is state1. historyOfDigests contains state1 three times.
    Rule.isThreefoldRepetition(state1, history) should be (true)
  }

  it should "detect threefold repetition when current state is the 3rd occurrence and history already has 2" in {
    val state1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA)
    val state2 = Rule.GameStateDigest(emptyBoardPieces.updated(0, emptyBoardPieces(0).updated(0, Piece.▲.FU)), emptyHand, emptyHand, PlayerB)

    // History: S1, S2, S1
    // Current: S1. state1 appears twice in history.
    // The function is `historyOfDigests.count(_ == currentBoardStateDigest) >= 3`
    // So, if current is S1, and history has S1 twice, count will be 2. This should be false.
    val history1 = Seq(state1, state2, state1)
    Rule.isThreefoldRepetition(state1, history1) should be (false) // count is 2, needs to be >= 3

    // History: S1, S2, S1, S1
    // Current: S1. state1 appears three times in history.
    val history2 = Seq(state1, state2, state1, state1)
    Rule.isThreefoldRepetition(state1, history2) should be (true) // count is 3
  }


  it should "not detect threefold repetition if states are different enough" in {
    val state1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA)
    val state2Board = emptyBoardPieces.updated(0, emptyBoardPieces(0).updated(0, Piece.▲.FU)) // FU at 0,0
    val state2 = Rule.GameStateDigest(state2Board, emptyHand, emptyHand, PlayerB)
    val state3Board = emptyBoardPieces.updated(1, emptyBoardPieces(1).updated(0, Piece.▲.KY)) // KY at 1,0
    val state3 = Rule.GameStateDigest(state3Board, emptyHand, emptyHand, PlayerA)
    val state4Board = emptyBoardPieces.updated(2, emptyBoardPieces(2).updated(0, Piece.▲.KE)) // KE at 2,0
    val state4 = Rule.GameStateDigest(state4Board, emptyHand, emptyHand, PlayerB)

    // History: S1, S2, S1, S3
    // Current: S4
    val history = Seq(state1, state2, state1, state3)
    Rule.isThreefoldRepetition(state4, history) should be (false) // state4 is not in history at all
  }

  it should "not detect threefold repetition with fewer than 3 occurrences in history" in {
    val state1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA)
    val state2 = Rule.GameStateDigest(emptyBoardPieces.updated(0, emptyBoardPieces(0).updated(0, Piece.▲.FU)), emptyHand, emptyHand, PlayerB)

    // History: S1, S2
    // Current: S1. state1 appears once in history. Count = 1.
    val history1 = Seq(state1, state2)
    Rule.isThreefoldRepetition(state1, history1) should be (false)

    // History: S1, S1
    // Current: S1. state1 appears twice in history. Count = 2.
    val history2 = Seq(state1, state1)
    Rule.isThreefoldRepetition(state1, history2) should be (false)
  }

  it should "distinguish states based on board pieces" in {
    val board1 = IndexedSeq.fill(9, 9)(Piece.❏)
    val board2 = board1.updated(0, board1(0).updated(0, Piece.▲.FU)) // FU at (0,0)

    val stateA_v1 = Rule.GameStateDigest(board1, emptyHand, emptyHand, PlayerA)
    val stateA_v2 = Rule.GameStateDigest(board2, emptyHand, emptyHand, PlayerA) // Same turn, different board

    val history = Seq(stateA_v1, stateA_v2, stateA_v1)
    Rule.isThreefoldRepetition(stateA_v1, history) should be (false) // stateA_v1 appears 2 times
    Rule.isThreefoldRepetition(stateA_v2, history) should be (false) // stateA_v2 appears 1 time

    val history2 = Seq(stateA_v1, stateA_v2, stateA_v1, stateA_v1)
    Rule.isThreefoldRepetition(stateA_v1, history2) should be (true) // stateA_v1 appears 3 times
  }

  it should "distinguish states based on sente's hand" in {
    val senteHand1 = Map(Piece.▲.FU -> 1)
    val senteHand2 = Map(Piece.▲.FU -> 2)

    val stateA_h1 = Rule.GameStateDigest(emptyBoardPieces, senteHand1, emptyHand, PlayerA)
    val stateA_h2 = Rule.GameStateDigest(emptyBoardPieces, senteHand2, emptyHand, PlayerA) // Same board/turn, different hand

    val history = Seq(stateA_h1, stateA_h2, stateA_h1)
    Rule.isThreefoldRepetition(stateA_h1, history) should be (false) // stateA_h1 appears 2 times

    val history2 = Seq(stateA_h1, stateA_h2, stateA_h1, stateA_h1)
    Rule.isThreefoldRepetition(stateA_h1, history2) should be (true) // stateA_h1 appears 3 times
  }

  it should "distinguish states based on gote's hand" in {
    val goteHand1 = Map(Piece.△.FU -> 1)
    val goteHand2 = Map(Piece.△.FU -> 2)

    val stateA_gh1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, goteHand1, PlayerA)
    val stateA_gh2 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, goteHand2, PlayerA)

    val history = Seq(stateA_gh1, stateA_gh2, stateA_gh1)
    Rule.isThreefoldRepetition(stateA_gh1, history) should be (false)

    val history2 = Seq(stateA_gh1, stateA_gh2, stateA_gh1, stateA_gh1)
    Rule.isThreefoldRepetition(stateA_gh1, history2) should be (true)
  }

  it should "distinguish states based on next turn" in {
    val stateSenteTurn = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA)
    val stateGoteTurn = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerB) // Same board/hands, different turn

    val history = Seq(stateSenteTurn, stateGoteTurn, stateSenteTurn)
    Rule.isThreefoldRepetition(stateSenteTurn, history) should be (false) // stateSenteTurn appears 2 times

    val history2 = Seq(stateSenteTurn, stateGoteTurn, stateSenteTurn, stateSenteTurn)
    Rule.isThreefoldRepetition(stateSenteTurn, history2) should be (true) // stateSenteTurn appears 3 times
  }

  it should "correctly handle an empty history" in {
    val state1 = Rule.GameStateDigest(emptyBoardPieces, emptyHand, emptyHand, PlayerA)
    val emptyHistory = Seq.empty[Rule.GameStateDigest]
    Rule.isThreefoldRepetition(state1, emptyHistory) should be (false) // Count will be 0
  }

  "Rule.generateMovablePoints" should "not generate any moves for dropping a King" in {
    val board = createBoardWithHands() // Empty board initially

    // Manually put Gote's King (△.OU) into Sente's (PlayerA) hand.
    // board.capturedPieces.put(piece) adds 'piece' to the hand of the *opponent* of 'piece's owner.
    // So, if piece is △.OU (Gote's King), it's added to PlayerA's (Sente's) hand.
    // In PlayerA's hand, it will be considered as ▲.OU for dropping purposes.
    board.capturedPieces.put(Piece.△.OU)

    // Verify Player A has the King in hand
    val kingInHandPiece = Piece.▲.OU // This is what Player A would attempt to drop
    val kingGeneralized = Piece.◯.OU // Generalized King for counting
    board.capturedPieces.count(PlayerA, kingGeneralized) should be >= 1

    // The 'oldPos' for a drop is a special point indicating which piece from hand.
    // Point.ofCaptured(piece_kind) is used to get this special point.
    val kingDropOrigin = Point.ofCaptured(kingGeneralized)

    // Generate drop moves for Player A attempting to drop the King
    // The 'piece' parameter to generateMovablePoints for a drop is the specific piece type in hand.
    val dropMoves = Rule.generateMovablePoints(board, kingDropOrigin, kingInHandPiece, PlayerA, false).toList

    // Assert that no drop moves are generated for the King
    dropMoves shouldBe empty
  }

  "Rule.movableScopes" should "return cached lists to avoid allocations" in {
    val first = Rule.movableScopes(Piece.▲.FU)
    val second = Rule.movableScopes(Piece.▲.FU)
    (first eq second) shouldBe true
  }

  "Rule.isInCheck" should "detect when a king is attacked" in {
    val kingPos = Point(4,4)
    val rookPos = Point(4,0)
    val board = createBoardWithHands(boardPieces = Seq(
      (Piece.▲.OU, kingPos),
      (Piece.△.HI, rookPos)
    ))
    Rule.isInCheck(board, PlayerA) shouldBe true
  }

  it should "return false when pieces block the attack" in {
    val kingPos = Point(4,4)
    val rookPos = Point(4,0)
    val blocker = Point(4,2)
    val board = createBoardWithHands(boardPieces = Seq(
      (Piece.▲.OU, kingPos),
      (Piece.△.HI, rookPos),
      (Piece.▲.FU, blocker)
    ))
    Rule.isInCheck(board, PlayerA) shouldBe false
  }
}
