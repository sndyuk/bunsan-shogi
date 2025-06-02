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

  "28HI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(2, 8)
    val piece = ▲.HI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(1, 8), false), (Board.humanReadableToPoint(3, 8), false), (Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(5, 8), false), (Board.humanReadableToPoint(6, 8), false), (Board.humanReadableToPoint(7, 8), false))
  }

  "88KA" should "not be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(8, 8)
    val piece = ▲.KA
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream shouldBe empty
  }

  // --- Tests for Rule.movableScopes ---

  "▲.FU (Sente Pawn)" should "have correct movable scopes" in {
    val piece = ▲.FU
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-1, 0, false))
  }

  "▲.KY (Sente Lance)" should "have correct movable scopes" in {
    val piece = ▲.KY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-1, 0, Rule.∞))
  }

  "▲.KE (Sente Knight)" should "have correct movable scopes" in {
    val piece = ▲.KE
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-2, -1, false), (-2, 1, false))
  }

  "▲.GI (Sente Silver)" should "have correct movable scopes" in {
    val piece = ▲.GI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (1, -1, false), (1, 1, false)
    )
  }

  "▲.KI (Sente Gold)" should "have correct movable scopes" in {
    val piece = ▲.KI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.KA (Sente Bishop)" should "have correct movable scopes" in {
    val piece = ▲.KA
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "▲.HI (Sente Rook)" should "have correct movable scopes" in {
    val piece = ▲.HI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
  }

  "▲.OU (Sente King)" should "have correct movable scopes" in {
    val piece = ▲.OU
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false),
      (0, -1, false), (0, 1, false),
      (1, 0, false), (1, -1, false), (1, 1, false)
    )
  }

  // Promoted Sente Pieces
  "▲.TO (Promoted Sente Pawn)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.TO
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NY (Promoted Sente Lance)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NK (Promoted Sente Knight)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NK
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NG (Promoted Sente Silver)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NG
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.UM (Promoted Sente Bishop)" should "have correct movable scopes" in {
    val piece = ▲.UM
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from ▲.OU)
      (-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false),
      (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false),
      // Bishop moves (from ▲.KA)
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "▲.RY (Promoted Sente Rook)" should "have correct movable scopes" in {
    val piece = ▲.RY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from ▲.OU)
      (-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false),
      (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false),
      // Rook moves (from ▲.HI)
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
  }

  // --- Tests for Gote Rule.movableScopes ---

  "△.FU (Gote Pawn)" should "have correct movable scopes" in {
    val piece = △.FU
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((1, 0, false))
  }

  "△.KY (Gote Lance)" should "have correct movable scopes" in {
    val piece = △.KY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((1, 0, Rule.∞))
  }

  "△.KE (Gote Knight)" should "have correct movable scopes" in {
    val piece = △.KE
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((2, -1, false), (2, 1, false))
  }

  "△.GI (Gote Silver)" should "have correct movable scopes" in {
    val piece = △.GI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (-1, -1, false), (-1, 1, false)
    )
  }

  "△.KI (Gote Gold)" should "have correct movable scopes" in {
    val piece = △.KI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false)
    )
  }

  "△.KA (Gote Bishop)" should "have correct movable scopes" in {
    val piece = △.KA
    val scopes = Rule.movableScopes(piece)
    // Note: Bishop moves are symmetrical for Sente and Gote in terms of deltas
    scopes should contain theSameElementsAs List(
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "△.HI (Gote Rook)" should "have correct movable scopes" in {
    val piece = △.HI
    val scopes = Rule.movableScopes(piece)
    // Note: Rook moves are symmetrical for Sente and Gote in terms of deltas
    scopes should contain theSameElementsAs List(
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
  }

  "△.OU (Gote King)" should "have correct movable scopes" in {
    val piece = △.OU
    val scopes = Rule.movableScopes(piece)
    // Note: King moves are symmetrical
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false),
      (0, -1, false), (0, 1, false),
      (1, 0, false), (1, -1, false), (1, 1, false)
    )
  }

  // Promoted Gote Pieces
  "△.TO (Promoted Gote Pawn)" should "have correct movable scopes (same as Gote Gold)" in {
    val piece = △.TO
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false)
    )
  }

  "△.NY (Promoted Gote Lance)" should "have correct movable scopes (same as Gote Gold)" in {
    val piece = △.NY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false)
    )
  }

  "△.NK (Promoted Gote Knight)" should "have correct movable scopes (same as Gote Gold)" in {
    val piece = △.NK
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false)
    )
  }

  "△.NG (Promoted Gote Silver)" should "have correct movable scopes (same as Gote Gold)" in {
    val piece = △.NG
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (1, 0, false), (1, -1, false), (1, 1, false), (0, -1, false), (0, 1, false), (-1, 0, false)
    )
  }

  "△.UM (Promoted Gote Bishop)" should "have correct movable scopes" in {
    val piece = △.UM
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from △.OU)
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false),
      (1, 0, false), (1, -1, false), (1, 1, false),
      // Bishop moves (from △.KA)
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "△.RY (Promoted Gote Rook)" should "have correct movable scopes" in {
    val piece = △.RY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from △.OU)
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false),
      (1, 0, false), (1, -1, false), (1, 1, false),
      // Rook moves (from △.HI)
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
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

  // --- Tests for Rule.isThreefoldRepetition ---
  // Note: Board state is not used by current isThreefoldRepetition, only history.
  // Points for transitions
  val tP0 = Point(0,0); val tP1 = Point(0,1); val tP2 = Point(0,2); val tP3 = Point(0,3)
  val tP4 = Point(0,4); val tP5 = Point(0,5); val tP6 = Point(0,6); val tP7 = Point(0,7)
  val tP8 = Point(0,8); val tP9 = Point(1,0)

  "isThreefoldRepetition" should "be false for insufficient history (less than 5 moves)" in {
    val state = State(List(
      Transition(tP0, tP1, false, None), // Sente
      Transition(tP2, tP3, false, None), // Gote
      Transition(tP4, tP5, false, None), // Sente
      Transition(tP6, tP7, false, None)  // Gote
    ), PlayerA) // Sente's turn, Gote made last move. List size = 4
    Rule.isThreefoldRepetition(Board(), state) shouldBe false
  }

  it should "be true if current player's move destination matches their previous two destinations" in {
    // Sente moves to tP1, Gote to tP3, Sente to tP1, Gote to tP5, Sente to tP1
    val history = List(
      Transition(tP0, tP1, false, None), // Sente (current, idx 0) to tP1
      Transition(tP2, tP3, false, None), // Gote (idx 1)
      Transition(tP4, tP1, false, None), // Sente (idx 2) to tP1
      Transition(tP6, tP5, false, None), // Gote (idx 3)
      Transition(tP8, tP1, false, None)  // Sente (idx 4) to tP1
    ) // size = 5
    val state = State(history, PlayerB) // Player B's turn, Sente made last move his(0)
    Rule.isThreefoldRepetition(Board(), state) shouldBe true // Checks his(0), his(2), his(4)
  }

  it should "be true if opponent's last move destination matches their previous two destinations" in {
    // Original history setup for this test was split; this part was unused.
    // val history = List(
    //   Transition(tP0, tP1, false, None), // Gote (current, idx 0) to tP1
    //   Transition(tP2, tP0, false, None), // Sente (idx 1)
    //   Transition(tP3, tP1, false, None), // Gote (idx 2) to tP1
    //   Transition(tP4, tP2, false, None), // Sente (idx 3)
    //   Transition(tP5, tP1, false, None), // Gote (idx 4) to tP1
    //   Transition(tP6, tP4, false, None)  // Sente (idx 5)
    // ) // size = 6
    // If it's Gote's turn (Sente made last move at history(0))
    // then we check history(1), history(3), history(5) for Gote's moves.
    // Let's use the "swapped order for clarity" version directly:
    val historyForOpponentCheck = List(
      Transition(tP6, tP4, false, None),  // Sente (idx 0)
      Transition(tP5, tP1, false, None), // Gote (idx 1) to tP1
      Transition(tP4, tP2, false, None), // Sente (idx 2)
      Transition(tP3, tP1, false, None), // Gote (idx 3) to tP1
      Transition(tP2, tP0, false, None), // Sente (idx 4)
      Transition(tP0, tP1, false, None)  // Gote (idx 5) to tP1
    )
    val state = State(historyForOpponentCheck, PlayerA) // Player A's turn, Gote made last move his(0)
    Rule.isThreefoldRepetition(Board(), state) shouldBe true // Checks his(1), his(3), his(5)
  }

  it should "be false if destinations do not repeat sufficiently" in {
    val history = List(
      Transition(tP0, tP1, false, None),
      Transition(tP2, tP3, false, None),
      Transition(tP4, tP5, false, None),
      Transition(tP6, tP0, false, None), // Different dest
      Transition(tP8, tP2, false, None),
      Transition(tP7, tP4, false, None)
    ) // size = 6
    val state = State(history, PlayerA)
    Rule.isThreefoldRepetition(Board(), state) shouldBe false
  }

  it should "be false for an interrupted sequence for current player" in {
    // Sente to tP1, Gote to tP3, Sente to tP1, Gote to tP5, Sente to tP0 (different)
    val history = List(
      Transition(tP8, tP0, false, None), // Sente (current, idx 0) to tP0
      Transition(tP6, tP5, false, None), // Gote (idx 1)
      Transition(tP4, tP1, false, None), // Sente (idx 2) to tP1
      Transition(tP2, tP3, false, None), // Gote (idx 3)
      Transition(tP0, tP1, false, None)  // Sente (idx 4) to tP1
    ) // size = 5
    val state = State(history, PlayerB)
    Rule.isThreefoldRepetition(Board(), state) shouldBe false
  }

  // Test cases for the second block of checks (his(0) vs his(3) vs his(6), etc.)
  // These checks compare newPos of moves made by different players in some cases,
  // so they are testing the code as written, not necessarily standard shogi rules.

  it should "trigger repetition on pattern: S(X) G(X) S(Y) G(X) S(Z) G(W) S(X) - (his(0)==his(3)==his(6) based on newPos)" in {
    // History (newest to oldest):
    // his(0): Sente to pX
    // his(1): Gote to pW
    // his(2): Sente to pZ
    // his(3): Gote to pX  <-
    // his(4): Sente to pY
    // his(5): Gote to pX
    // his(6): Sente to pX  <-
    val history = List(
      Transition(Point(0,0), pX, false, None), // S0: Sente to pX
      Transition(Point(1,1), pW, false, None), // G0
      Transition(Point(2,2), pZ, false, None), // S1
      Transition(Point(3,3), pX, false, None), // G1: Gote to pX
      Transition(Point(4,4), pY, false, None), // S2
      Transition(Point(5,5), pX, false, None), // G2: Gote to pX -- this Gote move actually does not fit the pattern name, but the code compares his(0) with his(3)
      Transition(Point(6,6), pX, false, None)  // S3: Sente to pX
    ) // size = 7. Player B's turn next. Sente made the last move.
    val state = State(history, PlayerB)
    // Rule checks if (size >= 7 && same(his(0),his(3)) && same(his(0),his(6)))
    // his(0).newPos = pX
    // his(3).newPos = pX (Gote's move)
    // his(6).newPos = pX (Sente's move)
    // This will be true because all newPos are pX.
    Rule.isThreefoldRepetition(Board(), state) shouldBe true
  }

  it should "NOT trigger repetition if pattern S(X) G(A) S(Y) G(B) S(Z) G(C) S(X) (his(0)==his(6) but his(0)!=his(3))" in {
    val history = List(
      Transition(Point(0,0), pX, false, None), // S0: Sente to pX
      Transition(Point(1,1), pC, false, None), // G0
      Transition(Point(2,2), pZ, false, None), // S1
      Transition(Point(3,3), pB, false, None), // G1
      Transition(Point(4,4), pY, false, None), // S2
      Transition(Point(5,5), pA, false, None), // G2
      Transition(Point(6,6), pX, false, None)  // S3: Sente to pX
    ) // size = 7.
    val state = State(history, PlayerB)
    // his(0).newPos = pX
    // his(3).newPos = pB
    // his(6).newPos = pX
    // same(his(0),his(3)) is false.
    Rule.isThreefoldRepetition(Board(), state) shouldBe false
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
}
