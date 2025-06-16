package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// Unused aliases GamePlayer and GameSimplePieceType were removed. Direct usages like Player.SENTE are preferred.


class GameStateMapperSpec extends AnyFlatSpec with Matchers {

  // Reusable core pieces
  val SENTE_FU = Piece.▲.FU
  val SENTE_KI = Piece.▲.KI
  val SENTE_RY = Piece.▲.RY // Promoted Rook (HI)
  val SENTE_HI = Piece.▲.HI

  val GOTE_FU = Piece.△.FU
  val GOTE_KI = Piece.△.KI
  val GOTE_UM = Piece.△.UM // Promoted Bishop (KA)
  val GOTE_KA = Piece.△.KA
  val GOTE_GI = Piece.△.GI
  val GOTE_NG = Piece.△.NG // Promoted Silver (GI)


  "GameStateMapper" should "map coreTurn to Player and vice-versa" in {
    GameStateMapper.coreTurnToPlayer(PlayerA) shouldBe Player.SENTE
    GameStateMapper.coreTurnToPlayer(PlayerB) shouldBe Player.GOTE

    GameStateMapper.playerToCoreTurn(Player.SENTE) shouldBe PlayerA
    GameStateMapper.playerToCoreTurn(Player.GOTE) shouldBe PlayerB
  }

  it should "map corePiece to SimplePieceType, Player, and promotion flag" in {
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(Piece.❏) shouldBe None

    // Sente pieces
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(SENTE_FU) shouldBe Some((SimplePiece.FU, Player.SENTE, false))
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(SENTE_KI) shouldBe Some((SimplePiece.KI, Player.SENTE, false))
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(SENTE_RY) shouldBe Some((SimplePiece.HI, Player.SENTE, true)) // RY is promoted HI

    // Gote pieces
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(GOTE_FU) shouldBe Some((SimplePiece.FU, Player.GOTE, false))
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(GOTE_KI) shouldBe Some((SimplePiece.KI, Player.GOTE, false))
    GameStateMapper.corePieceToSimplePieceTypeAndPlayer(GOTE_UM) shouldBe Some((SimplePiece.KA, Player.GOTE, true)) // UM is promoted KA
  }

  it should "map SimplePieceType, Player, and promotion flag to corePiece" in {
    GameStateMapper.simplePiecePlayerToCorePiece(SimplePiece.FU, Player.SENTE, false) shouldBe SENTE_FU
    GameStateMapper.simplePiecePlayerToCorePiece(SimplePiece.HI, Player.SENTE, true) shouldBe SENTE_RY
    GameStateMapper.simplePiecePlayerToCorePiece(SimplePiece.KA, Player.GOTE, false) shouldBe GOTE_KA
    GameStateMapper.simplePiecePlayerToCorePiece(SimplePiece.GI, Player.GOTE, true) shouldBe GOTE_NG
  }

  it should "map core.Point to Position and vice-versa" in {
    // Valid Shogi positions (1-9 for file/rank)
    // Position(file, rank) maps to Point(y = rank-1, x = 9-file)

    val p11_game = Position(1, 1) // File 1, Rank 1 (bottom-right for Sente view if board inverted)
    val p11_core = Point(0, 8)    // y=0 (rank 'a'), x=8 (file '1' in USI, file 9 by 0-idx)
                                  // Corrected: x = 9-1 = 8, y = 1-1 = 0
    GameStateMapper.corePointToPosition(p11_core) shouldBe Position(1,1) // Check with corrected understanding
    GameStateMapper.positionToCorePoint(p11_game) shouldBe p11_core

    val p99_game = Position(9, 9) // File 9, Rank 9 (top-left for Sente view if board inverted)
    val p99_core = Point(8, 0)    // x = 9-9 = 0, y = 9-1 = 8
    GameStateMapper.corePointToPosition(p99_core) shouldBe Position(9,9)
    GameStateMapper.positionToCorePoint(p99_game) shouldBe p99_core

    val p54_game = Position(5, 4) // File 5, Rank 4
    val p54_core = Point(3, 4)    // x = 9-5 = 4, y = 4-1 = 3
    GameStateMapper.corePointToPosition(p54_core) shouldBe Position(5,4)
    GameStateMapper.positionToCorePoint(p54_game) shouldBe p54_core
  }

  // Helper to convert Position (1-indexed) to "x_y" string key (0-indexed core Point x,y)
  private def posToKey(pos: Position): String = {
    val coreP = GameStateMapper.positionToCorePoint(pos)
    s"${coreP.x}_${coreP.y}"
  }

  it should "map coreBoard to boardSetup (Map[String, PieceInfo])" ignore {
    val board = Board() // Initial layout
    val boardSetup = GameStateMapper.coreBoardToBoardSetup(board)

    // Sente King at Point(y=8, x=4) is Position(file=5, rank=9). Key "4_8"
    boardSetup(posToKey(Position(5,9))) shouldBe PieceInfo(SimplePiece.OU, Player.SENTE, false)
    // Gote King at Point(y=0, x=4) is Position(file=5, rank=1). Key "4_0"
    boardSetup(posToKey(Position(5,1))) shouldBe PieceInfo(SimplePiece.OU, Player.GOTE, false)

    // Sente Pawn at Point(y=6, x=2) is Position(file=7, rank=7). Key "2_6"
    boardSetup(posToKey(Position(7,7))) shouldBe PieceInfo(SimplePiece.FU, Player.SENTE, false)
    // Gote Pawn at Point(y=2, x=2) is Position(file=7, rank=3). Key "2_2"
    boardSetup(posToKey(Position(7,3))) shouldBe PieceInfo(SimplePiece.FU, Player.GOTE, false)

    // Check an empty square: Position(5,5) maps to core Point(y=4, x=4). Key "4_4"
    boardSetup.get(posToKey(Position(5,5))) shouldBe None
  }

  it should "reconstruct coreBoard from boardSetup (Map[String, PieceInfo]) and captured pieces" ignore {
    val boardSetupMap: Map[String, PieceInfo] = Map(
      posToKey(Position(5, 9)) -> PieceInfo(SimplePiece.OU, Player.SENTE, false), // Sente King
      posToKey(Position(5, 1)) -> PieceInfo(SimplePiece.OU, Player.GOTE, false),  // Gote King
      posToKey(Position(7, 7)) -> PieceInfo(SimplePiece.FU, Player.SENTE, false)  // Sente Pawn
    )
    val senteCaptured = List(SimplePiece.HI, SimplePiece.KA)
    val goteCaptured = List(SimplePiece.GI)

    val board = GameStateMapper.reconstructCoreBoard(boardSetupMap, senteCaptured, goteCaptured)

    // Verify pieces on board (using core Point(y,x) for board.squares.get)
    // Sente King: Position(5,9) -> Point(8,4)
    board.squares.get(Point(8, 4)) shouldBe Piece.▲.OU
    // Gote King: Position(5,1) -> Point(0,4)
    board.squares.get(Point(0, 4)) shouldBe Piece.△.OU
    // Sente Pawn: Position(7,7) -> Point(6,2)
    board.squares.get(Point(6, 2)) shouldBe Piece.▲.FU
    // Empty square: e.g. Point(0,0)
    board.squares.get(Point(0,0)) shouldBe Piece.❏

    // Verify captured pieces
    board.capturedPieces.count(PlayerA, Piece.◯.HI) shouldBe 1
    board.capturedPieces.count(PlayerA, Piece.◯.KA) shouldBe 1
    board.capturedPieces.count(PlayerA, Piece.◯.GI) shouldBe 0 // Sente should not have Gote's GI

    board.capturedPieces.count(PlayerB, Piece.◯.GI) shouldBe 1
    board.capturedPieces.count(PlayerB, Piece.◯.HI) shouldBe 0
  }

  // Helper for coreTransitionToMoveString tests
  val testBoardBeforeMove = Board() // Standard initial board

  it should "map simplePiece to USI char" in {
    GameStateMapper.simplePieceToUSIChar(SimplePiece.FU) shouldBe "P"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.HI) shouldBe "R"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.KA) shouldBe "B"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.KI) shouldBe "G"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.GI) shouldBe "S"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.KE) shouldBe "N"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.KY) shouldBe "L"
    GameStateMapper.simplePieceToUSIChar(SimplePiece.OU) shouldBe "K"
  }

  it should "map core.Point to USI string" in {
    // GameStateMapper has private pointToUSI, testing indirectly via coreTransitionToMoveString
    // If it were public:
    // GameStateMapper.pointToUSI(Point(y=6, x=7)) shouldBe "2g" // (9-7=2), ('a'+6='g') No, x is file (1-9), y is rank (a-i)
    // Shogi USI: file is 1-9, rank is a-i. Point(y,x) -> file 9-x, rank 'a'+y
    // Point(y=0, x=0) -> file 9, rank a -> "9a"
    // Point(y=8, x=8) -> file 1, rank i -> "1i"
    // Point(y=6, x=2) -> Sente FU from 7g (File 7, Rank g) is Point(y=6, x=2). USI: (9-2)=7, ('a'+6)=g => "7g"
    // Point(y=5, x=2) -> Sente FU to 7f (File 7, Rank f) is Point(y=5, x=2). USI: (9-2)=7, ('a'+5)=f => "7f"
    // This seems to be how `pointToUSI` in GameStateMapper is implemented: (9-point.x) and ('a'+point.y)
  }


  it should "map coreTransition to USI move string" in {
    // Sente FU from 7g to 7f. Point(y,x): 7g is (6,2), 7f is (5,2)
    val moveFu = Transition(Point(6,2), Point(5,2), false, None)
    GameStateMapper.coreTransitionToMoveString(moveFu, testBoardBeforeMove) shouldBe "7g7f"

    // Sente HI from 2h to 2b, promoting. 2h is (7,7), 2b is (1,7)
    // Assume some piece △.GI was captured at 2b for illustration of 'captured' field
    val moveHiPromote = Transition(Point(7,7), Point(1,7), true, Some(Piece.△.GI))
    GameStateMapper.coreTransitionToMoveString(moveHiPromote, testBoardBeforeMove) shouldBe "2h2b+"

    // Sente drops FU (Pawn) to 5e. 5e is Point(y=4, x=4)
    val dropFuSente = Transition(Point.ofCaptured(Piece.◯.FU), Point(4,4), false, None)
    GameStateMapper.coreTransitionToMoveString(dropFuSente, testBoardBeforeMove) shouldBe "P*5e"

    // Gote drops GI (Silver) to 4d. 4d is Point(y=3, x=5)
    // Note: coreTransitionToMoveString doesn't know whose turn it is, relies on Point.ofCaptured structure
    val dropGiGote = Transition(Point.ofCaptured(Piece.◯.GI), Point(3,5), false, None) // Corrected newPos Point(3,5) for "4d"
                                                                                      // pointToUSI(Point(3,5)) -> 9-5=4, 'a'+3=d -> "4d"
    GameStateMapper.coreTransitionToMoveString(dropGiGote, testBoardBeforeMove) shouldBe "S*4d"
  }

  it should "map coreTransition to SimpleTransition" ignore {
    val boardBefore = Board() // Standard initial
    val boardAfter = boardBefore.copy() // Changed var to val

    // Sente FU from 7g to 7f. Point(y,x): 7g is (6,2), 7f is (5,2)
    // Position: 7g is Position(7,7), 7f is Position(7,6)
    val coreTrans = Transition(Point(6,2), Point(5,2), false, None) // Sente moves FU 7g -> 7f

    // Manually apply the move to boardAfter for testing
    val pieceToMove = boardAfter.squares.get(Point(6,2)) // Get piece from 7g (core Point(6,2))
    boardAfter.squares.setAndGet(Piece.❏, Point(6,2))    // Empty 7g
    boardAfter.squares.setAndGet(pieceToMove, Point(5,2)) // Place piece at 7f (core Point(5,2))

    val simpleTrans = GameStateMapper.coreTransitionToSimpleTransition(coreTrans, boardBefore, boardAfter)

    simpleTrans.move shouldBe "7g7f"

    // Key for 7f (new position) is Point(y=5,x=2) -> "2_5"
    val newPosKey = posToKey(Position(7,6)) // Position(7,6) -> Point(5,2) -> "2_5"
    simpleTrans.boardStateAfterMove(newPosKey) shouldBe PieceInfo(SimplePiece.FU, Player.SENTE, false)

    // Key for 7g (old position) is Point(y=6,x=2) -> "2_6"
    val oldPosKey = posToKey(Position(7,7)) // Position(7,7) -> Point(6,2) -> "2_6"
    simpleTrans.boardStateAfterMove.get(oldPosKey) shouldBe None

    // Check that other pieces from initial setup are still there, e.g., Sente King
    // Sente King is at Position(5,9) -> Point(8,4) -> key "4_8"
    val senteKingKey = posToKey(Position(5,9))
    simpleTrans.boardStateAfterMove.get(senteKingKey) shouldBe Some(PieceInfo(SimplePiece.OU, Player.SENTE, false))
  }
}
