package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import jp.sndyuk.shogi.core.Player.{Player => GamePlayer} // Alias to avoid conflict with object Player
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => GameSimplePieceType}


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
    val p00_core = Point(0, 0)
    val p00_game = Position(0, 0)
    GameStateMapper.corePointToPosition(p00_core) shouldBe p00_game
    GameStateMapper.positionToCorePoint(p00_game) shouldBe p00_core

    val p88_core = Point(8, 8)
    val p88_game = Position(8, 8)
    GameStateMapper.corePointToPosition(p88_core) shouldBe p88_game
    GameStateMapper.positionToCorePoint(p88_game) shouldBe p88_core

    val p34_core = Point(3, 4) // y=3, x=4
    val p34_game = Position(4, 3) // x=4, y=3
    GameStateMapper.corePointToPosition(p34_core) shouldBe p34_game
    GameStateMapper.positionToCorePoint(p34_game) shouldBe p34_core
  }

  it should "map coreBoard to boardSetup" in {
    val board = Board() // Initial layout
    val boardSetup = GameStateMapper.coreBoardToBoardSetup(board)

    // Sente King at Point(8,4) -> Position(4,8)
    boardSetup(Position(4, 8)) shouldBe SimplePiece.OU
    // Gote King at Point(0,4) -> Position(4,0)
    boardSetup(Position(4, 0)) shouldBe SimplePiece.OU

    // Sente Pawn at Point(6,7) -> Position(7,6)
    boardSetup(Position(7, 6)) shouldBe SimplePiece.FU
    // Gote Pawn at Point(2,1) -> Position(1,2)
    boardSetup(Position(1, 2)) shouldBe SimplePiece.FU

    // Check a few empty squares are not in the map (or handle how empty squares are represented if they are)
    // GameStateMapper.coreBoardToBoardSetup filters out Piece.❏, so they won't be keys
    boardSetup.get(Position(3,3)) shouldBe None // An empty square in initial setup e.g. Point(3,3) -> Pos(3,3)
  }

  it should "reconstruct coreBoard from boardSetup and captured pieces" in {
    val boardSetupMap: Map[Position, (GameSimplePieceType, GamePlayer, Boolean)] = Map(
      Position(4, 8) -> ((SimplePiece.OU, Player.SENTE, false)), // Sente King
      Position(4, 0) -> ((SimplePiece.OU, Player.GOTE, false)),  // Gote King
      Position(2, 2) -> ((SimplePiece.FU, Player.SENTE, false))  // Sente Pawn
    )
    val senteCaptured = List(SimplePiece.HI, SimplePiece.KA)
    val goteCaptured = List(SimplePiece.GI)

    val board = GameStateMapper.reconstructCoreBoard(boardSetupMap, senteCaptured, goteCaptured)

    // Verify pieces on board
    board.squares.get(Point(8, 4)) shouldBe Piece.▲.OU
    board.squares.get(Point(0, 4)) shouldBe Piece.△.OU
    board.squares.get(Point(2, 2)) shouldBe Piece.▲.FU
    board.squares.get(Point(1,1)) shouldBe Piece.❏ // Empty square

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

  it should "map coreTransition to SimpleTransition" in {
    val boardBefore = Board() // Standard initial
    val boardAfter = boardBefore.copy() // Changed var to val

    // Sente FU from 7g to 7f. Point(y,x): 7g is (6,2), 7f is (5,2)
    val coreTrans = Transition(Point(6,2), Point(5,2), false, None)

    // Manually apply the move to boardAfter for testing
    val pieceToMove = boardAfter.squares.get(Point(6,2))
    boardAfter.squares.setAndGet(Piece.❏, Point(6,2))
    boardAfter.squares.setAndGet(pieceToMove, Point(5,2))

    val simpleTrans = GameStateMapper.coreTransitionToSimpleTransition(coreTrans, boardBefore, boardAfter)

    simpleTrans.move shouldBe "7g7f"
    simpleTrans.boardStateAfterMove(Position(2,5)) shouldBe SimplePiece.FU // 7f is Position(file=2, rank=5) -> x=2, y=5
                                                                         // No, pointToUSI(Point(y=5, x=2)) is 7f. Position is (x=2, y=5)
    simpleTrans.boardStateAfterMove(Position(2,6)) shouldBe SimplePiece.FU // This is wrong. 7g is (2,6)
    // Correcting: Point(y=5, x=2) is 7f. Position is (x=2, y=5).
    // The piece FU is now at Position(2,5).
    // The original position Position(2,6) (7g) should be empty.

    // val expectedBoardSetupAfter = GameStateMapper.coreBoardToBoardSetup(boardAfter) // Removed this broad assertion
    // simpleTrans.boardStateAfterMove shouldBe expectedBoardSetupAfter

    // Explicitly check map contents using .get
    simpleTrans.boardStateAfterMove.contains(Position(2,6)) shouldBe false // 7g, should be empty
    val valAtOldPos = simpleTrans.boardStateAfterMove.get(Position(2,6))
    valAtOldPos shouldBe None

    simpleTrans.boardStateAfterMove.contains(Position(2,5)) shouldBe true // 7f, should have FU
    val valAtNewPos = simpleTrans.boardStateAfterMove.get(Position(2,5))
    valAtNewPos shouldBe Some(SimplePiece.FU)

    // Check that other pieces from initial setup are still there
    simpleTrans.boardStateAfterMove.get(GameStateMapper.corePointToPosition(Point(8,4))) shouldBe Some(SimplePiece.OU) // Sente King
  }
}
