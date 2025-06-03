package jp.sndyuk.shogi.kifu

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import jp.sndyuk.shogi.core.{
  Piece => CorePieceObject, // Alias for the Piece object
  Point => CorePoint,
  // Turn => CoreTurnTrait, // Removed unused alias
  Transition => CoreTransition,
  Board => CoreBoard,
  PlayerA => CorePlayerA,
  PlayerB => CorePlayerB,
  SimplePiece => CoreSimplePiece
}
// Type Piece = Int is brought into scope by jp.sndyuk.shogi.core package object,
// used by KifuMapper's function signatures. No explicit import for it needed here for that.
import jp.sndyuk.shogi.kifu.{TempCore => KifuTempCore}
import jp.sndyuk.shogi.kifu.KifuMapper._ // Import all members of KifuMapper

class KifuMapperSpec extends AnyFlatSpec with Matchers {

  "KifuMapper" should "map coreTurn to KifuPlayer" in {
    coreTurnToKifuPlayer(CorePlayerA) shouldBe KifuTempCore.SENTE
    coreTurnToKifuPlayer(CorePlayerB) shouldBe KifuTempCore.GOTE
  }

  it should "map corePoint to KifuPosition" in {
    // core.Point(y,x) -> KifuTempCore.Position(file=9-x, rank=y+1)
    corePointToKifuPosition(CorePoint(0,0)) shouldBe KifuTempCore.Position(9,1) // 9a
    corePointToKifuPosition(CorePoint(8,8)) shouldBe KifuTempCore.Position(1,9) // 1i

    // Sente's Pawn at 7g is core.Point(y=6, x=2) -> Kifu Position(7,7)
    corePointToKifuPosition(CorePoint(6,2)) shouldBe KifuTempCore.Position(7,7)

    // Sente's Rook at 2h is core.Point(y=7, x=7) -> Kifu Position(2,8)
    corePointToKifuPosition(CorePoint(7,7)) shouldBe KifuTempCore.Position(2,8)
  }

  it should "map corePiece (raw Int type) to base KifuTempCore.Piece" in {
    corePieceToKifuPiece(CorePieceObject.▲.FU) shouldBe KifuTempCore.FU
    corePieceToKifuPiece(CorePieceObject.△.HI) shouldBe KifuTempCore.HI
    corePieceToKifuPiece(CorePieceObject.▲.RY) shouldBe KifuTempCore.HI
    corePieceToKifuPiece(CorePieceObject.◯.KI) shouldBe KifuTempCore.KI
    assertThrows[IllegalArgumentException] {
      corePieceToKifuPiece(CorePieceObject.❏) // Corrected: Was CorePieceObject.Piece.❏
    }
  }

  it should "map SimplePieceType to KifuTempCore.Piece" in {
    simplePieceTypeToKifuPiece(CoreSimplePiece.FU) shouldBe KifuTempCore.FU
    simplePieceTypeToKifuPiece(CoreSimplePiece.KA) shouldBe KifuTempCore.KA
    simplePieceTypeToKifuPiece(CoreSimplePiece.OU) shouldBe KifuTempCore.OU
  }

  val initialBoard = CoreBoard()

  it should "map a standard move core.Transition to KifuTempCore.Move" in {
    // Sente's FU at 7g (core.Point(6,2)) to 7f (core.Point(5,2))
    val coreTrans = CoreTransition(CorePoint(6,2), CorePoint(5,2), false, None)
    val kifuMove = coreTransitionToKifuMove(coreTrans, CorePlayerA, initialBoard)

    kifuMove.player shouldBe KifuTempCore.SENTE
    kifuMove.from shouldBe Some(KifuTempCore.Position(7,7)) // 7g
    kifuMove.to shouldBe KifuTempCore.Position(7,6)     // 7f
    kifuMove.piece shouldBe KifuTempCore.FU
    kifuMove.promote shouldBe false
    kifuMove.isDrop shouldBe false
  }

  it should "map a promotion move core.Transition to KifuTempCore.Move" in {
    // Sente FU from 7g (core.Point(6,2)) to 7c (core.Point(2,2)), promoting
    val coreTrans = CoreTransition(CorePoint(6,2), CorePoint(2,2), true, None)
    val boardBefore = CoreBoard()

    val kifuMove = coreTransitionToKifuMove(coreTrans, CorePlayerA, boardBefore)

    kifuMove.player shouldBe KifuTempCore.SENTE
    kifuMove.from shouldBe Some(KifuTempCore.Position(7,7)) // 7g
    kifuMove.to shouldBe KifuTempCore.Position(7,3)     // 7c
    kifuMove.piece shouldBe KifuTempCore.FU // Base piece
    kifuMove.promote shouldBe true
    kifuMove.isDrop shouldBe false
  }

  it should "map a drop move core.Transition to KifuTempCore.Move" in {
    // Sente drops FU to 5e (core.Point(4,4))
    // oldPos for drop: Point.ofCaptured(Piece.◯.FU) is Point(y=9, x=2 for FU)
    val dropOldPos = CorePoint.ofCaptured(CorePieceObject.◯.FU) // This is Point(9,2)
    val coreTrans = CoreTransition(dropOldPos, CorePoint(4,4), false, None)

    val boardBeforeDrop = CoreBoard()
    val kifuMove = coreTransitionToKifuMove(coreTrans, CorePlayerA, boardBeforeDrop)

    kifuMove.player shouldBe KifuTempCore.SENTE
    kifuMove.from shouldBe None
    kifuMove.to shouldBe KifuTempCore.Position(5,5) // 5e
    kifuMove.piece shouldBe KifuTempCore.FU
    kifuMove.promote shouldBe false
    kifuMove.isDrop shouldBe true
  }
}
