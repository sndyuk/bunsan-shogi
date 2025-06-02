package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AlphaBetaAI_V3_Spec extends AnyFlatSpec with Matchers {

  def setupBoard(pieces: List[(Piece, Point)], turn: Turn = PlayerA): (State, Board) = {
    val board = Board()
    for (y <- 0 to 8; x <- 0 to 8) {
      board.squares.setAndGet(Piece.❏, Point(y, x))
    }
    board.capturedPieces.playerA = 0
    board.capturedPieces.playerB = 0
    pieces.foreach { case (piece, pos) =>
      board.squares.setAndGet(piece, pos)
    }
    (State(Nil, turn), board)
  }

  behavior of "AlphaBetaAI_V3"

  it should "choose a move leading to a better PST score" in {
    val ai = new AlphaBetaAI_V3(
      searchDepth = 1,
      quiescenceSearchDepth = 2 // Quiescence depth, may not be heavily used here
    )

    // Sente Gold (KI) at Point(6,3). Sente King at Point(8,4), Gote King at Point(0,4).
    // Possible moves for Sente Gold from (6,3):
    // 1. (6,3) -> (5,3) (y=5, x=3). KI_PST_SENTE for (5,3):
    //    centerBonus = if (3>=2&&3<=6 && 5>=2&&5<=6) 6 else 0 = 6
    //    defenseBonus = if (5>5) (5-5) else 0 = 0
    //    Total = 6 + 0 + 6 = 12.
    // 2. (6,3) -> (7,3) (y=7, x=3). KI_PST_SENTE for (7,3):
    //    centerBonus = if (3>=2&&3<=6 && 7>=2&&7<=6) 6 else 0 = 0 (y=7 is not in center y-range 2-6)
    //    Correction: KI_PST_SENTE definition for centerBonus: (x >= 2 && x <= 6 && y >= 2 && y <= 6)
    //    For (7,3): y=7, so centerBonus = 0.
    //    defenseBonus = if (7>5) (7-5) else 0 = 2
    //    Total = 0 + 2 + 6 = 8.
    // This means (5,3) has PST 12, and (7,3) has PST 8. AI should prefer (5,3).

    val initialPieces = List(
      (Piece.▲.KI, Point(6,3)),
      (Piece.▲.OU, Point(8,4)), // Sente King
      (Piece.△.OU, Point(0,4))  // Gote King
    )
    val (initialState, initialBoard) = setupBoard(initialPieces, PlayerA)

    // Make sure these moves are legal for Gold from (6,3)
    // Gold can move one step orthogonally, or one step diagonally forward.
    // From (6,3):
    //  (5,2) diag fwd left
    //  (5,3) fwd
    //  (5,4) diag fwd right
    //  (6,2) left
    //  (6,4) right
    //  (7,3) back
    // So, (5,3) and (7,3) are legal.

    // Let's recalculate PSTs for these specific target squares based on EvaluationV3.scala:
    // Target 1: Point(5,3) (Sente KI moves here)
    //   y=5, x=3. centerBonus=(x>=2&&x<=6)=true && (y>=2&&y<=6)=true => 6. defenseBonus=(y>5)=false => 0.
    //   PST_KI(5,3) = 6 + 0 + 6 = 12.
    // Target 2: Point(7,3) (Sente KI moves here)
    //   y=7, x=3. centerBonus=(x>=2&&x<=6)=true && (y>=2&&y<=6)=false => 0. defenseBonus=(y>5)=true => (7-5)=2.
    //   PST_KI(7,3) = 0 + 2 + 6 = 8.

    // With searchDepth = 1, the AI evaluates states after one Sente move.
    // Score(state after KI to (5,3)) should be > Score(state after KI to (7,3)) due to PST.
    // Other eval terms (material, mobility for KI, king safety) should be similar.
    // Kings' PSTs are constant: OU_SENTE(8,4)=20, OU_GOTE(0,4)=OU_SENTE(8,4)=20. They cancel.

    val bestMoveOpt = ai.findBestMove(initialState, initialBoard, PlayerA, 1)

    bestMoveOpt should be (defined)
    bestMoveOpt.get.oldPos should be (Point(6,3))
    bestMoveOpt.get.newPos should be (Point(5,3)) // Expect move to (5,3) due to higher PST
  }
}
