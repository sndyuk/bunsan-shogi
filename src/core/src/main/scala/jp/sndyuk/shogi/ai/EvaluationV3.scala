package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._

// EvaluationV3 cannot extend EvaluationV2 if EvaluationV2 is an object.
// It will use EvaluationV2's methods directly.
object EvaluationV3 {

  // Helper function to flip tables for Gote
  private def flipTable(senteTable: Array[Array[Int]]): Array[Array[Int]] = senteTable.reverse

  // --- Sente Piece-Square Tables (PSTs) ---
  // Values are from Sente's perspective. Row 0 is Sente's 1st rank (opponent's back rank).
  // Row 8 is Sente's 9th rank (Sente's back rank).

  private val FU_PST_SENTE: Array[Array[Int]] = Array(
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0), // Rank 1 (Gote's back rank)
    Array( 5,  5,  5,  5,  5,  5,  5,  5,  5), // Rank 2
    Array(10, 10, 10, 10, 10, 10, 10, 10, 10), // Rank 3 - Promotion zone
    Array( 8,  8,  8,  8,  8,  8,  8,  8,  8), // Rank 4
    Array( 5,  5,  5,  5,  5,  5,  5,  5,  5), // Rank 5
    Array( 2,  2,  2,  2,  2,  2,  2,  2,  2), // Rank 6
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0), // Rank 7 - Starting rank for Sente pawns
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0), // Rank 8
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0)  // Rank 9 (Sente's back rank)
  )

  private val KY_PST_SENTE: Array[Array[Int]] = Array.tabulate(9,9) { (y, x) =>
    val edgeFileBonus = if (x == 0 || x == 8) 5 else 0 // Lances start on edge files
    val forwardBonus = (6 - y) * 2 // Bonus for advancing (y=8 own back rank, y=0 enemy back rank)
    // Max for y=0 (rank 1) is 12. Min for y=8 (rank 9) is -4.
    // Lances are generally better when advanced or ready to advance on an open file.
    edgeFileBonus + forwardBonus
  }

  private val KE_PST_SENTE: Array[Array[Int]] = Array( // Knights are good 2 ranks forward, towards center
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0),
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0),
    Array(10, 15, 15, 15, 15, 15, 15, 15, 10), // Target rank for Sente
    Array( 5, 10, 10, 10, 10, 10, 10, 10,  5),
    Array( 0,  5,  5,  5,  5,  5,  5,  5,  0),
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0),
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0), // Starting rank for Sente Knights (row 7)
    Array( 0,  0,  0,  0,  0,  0,  0,  0,  0),
    Array(-5,-10,-10,-10,-10,-10,-10,-10, -5) // Bad on Sente's own back rank (row 8)
  )

  private val GI_PST_SENTE: Array[Array[Int]] = Array.tabulate(9,9) { (y, x) =>
    val centerBonus = if (x >= 2 && x <= 6 && y >= 2 && y <= 6) 5 else 0 // General central area
    val attackBonus = if (y < 5) (4-y) else 0 // Bonus for being in attacking half
    centerBonus + attackBonus + 5 // Base value 5
  }

  private val KI_PST_SENTE: Array[Array[Int]] = Array.tabulate(9,9) { (y, x) =>
    val centerBonus = if (x >= 2 && x <= 6 && y >= 2 && y <= 6) 6 else 0
    val defenseBonus = if (y > 5) (y-5) else 0 // Bonus for being in defensive half (near king start)
    centerBonus + defenseBonus + 6 // Base value 6
  }

  private val KA_PST_SENTE: Array[Array[Int]] = Array.tabulate(9,9) { (y, x) =>
    val diagBonus = Math.min(Math.min(y, 8-y), Math.min(x, 8-x)) * 2 // Bonus for being away from edges (more diagonal freedom)
    val centerBonus = if ( (x >=2 && x <=6) && (y >=2 && y <=6) ) 5 else 0
    diagBonus + centerBonus + 8 // Base value 8
  }

  private val HI_PST_SENTE: Array[Array[Int]] = Array.tabulate(9,9) { (y,x) =>
    // Bonus for central files/ranks, or 2nd/7th ranks which are often contested
    val lineBonus = (if (x==4) 5 else 0) + (if (y==4) 5 else 0) + // Central file/rank
                    (if (y==1 || y==7) 3 else 0) // Key attacking/defending ranks
    lineBonus + 10 // Base value 10
  }


  private val OU_PST_SENTE: Array[Array[Int]] = Array( // King safety: Simple version, prefers back ranks, not corners
    Array(-50,-40,-30,-20,-20,-20,-30,-40,-50), // Rank 1 (Gote's territory) - very bad
    Array(-30,-20,-10, -5, -5,-10,-20,-30,-30), // Rank 2
    Array(-10,  0,  5,  5,  5,  5,  0,  0,-10),
    Array(-20,-10,  0,  0,  0,  0,-10,-20,-20), // Rank 3
    Array(-10,  0,  5,  5,  5,  5,  0,-10,-10), // Rank 4
    Array(-10,  0,  5,  0,  0,  5,  0,-10,-10), // Rank 5 - center still not ideal for king
    Array(-10,  0,  5,  5,  5,  5,  0,-10,-10), // Rank 6
    Array(-20,-10,  0, 10, 10,  0,-10,-20,-20), // Rank 7 - getting safer
    Array(-30,-20, 10, 15, 15, 10,-20,-30,-30), // Rank 8 - "castle" area
    Array(-40,-30,  5, 20, 20,  5,-30,-40,-40)  // Rank 9 (Sente's back rank) - good spots are e.g. (8,3), (8,4)
  )

  // Promoted Pieces PSTs (can be same as Gold or stronger versions)
  private val TO_PST_SENTE: Array[Array[Int]] = KI_PST_SENTE.map(_.map(_ + 2)) // Slightly better than Gold
  private val NY_PST_SENTE: Array[Array[Int]] = KI_PST_SENTE.map(_.map(_ + 1))
  private val NK_PST_SENTE: Array[Array[Int]] = KI_PST_SENTE.map(_.map(_ + 1))
  private val NG_PST_SENTE: Array[Array[Int]] = KI_PST_SENTE.map(_.map(_ + 2))

  private val UM_PST_SENTE: Array[Array[Int]] = KA_PST_SENTE.map(_.map(_ + 20)) // Significantly stronger
  private val RY_PST_SENTE: Array[Array[Int]] = HI_PST_SENTE.map(_.map(_ + 20)) // Significantly stronger

  // --- Gote Piece-Square Tables (flipped versions of Sente's) ---
  private val FU_PST_GOTE: Array[Array[Int]] = flipTable(FU_PST_SENTE)
  private val KY_PST_GOTE: Array[Array[Int]] = flipTable(KY_PST_SENTE)
  private val KE_PST_GOTE: Array[Array[Int]] = flipTable(KE_PST_SENTE)
  private val GI_PST_GOTE: Array[Array[Int]] = flipTable(GI_PST_SENTE)
  private val KI_PST_GOTE: Array[Array[Int]] = flipTable(KI_PST_SENTE)
  private val KA_PST_GOTE: Array[Array[Int]] = flipTable(KA_PST_SENTE)
  private val HI_PST_GOTE: Array[Array[Int]] = flipTable(HI_PST_SENTE)
  private val OU_PST_GOTE: Array[Array[Int]] = flipTable(OU_PST_SENTE)
  private val TO_PST_GOTE: Array[Array[Int]] = flipTable(TO_PST_SENTE)
  private val NY_PST_GOTE: Array[Array[Int]] = flipTable(NY_PST_SENTE)
  private val NK_PST_GOTE: Array[Array[Int]] = flipTable(NK_PST_SENTE)
  private val NG_PST_GOTE: Array[Array[Int]] = flipTable(NG_PST_SENTE)
  private val UM_PST_GOTE: Array[Array[Int]] = flipTable(UM_PST_SENTE)
  private val RY_PST_GOTE: Array[Array[Int]] = flipTable(RY_PST_SENTE)

  // Simpler PST getter: one for Sente pieces, one for Gote pieces.
  // The evaluation function will then use the one matching the piece's actual owner.
  private def getPstForSentePiece(piece: Piece): Option[Array[Array[Int]]] = piece match {
    case Piece.▲.FU => Some(FU_PST_SENTE); case Piece.▲.KY => Some(KY_PST_SENTE); case Piece.▲.KE => Some(KE_PST_SENTE)
    case Piece.▲.GI => Some(GI_PST_SENTE); case Piece.▲.KI => Some(KI_PST_SENTE); case Piece.▲.KA => Some(KA_PST_SENTE)
    case Piece.▲.HI => Some(HI_PST_SENTE); case Piece.▲.OU => Some(OU_PST_SENTE); case Piece.▲.TO => Some(TO_PST_SENTE)
    case Piece.▲.NY => Some(NY_PST_SENTE); case Piece.▲.NK => Some(NK_PST_SENTE); case Piece.▲.NG => Some(NG_PST_SENTE)
    case Piece.▲.UM => Some(UM_PST_SENTE); case Piece.▲.RY => Some(RY_PST_SENTE)
    case _ => None
  }

  private def getPstForGotePiece(piece: Piece): Option[Array[Array[Int]]] = piece match {
    case Piece.△.FU => Some(FU_PST_GOTE); case Piece.△.KY => Some(KY_PST_GOTE); case Piece.△.KE => Some(KE_PST_GOTE)
    case Piece.△.GI => Some(GI_PST_GOTE); case Piece.△.KI => Some(KI_PST_GOTE); case Piece.△.KA => Some(KA_PST_GOTE)
    case Piece.△.HI => Some(HI_PST_GOTE); case Piece.△.OU => Some(OU_PST_GOTE); case Piece.△.TO => Some(TO_PST_GOTE)
    case Piece.△.NY => Some(NY_PST_GOTE); case Piece.△.NK => Some(NK_PST_GOTE); case Piece.△.NG => Some(NG_PST_GOTE)
    case Piece.△.UM => Some(UM_PST_GOTE); case Piece.△.RY => Some(RY_PST_GOTE)
    case _ => None
  }

  // Removed 'override' as EvaluationV3 no longer extends EvaluationV2 directly.
  def evaluate(board: Board, turn: Turn): Int = {
    val baseScore = EvaluationV2.evaluate(board, turn) // Call EvaluationV2.evaluate directly
    var pstScoreAdjustment = 0

    for (y <- 0 to 8; x <- 0 to 8) {
      val point = Point(y, x)
      val piece = board.squares.get(point)

      if (piece != Piece.❏) {
        val maybePst = if (Piece.▲(piece)) { // Is it a Sente piece type?
          getPstForSentePiece(piece)
        } else { // Must be a Gote piece type
          getPstForGotePiece(piece)
        }

        maybePst match {
          case Some(pst) =>
            val piecePstValue = pst(point.y)(point.x)
            if (Piece.▲△(piece, turn)) { // Piece belongs to the current player 'turn'
              pstScoreAdjustment += piecePstValue
            } else { // Piece belongs to the opponent
              pstScoreAdjustment -= piecePstValue
            }
          case None => // Should not happen if all piece types are covered
        }
      }
    }
    baseScore + pstScoreAdjustment
  }
}
