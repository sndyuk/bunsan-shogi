package jp.sndyuk.shogi.kifu

// Removed imports for jp.sndyuk.shogi.core.Piece, Position, Move, Transition as TempCore defines its own
// Assuming jp.sndyuk.shogi.core.Board.Board and jp.sndyuk.shogi.core.Player.Player might be different
// and are kept for now if they are used by external callers of exportToString.
// However, the function signature of exportToString uses TempCore types due to `import TempCore._`.
// For now, let's assume all types passed to exportToString are from TempCore or compatible.
// If Board and Player from jp.sndyuk.shogi.core are truly needed, aliasing or qualification will be necessary.

// import jp.sndyuk.shogi.core.Board.Board // Potentially unused or conflicting
// import jp.sndyuk.shogi.core.Player.Player // Potentially unused or conflicting

// TempCore object has been moved to its own file: TempCore.scala
import jp.sndyuk.shogi.kifu.TempCore._


object CSAExporter {

  // import TempCore._ // This import is now at the top level of the file or covered by package access.
                      // Re-evaluate if needed, but direct package access or the top-level import should suffice.

  def exportToString(
      board: Board, // Current or initial board state
      history: Seq[Transition], // Sequence of moves made
      currentTurnPlayer: Turn, // Player whose turn it is (if game is ongoing)
      result: Option[String] // Game result e.g., "%TORYO"
  ): String = {
    val sb = new StringBuilder

    // Version
    sb.append("V2.2\n")

    // Player names (optional, using generic SENTE/GOTE for now)
    // N+SENTE_PLAYER_NAME
    // N-GOTE_PLAYER_NAME
    // For now, let's assume no specific player names are passed
    // sb.append("N+Player1\n")
    // sb.append("N-Player2\n")

    // Initial board setup
    // If standard, this section is often omitted or can be PI
    if (board.isStandardInitialPosition) {
      // For standard hirate (even) game
      // PI could be used, or P1 through P9 for individual pieces.
      // Or simply omit if it's the default Hirate.
      // Let's assume Hirate for now and omit detailed PI for simplicity.
      // If it were not standard, it would be like:
      // P1-KY-KE-GI-KI-OU-KI-GI-KE-KY
      // P2 * -HI *  *  *  *  * -KA *
      // P3-FU-FU-FU-FU-FU-FU-FU-FU-FU
      // ... etc.
      // And P+00FU for pieces in hand
    } else {
      // Non-standard initial position - this requires detailed board state.
      // Example: P1-KY-KE-GI-KI-OU-KI-GI-KE-KY
      //          P2 * -HI *  *  *  *  * -KA *
      //          P3-FU-FU-FU-FU-FU-FU-FU-FU-FU
      // (This part needs full board details to implement correctly)
      sb.append("PI\n") // Placeholder for standard, a full impl would print board state
      // For a custom setup:
      // for (y <- 1 to 9) {
      //   sb.append(s"P$y")
      //   for (x <- 9 to 1 by -1) { // CSA board x-coords are right to left (9 to 1)
      //     board.pieceAt(Position(x,y)) match {
      //       case Some((piece, SENTE)) => sb.append(s"+${piece.toCSA}")
      //       case Some((piece, GOTE)) => sb.append(s"-${piece.toCSA}")
      //       case None => sb.append(" *  * ")
      //     }
      //   }
      //   sb.append("\n")
      // }
      // // Pieces in hand for Sente (P+) and Gote (P-)
      // // e.g. P+00FU00KA for Sente having a FU and KA in hand
    }


    // First player (usually SENTE, which is '+')
    // The spec says this indicates the player to move if the board is set up mid-game.
    // If starting from beginning, it's always SENTE.
    // For simplicity, assuming new game start or SENTE's turn if mid-game.
    sb.append("+\n") // Indicates Sente moves first from this position

    // Moves
    history.foreach { transition =>
      sb.append(transition.move.toCSA).append("\n")
      transition.comment.foreach { c =>
        sb.append("'").append(c.replace("\n", "\n'")).append("\n") // Comments start with '
      }
    }

    // Game result
    result.foreach { res =>
      sb.append(res).append("\n")
    }

    sb.toString()
  }

  // Helper for testing
  def main(args: Array[String]): Unit = {
    // Example Usage:
    val initialBoard = Board(Map.empty, SENTE) // Simplified board

    val gameHistory = Seq(
      Transition(Move(SENTE, Some(Position(7,7)), Position(7,6), FU)),
      Transition(Move(GOTE, Some(Position(3,3)), Position(3,4), FU)),
      Transition(Move(SENTE, Some(Position(8,8)), Position(2,2), KA, promote = true), comment = Some("A great move!")),
      Transition(Move(GOTE, None, Position(5,5), KI, isDrop = true)) // Drop
    )

    // Assuming Sente made the last move, it's Gote's turn if game is ongoing
    // Or, if game ended, currentTurnPlayer might be the one who resigned or was checkmated.
    val turn = GOTE

    println("--- CSA Output ---")
    val csaOutput = exportToString(initialBoard, gameHistory, turn, Some("%TORYO"))
    println(csaOutput)

    // Example for non-standard start (very simplified representation)
    // This part of board representation needs to be robust based on actual Board structure
    // val customBoard = Board(Map(Position(5,5) -> (OU, SENTE)), SENTE)
    // val csaCustom = exportToString(customBoard, Seq(), SENTE, None)
    // println("\n--- CSA Custom Start (Conceptual) ---")
    // println(csaCustom)

  }
}
