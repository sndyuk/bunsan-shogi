package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.{Board, Piece, Turn, Point, PlayerA, PlayerB}
// PlayerA and PlayerB are instances of Turn, not separate types to import.
// Turn itself is needed for type annotations. Point is used. Piece for constants. Board for its structure.

object EvaluationV1 {

  // Piece values are Ints (aliases for Byte actually)
  private val pieceValues: Map[Int, Int] = Map(
    Piece.▲.FU -> 100, Piece.△.FU -> 100,
    Piece.▲.KY -> 300, Piece.△.KY -> 300,
    Piece.▲.KE -> 320, Piece.△.KE -> 320,
    Piece.▲.GI -> 450, Piece.△.GI -> 450,
    Piece.▲.KI -> 500, Piece.△.KI -> 500,
    Piece.▲.KA -> 800, Piece.△.KA -> 800,
    Piece.▲.HI -> 900, Piece.△.HI -> 900,
    Piece.▲.TO -> 550, Piece.△.TO -> 550, // Promoted FU (Gold equivalent + bit more)
    Piece.▲.NY -> 530, Piece.△.NY -> 530, // Promoted KY (Gold equivalent + bit more)
    Piece.▲.NK -> 530, Piece.△.NK -> 530, // Promoted KE (Gold equivalent + bit more)
    Piece.▲.NG -> 550, Piece.△.NG -> 550, // Promoted GI (Gold equivalent + bit more)
    Piece.▲.UM -> 1200, Piece.△.UM -> 1200, // Promoted KA
    Piece.▲.RY -> 1300, Piece.△.RY -> 1300, // Promoted HI
    Piece.▲.OU -> 0, Piece.△.OU -> 0     // King value is 0 for material evaluation
  ).withDefaultValue(0) // Return 0 for Piece.❏ (empty) or any other piece type not listed

  private def getPieceValue(piece: Piece): Int = {
    // The map uses specific Sente/Gote pieces.
    // Piece type is Int, which is what the map expects.
    pieceValues(piece)
  }

  def evaluate(board: Board, turn: Turn): Int = {
    println(s"EVAL DEBUG: Evaluating for turn: ${if (turn == PlayerA) "PlayerA (Sente)" else "PlayerB (Gote)"}")
    // Detailed board state printing:
    println(s"EVAL DEBUG: Board state (perspective of $turn):")
    val sb = new StringBuilder()
    for (y <- 0 to 8) {
      for (x <- 0 to 8) {
        val p = board.squares.get(Point(y,x))
        sb.append(f"${Piece.name(p)}%-3s") // Use %-3s for alignment with Japanese characters
      }
      sb.append(s" | Rank ${y+1}")
      sb.append("\n")
    }
    println(sb.toString())

    val playerAHand = Piece.◯.all.filter(_ != Piece.◯.OU).map(gP => s"${Piece.name(gP)}x${board.capturedPieces.count(PlayerA, gP)}").mkString(" ")
    val playerBHand = Piece.◯.all.filter(_ != Piece.◯.OU).map(gP => s"${Piece.name(gP)}x${board.capturedPieces.count(PlayerB, gP)}").mkString(" ")
    println(s"EVAL DEBUG: Sente (PlayerA) hand: $playerAHand")
    println(s"EVAL DEBUG: Gote (PlayerB) hand: $playerBHand")

    var myScore = 0
    var opponentScore = 0

    // 1. Pieces on board
    for (y <- 0 to 8; x <- 0 to 8) {
      val point = Point(y, x)
      val piece = board.squares.get(point) // board.squares.get returns Piece (Int)
      if (piece != Piece.❏) { // If square is not empty
        if (Piece.▲△(piece, turn)) { // Check if piece belongs to current player 'turn'
          myScore += getPieceValue(piece)
        } else { // Piece belongs to opponent
          opponentScore += getPieceValue(piece)
        }
      }
    }

    // 2. Pieces in hand
    // Piece.◯.all provides generalized pieces (e.g., Piece.◯.FU)
    for (generalizedPiece <- Piece.◯.all) {
      if (generalizedPiece != Piece.◯.OU) { // Kings cannot be in hand
        // Current player's hand (player for whom 'turn' is)
        val specificPieceMyHand = Piece.convert(generalizedPiece, turn) // e.g. if turn is Sente, this is Sente piece
        val countMyHand = board.capturedPieces.count(turn, generalizedPiece)
        myScore += countMyHand * getPieceValue(specificPieceMyHand)

        // Opponent's hand
        val opponentTurn = turn.change
        val specificPieceOpponentHand = Piece.convert(generalizedPiece, opponentTurn)
        val countOpponentHand = board.capturedPieces.count(opponentTurn, generalizedPiece)
        opponentScore += countOpponentHand * getPieceValue(specificPieceOpponentHand)
      }
    }

    val finalScore = myScore - opponentScore
    println(s"EVAL DEBUG: myScore (for $turn)=$myScore, opponentScore (for ${turn.change})=$opponentScore, finalScoreForTurn_${turn}=$finalScore")
    finalScore
  }
}
