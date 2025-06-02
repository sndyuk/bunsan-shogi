package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._
// No direct Utils import needed if Rule is used.

object EvaluationV2 {

  private val MOBILITY_BONUS_PER_MOVE = 2
  private val KING_FEW_ESCAPES_PENALTY = -50 // Penalty if king has few escape moves
  private val MIN_KING_ESCAPES_THRESHOLD = 3 // Threshold for king escape penalty
  private val PAWN_SHIELD_BONUS_PER_PAWN = 30 // Bonus for each pawn protecting the king

  private val PROMOTION_POTENTIAL_MINOR = 50 // Bonus for FU, KY, KE, GI in enemy zone
  private val PROMOTION_POTENTIAL_MAJOR = 100 // Bonus for KA, HI in enemy zone

  // getPieceValueForEval is not needed here as EvaluationV1.evaluate is used for material score
  // and other heuristics add fixed bonuses.

  private def calculatePieceMobilityScore(board: Board, playerTurn: Turn): Int = {
    var mobilityScore = 0
    for (y <- 0 to 8; x <- 0 to 8) {
      val point = Point(y, x)
      val piece = board.squares.get(point) // Assuming board.squares.get returns Piece
      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn)) {
        val generalizedPiece = Piece.generalize(piece)
        if (generalizedPiece != Piece.◯.FU) { // Excludes unpromoted Pawns (FU) and promoted Pawns (TO)
          // Rule.generateMovablePoints(board: Board, oldPos: Point, piece: Piece, turn: Turn, includePromoted: Boolean)
          val pieceMoves = Rule.generateMovablePoints(board, point, piece, playerTurn, false).toList
          mobilityScore += pieceMoves.size * MOBILITY_BONUS_PER_MOVE
        }
      }
    }
    mobilityScore
  }

  private def calculateKingSafetyScore(board: Board, playerTurn: Turn): Int = {
    var safetyScore = 0
    val playerKingPiece = Piece.convert(Piece.◯.OU, playerTurn) // Get specific King piece (▲.OU or △.OU)

    board.squares.find(playerKingPiece) match {
      case Some(kingPos) =>
        // 1. Penalty for few escape moves
        val kingMoves = Rule.generateMovablePoints(board, kingPos, playerKingPiece, playerTurn, false).toList
        if (kingMoves.size < MIN_KING_ESCAPES_THRESHOLD) {
          safetyScore += KING_FEW_ESCAPES_PENALTY
        }

        // 2. Bonus for simple pawn shield (pawns on rank in front, same/adjacent files)
        // Note: y coordinates: PlayerA (Sente) ranks 0-8 (top to bottom). PlayerB (Gote) ranks 0-8 (top to bottom).
        // Sente's king typically starts at y=8, Gote's at y=0.
        // "In front" for Sente means y-1. For Gote means y+1.
        val (pawnRankY, _) = if (playerTurn == PlayerA) (kingPos.y - 1, -1) else (kingPos.y + 1, 1)

        if (pawnRankY >= 0 && pawnRankY <= 8) { // Check if pawnRankY is on board
          for (dx <- -1 to 1) { // Check king's file and adjacent files
            val pawnCheckX = kingPos.x + dx
            if (pawnCheckX >= 0 && pawnCheckX <= 8) { // Check if pawnCheckX is on board
              val pieceOnShieldSquare = board.squares.get(Point(pawnRankY, pawnCheckX))
              // Check if it's a pawn of the current player
              if (Piece.generalize(pieceOnShieldSquare) == Piece.◯.FU && Piece.▲△(pieceOnShieldSquare, playerTurn)) {
                safetyScore += PAWN_SHIELD_BONUS_PER_PAWN
              }
            }
          }
        }
      case None => // King not on board
        safetyScore -= 10000 // Should ideally not happen if called on valid game states
    }
    safetyScore
  }

  private def calculatePromotionPotentialScore(board: Board, playerTurn: Turn): Int = {
    var promotionScore = 0
    // Promotion zone for Sente (PlayerA) is ranks 1-3 (y = 0, 1, 2)
    // Promotion zone for Gote (PlayerB) is ranks 7-9 (y = 6, 7, 8)
    val isInPromotionZone = if (playerTurn == PlayerA) (y: Int) => y <= 2 else (y: Int) => y >= 6

    for (y <- 0 to 8; x <- 0 to 8) {
      val point = Point(y, x)
      val piece = board.squares.get(point)

      // Check if it's player's piece, on board, and not already promoted
      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn) && !Piece.isPromoted(piece)) {
        if (isInPromotionZone(point.y)) { // Check if the piece is in its promotion zone
          Piece.generalize(piece) match {
            case Piece.◯.FU | Piece.◯.KY | Piece.◯.KE | Piece.◯.GI =>
              promotionScore += PROMOTION_POTENTIAL_MINOR
            case Piece.◯.KA | Piece.◯.HI =>
              promotionScore += PROMOTION_POTENTIAL_MAJOR
            case _ => // OU (King) and KI (Gold) cannot promote.
          }
        }
      }
    }
    promotionScore
  }

  def evaluate(board: Board, turn: Turn): Int = {
    // Material score is already relative (myMaterial - oppMaterial)
    val materialScore = EvaluationV1.evaluate(board, turn)

    // Calculate positional scores for the player whose turn it is
    val myMobility = calculatePieceMobilityScore(board, turn)
    val myKingSafety = calculateKingSafetyScore(board, turn)
    val myPromotionPotential = calculatePromotionPotentialScore(board, turn)

    // Calculate positional scores for the opponent
    val opponentTurn = turn.change
    val opponentMobility = calculatePieceMobilityScore(board, opponentTurn)
    val opponentKingSafety = calculateKingSafetyScore(board, opponentTurn)
    val opponentPromotionPotential = calculatePromotionPotentialScore(board, opponentTurn)

    // Combine: Material + (My Positional Bonuses - Opponent's Positional Bonuses)
    val positionalScoreDifference =
      (myMobility - opponentMobility) +
      (myKingSafety - opponentKingSafety) +
      (myPromotionPotential - opponentPromotionPotential)
      // TODO: Add other heuristics like Center Control, Piece Activity Detail etc.

    materialScore + positionalScoreDifference
  }
}
