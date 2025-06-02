package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._
// No direct Utils import needed if Rule is used.

object EvaluationV2 {

  private val MOBILITY_BONUS_PER_MOVE = 2
  private val KING_FEW_ESCAPES_PENALTY = -50 // Penalty if king has few escape moves
  private val MIN_KING_ESCAPES_THRESHOLD = 3 // Threshold for king escape penalty
  private val PAWN_SHIELD_BONUS_PER_PAWN = 30 // Bonus for each pawn protecting the king
  private val KING_ADJACENT_ATTACK_PENALTY = -25 // Penalty for each enemy piece attacking a square adjacent to the king
  private val GOOD_CASTLE_BONUS = 40 // Bonus for having at least two generals (Gold or Silver) near the king
  private val GENERAL_NEAR_KING_DISTANCE = 2 // Manhattan distance to define "near the king" for generals

  // Piece-Square Tables (Sente perspective)
  // format: off
  private val ROOK_PST_SENTE: Array[Array[Int]] = Array(
    Array( 1,  2,  2,  3,  3,  3,  2,  2,  1), // Rank 1 (y=0)
    Array( 2,  5,  5,  5,  5,  5,  5,  5,  2), // Rank 2
    Array( 2,  5,  7,  7,  7,  7,  7,  5,  2), // Rank 3
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3), // Rank 4
    Array( 3,  5,  7,  8,  9,  8,  7,  5,  3), // Rank 5
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3), // Rank 6
    Array( 2,  5,  7,  7,  7,  7,  7,  5,  2), // Rank 7
    Array( 2,  5,  5,  5,  5,  5,  5,  5,  2), // Rank 8
    Array( 1,  2,  2,  3,  3,  3,  2,  2,  1)  // Rank 9 (y=8)
  )

  private val BISHOP_PST_SENTE: Array[Array[Int]] = Array(
    Array( 3,  3,  3,  3,  3,  3,  3,  3,  3),
    Array( 3,  5,  5,  5,  5,  5,  5,  5,  3),
    Array( 3,  5,  7,  7,  7,  7,  7,  5,  3),
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3),
    Array( 3,  5,  7,  8,  9,  8,  7,  5,  3),
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3),
    Array( 3,  5,  7,  7,  7,  7,  7,  5,  3),
    Array( 3,  5,  5,  5,  5,  5,  5,  5,  3),
    Array( 3,  3,  3,  3,  3,  3,  3,  3,  3)
  )

  private val KNIGHT_PST_SENTE: Array[Array[Int]] = Array(
    Array(1, 2, 3, 3, 3, 3, 3, 2, 1), // Rank 1 (y=0)
    Array(2, 3, 5, 5, 5, 5, 5, 3, 2), // Rank 2
    Array(3, 5, 7, 7, 7, 7, 7, 5, 3), // Rank 3 (good jump spots)
    Array(3, 5, 7, 6, 6, 6, 7, 5, 3), // Rank 4
    Array(2, 4, 6, 4, 4, 4, 6, 4, 2), // Rank 5
    Array(1, 3, 4, 2, 2, 2, 4, 3, 1), // Rank 6
    Array(1, 2, 3, 1, 1, 1, 3, 2, 1), // Rank 7 (getting crowded)
    Array(0, 1, 1, 0, 0, 0, 1, 1, 0), // Rank 8
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0)  // Rank 9 (y=8, own back rank)
  )
  // format: on

  private val PROMOTION_POTENTIAL_MINOR = 50 // Bonus for FU, KY, KE, GI in enemy zone
  private val PROMOTION_POTENTIAL_MAJOR = 100 // Bonus for KA, HI in enemy zone
  private val ATTACKING_PIECE_BONUS = 5 // General bonus for attacking any opponent piece
  private val ATTACKING_MORE_VALUABLE_PIECE_BONUS = 15 // Additional bonus if the attacked piece is more valuable

  // Center Control Heuristic Constants
  private val CENTER_FILES: Set[Int] = Set(3, 4, 5) // Files 6, 5, 4 (0-indexed: 0=9th file, 8=1st file)
  private val CENTER_RANKS: Set[Int] = Set(3, 4, 5) // Ranks 4, 5, 6 (0-indexed: 0=rank 1, 8=rank 9)
  private val CENTER_SQUARE_BONUS = 10 // Bonus for each piece occupying a center square
  // private val CENTER_ATTACK_BONUS = 5 // Optional: For pieces attacking center (not implemented in this version)

  // getPieceValueForEval is not needed here as EvaluationV1.evaluate is used for material score
  // and other heuristics add fixed bonuses.

  private def getNominalPieceValue(p: Piece): Int = {
    // Returns a nominal material value for a piece type, ignoring player, but respecting promotion.
    // We check against Sente (▲) piece types as they are the canonical base values in Piece.scala
    // and include the promotion bit if the piece is promoted.
    val pieceTypeOnly = p & Piece.bitsPiece // Strips player and general bits, keeps type and promotion status
    pieceTypeOnly match {
      case Piece.▲.FU => 10
      case Piece.▲.KY => 30
      case Piece.▲.KE => 30
      case Piece.▲.GI => 40
      case Piece.▲.KI => 50 // Gold General
      case Piece.▲.KA => 80 // Bishop
      case Piece.▲.HI => 100 // Rook
      case Piece.▲.OU => 10000 // King

      // Promoted pieces (values include promotion bit)
      case Piece.▲.TO => 50 // Promoted Pawn (Tokin)
      case Piece.▲.NY => 50 // Promoted Lance (Nariko)
      case Piece.▲.NK => 50 // Promoted Knight (Narikei)
      case Piece.▲.NG => 50 // Promoted Silver (Narigin)
      case Piece.▲.UM => 120 // Promoted Bishop (Uma)
      case Piece.▲.RY => 140 // Promoted Rook (Ryu)

      case Piece.❏ => 0 // Empty square
      case _ => 0 // Should ideally not be reached for other valid piece types if they are not covered above
                  // This might happen for △ pieces if not perfectly masked, but values are same.
                  // Or for generalized pieces if they somehow reach here (they shouldn't for nominal value calc).
    }
  }

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

        // 3. Penalty for enemy pieces attacking squares adjacent to the king
        val opponentTurn = playerTurn.change
        var attackingOpponentPiecePositions: Set[Point] = Set.empty
        for (y <- 0 to 8; x <- 0 to 8) {
          val piecePos = Point(y, x)
          val piece = board.squares.get(piecePos)
          if (piece != Piece.❏ && Piece.▲△(piece, opponentTurn)) { // Found an opponent's piece
            val legalMoves = Rule.generateMovablePoints(board, piecePos, piece, opponentTurn, false)
            // Use a flag to ensure we only count the piece once, even if it attacks multiple adjacent squares
            var pieceAttacksAdjacentSquare = false
            for (moveTuple <- legalMoves if !pieceAttacksAdjacentSquare) { // move is (Point, Boolean)
              val movePoint = moveTuple._1
              val manhattanDistance = Math.abs(movePoint.y - kingPos.y) + Math.abs(movePoint.x - kingPos.x)
              if (manhattanDistance <= 1) { // Square is adjacent to the king
                pieceAttacksAdjacentSquare = true
              }
            }
            if (pieceAttacksAdjacentSquare) {
              attackingOpponentPiecePositions += piecePos // Add the position of the attacking piece
            }
          }
        }
        safetyScore += attackingOpponentPiecePositions.size * KING_ADJACENT_ATTACK_PENALTY

        // 4. Bonus for good castle form (at least two friendly generals near the king)
        var friendlyGeneralCount = 0
        for (y <- 0 to 8; x <- 0 to 8) {
          val piecePos = Point(y, x)
          val piece = board.squares.get(piecePos)
          if (Piece.▲△(piece, playerTurn)) { // Check if it's a friendly piece
            Piece.generalize(piece) match {
              case Piece.◯.KI | Piece.◯.GI => // It's a Gold or Silver general
                val manhattanDistance = Math.abs(piecePos.y - kingPos.y) + Math.abs(piecePos.x - kingPos.x)
                if (manhattanDistance <= GENERAL_NEAR_KING_DISTANCE) {
                  friendlyGeneralCount += 1
                }
              case _ => // Not a general
            }
          }
        }
        if (friendlyGeneralCount >= 2) {
          safetyScore += GOOD_CASTLE_BONUS
        }

      case None => // King not on board
        safetyScore -= 10000 // Should ideally not happen if called on valid game states
    }
    safetyScore
  }

  private def calculateCenterControlScore(board: Board, playerTurn: Turn): Int = {
    var centerScore = 0
    for (y <- 0 to 8; x <- 0 to 8) {
      val point = Point(y, x)
      val piece = board.squares.get(point)

      if (piece != Piece.❏) { // If square is not empty
        if (CENTER_FILES.contains(point.x) && CENTER_RANKS.contains(point.y)) {
          if (Piece.▲△(piece, playerTurn)) {
            centerScore += CENTER_SQUARE_BONUS
          }
          // No penalty for opponent occupying, as (myScore - opponentScore) handles the relativity
        }
      }
    }
    // Attack bonus omitted for now as per plan
    centerScore
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

  private def calculatePieceSquareTableScore(board: Board, playerTurn: Turn): Int = {
    var pstScore = 0
    for (currentY <- 0 to 8; currentX <- 0 to 8) {
      val point = Point(currentY, currentX)
      val piece = board.squares.get(point)

      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn)) {
        val generalizedPiece = Piece.generalize(piece)

        generalizedPiece match {
          case Piece.◯.HI => // Rook
            val y = if (playerTurn == PlayerA) currentY else 8 - currentY
            val x = currentX // Rook PSTs often assume file symmetry from one side's view
            pstScore += ROOK_PST_SENTE(y)(x)
          case Piece.◯.KA => // Bishop
            val y = if (playerTurn == PlayerA) currentY else 8 - currentY
            val x = if (playerTurn == PlayerA) currentX else 8 - currentX
            pstScore += BISHOP_PST_SENTE(y)(x)
          case Piece.◯.KE => // Knight
            val y = if (playerTurn == PlayerA) currentY else 8 - currentY
            val x = if (playerTurn == PlayerA) currentX else 8 - currentX
            pstScore += KNIGHT_PST_SENTE(y)(x)
          case _ => // Other pieces, or no PST defined for them
        }
      }
    }
    pstScore
  }

  private def calculateAttackingPiecesScore(board: Board, playerTurn: Turn): Int = {
    var attackingScore = 0
    val opponentTurn = playerTurn.change

    for (y <- 0 to 8; x <- 0 to 8) {
      val attackerPos = Point(y, x)
      val attackerPiece = board.squares.get(attackerPos)

      if (attackerPiece != Piece.❏ && Piece.▲△(attackerPiece, playerTurn)) {
        // Rule.generateMovablePoints(board: Board, oldPos: Point, piece: Piece, turn: Turn, includePromoted: Boolean)
        // The 'includePromoted' flag is for moves that *result* in promotion, not if the piece *is* promoted.
        // We want all actual legal moves for the current piece.
        val legalMoves = Rule.generateMovablePoints(board, attackerPos, attackerPiece, playerTurn, true) // true to consider promotion options as separate moves if they attack

        for (moveTuple <- legalMoves) { // moveTuple is (Point, Boolean)
          // In Shogi, a move implies capturing the piece at the destination, if any.
          // So, move._1 is the square of the potentially attacked piece.
          val attackedPos = moveTuple._1 // Extract Point from the tuple
          val attackedPiece = board.squares.get(attackedPos)

          if (attackedPiece != Piece.❏ && Piece.▲△(attackedPiece, opponentTurn)) {
            attackingScore += ATTACKING_PIECE_BONUS
            if (getNominalPieceValue(attackedPiece) > getNominalPieceValue(attackerPiece)) {
              attackingScore += ATTACKING_MORE_VALUABLE_PIECE_BONUS
            }
          }
        }
      }
    }
    attackingScore
  }

  def evaluate(board: Board, turn: Turn): Int = {
    // Material score is already relative (myMaterial - oppMaterial)
    val materialScore = EvaluationV1.evaluate(board, turn)

    // Calculate positional scores for the player whose turn it is
    val myMobility = calculatePieceMobilityScore(board, turn)
    val myKingSafety = calculateKingSafetyScore(board, turn)
    val myPromotionPotential = calculatePromotionPotentialScore(board, turn)
    val myCenterControl = calculateCenterControlScore(board, turn)
    val myPstScore = calculatePieceSquareTableScore(board, turn)
    val myAttackingScore = calculateAttackingPiecesScore(board, turn)

    // Calculate positional scores for the opponent
    val opponentTurn = turn.change
    val opponentMobility = calculatePieceMobilityScore(board, opponentTurn)
    val opponentKingSafety = calculateKingSafetyScore(board, opponentTurn)
    val opponentPromotionPotential = calculatePromotionPotentialScore(board, opponentTurn)
    val opponentCenterControl = calculateCenterControlScore(board, opponentTurn)
    val opponentPstScore = calculatePieceSquareTableScore(board, opponentTurn)
    val opponentAttackingScore = calculateAttackingPiecesScore(board, opponentTurn)

    // Combine: Material + (My Positional Bonuses - Opponent's Positional Bonuses)
    val positionalScoreDifference =
      (myMobility - opponentMobility) +
      (myKingSafety - opponentKingSafety) +
      (myPromotionPotential - opponentPromotionPotential) +
      (myCenterControl - opponentCenterControl) +
      (myPstScore - opponentPstScore) +
      (myAttackingScore - opponentAttackingScore)
      // TODO: Add other heuristics like Piece Activity Detail etc.

    materialScore + positionalScoreDifference
  }
}
