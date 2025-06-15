package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._

object EvaluationV3 {

  private val MOBILITY_BONUS_PER_MOVE = 2
  private val KING_FEW_ESCAPES_PENALTY = -50
  private val MIN_KING_ESCAPES_THRESHOLD = 3
  private val PAWN_SHIELD_BONUS_PER_PAWN = 30
  private val KING_ADJACENT_ATTACK_PENALTY = -25
  private val GOOD_CASTLE_BONUS = 40
  private val GENERAL_NEAR_KING_DISTANCE = 2
  private val MINO_CASTLE_BONUS = 50
  private val LINE_OF_SIGHT_THREAT_PENALTY_MAJOR = -60
  private val LINE_OF_SIGHT_THREAT_PENALTY_MINOR = -40
  private val CONNECTED_ROOKS_BONUS = 30
  private val DEFENDING_PIECE_BONUS = 5
  private val DEFENDING_MORE_VALUABLE_PIECE_BONUS = 10
  private val VALUABLE_PIECE_DIFFERENCE_THRESHOLD = 20

  // Piece-Square Tables (Sente perspective)
  // format: off
  private val ROOK_PST_SENTE: Array[Array[Int]] = Array(
    Array( 1,  2,  2,  3,  3,  3,  2,  2,  1),
    Array( 2,  5,  5,  5,  5,  5,  5,  5,  2),
    Array( 2,  5,  7,  7,  7,  7,  7,  5,  2),
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3),
    Array( 3,  5,  7,  8,  9,  8,  7,  5,  3),
    Array( 3,  5,  7,  8,  8,  8,  7,  5,  3),
    Array( 2,  5,  7,  7,  7,  7,  7,  5,  2),
    Array( 2,  5,  5,  5,  5,  5,  5,  5,  2),
    Array( 1,  2,  2,  3,  3,  3,  2,  2,  1)
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
    Array(1, 2, 3, 3, 3, 3, 3, 2, 1),
    Array(2, 3, 5, 5, 5, 5, 5, 3, 2),
    Array(3, 5, 7, 7, 7, 7, 7, 5, 3),
    Array(3, 5, 7, 6, 6, 6, 7, 5, 3),
    Array(2, 4, 6, 4, 4, 4, 6, 4, 2),
    Array(1, 3, 4, 2, 2, 2, 4, 3, 1),
    Array(1, 2, 3, 1, 1, 1, 3, 2, 1),
    Array(0, 1, 1, 0, 0, 0, 1, 1, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  private val GOLD_PST_SENTE: Array[Array[Int]] = Array(
    Array(2, 3, 3, 4, 4, 4, 3, 3, 2),
    Array(2, 4, 4, 5, 5, 5, 4, 4, 2),
    Array(3, 4, 5, 6, 6, 6, 5, 4, 3),
    Array(3, 5, 6, 7, 7, 7, 6, 5, 3),
    Array(4, 5, 6, 7, 8, 7, 6, 5, 4),
    Array(4, 5, 6, 7, 7, 7, 6, 5, 4),
    Array(3, 4, 5, 6, 6, 6, 5, 4, 3),
    Array(2, 3, 4, 5, 5, 5, 4, 3, 2),
    Array(1, 2, 3, 4, 4, 4, 3, 2, 1)
  )

  private val SILVER_PST_SENTE: Array[Array[Int]] = Array(
    Array(2, 3, 3, 4, 4, 4, 3, 3, 2),
    Array(2, 4, 4, 5, 5, 5, 4, 4, 2),
    Array(3, 4, 5, 6, 6, 6, 5, 4, 3),
    Array(3, 5, 6, 7, 7, 7, 6, 5, 3),
    Array(4, 5, 6, 7, 8, 7, 6, 5, 4),
    Array(3, 4, 5, 6, 6, 6, 5, 4, 3),
    Array(2, 3, 4, 5, 5, 5, 4, 3, 2),
    Array(1, 2, 3, 4, 4, 4, 3, 2, 1),
    Array(1, 1, 2, 3, 3, 3, 2, 1, 1)
  )

  private val LANCE_PST_SENTE: Array[Array[Int]] = Array(
    Array(5, 5, 5, 5, 5, 5, 5, 5, 5),
    Array(4, 4, 4, 4, 4, 4, 4, 4, 4),
    Array(3, 3, 3, 3, 3, 3, 3, 3, 3),
    Array(2, 2, 2, 2, 2, 2, 2, 2, 2),
    Array(1, 1, 1, 1, 1, 1, 1, 1, 1),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  private val PAWN_PST_SENTE: Array[Array[Int]] = Array(
    Array(9, 9, 9, 9, 9, 9, 9, 9, 9),
    Array(7, 7, 7, 7, 7, 7, 7, 7, 7),
    Array(5, 5, 5, 6, 6, 6, 5, 5, 5),
    Array(4, 4, 4, 5, 5, 5, 4, 4, 4),
    Array(3, 3, 3, 4, 4, 4, 3, 3, 3),
    Array(2, 2, 2, 3, 3, 3, 2, 2, 2),
    Array(1, 1, 1, 2, 2, 2, 1, 1, 1),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  // format: on

  private val PROMOTION_POTENTIAL_MINOR = 50
  private val PROMOTION_POTENTIAL_MAJOR = 100
  private val ATTACKING_PIECE_BONUS = 5
  private val ATTACKING_MORE_VALUABLE_PIECE_BONUS = 15

  private val CENTER_FILES: Set[Int] = Set(3, 4, 5)
  private val CENTER_RANKS: Set[Int] = Set(3, 4, 5)
  private val CENTER_SQUARE_BONUS = 10

  private def getNominalPieceValue(p: Piece): Int = {
    val pieceTypeOnly = p & Piece.bitsPiece
    pieceTypeOnly match {
      case Piece.▲.FU => 10
      case Piece.▲.KY => 30
      case Piece.▲.KE => 30
      case Piece.▲.GI => 40
      case Piece.▲.KI => 50
      case Piece.▲.KA => 80
      case Piece.▲.HI => 100
      case Piece.▲.OU => 10000
      case Piece.▲.TO => 50
      case Piece.▲.NY => 50
      case Piece.▲.NK => 50
      case Piece.▲.NG => 50
      case Piece.▲.UM => 120
      case Piece.▲.RY => 140
      case Piece.❏ => 0
      case _ => 0
    }
  }

  private def calculatePieceMobilityScore(board: Board, playerTurn: Turn): Int = {
    var mobilityScore = 0
    for (y <- 0 to 8; x <- 0 to 8) {
      val p = Point(y, x)
      val piece = board.squares.get(p)
      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn)) {
        val gen = Piece.generalize(piece)
        if (gen != Piece.◯.FU) {
          val moves = Rule.generateMovablePoints(board, p, piece, playerTurn, false).toList
          mobilityScore += moves.size * MOBILITY_BONUS_PER_MOVE
        }
      }
    }
    mobilityScore
  }

  private def calculateKingSafetyScore(board: Board, playerTurn: Turn): Int = {
    var safetyScore = 0
    val playerKingPiece = Piece.convert(Piece.◯.OU, playerTurn)

    board.squares.find(playerKingPiece) match {
      case Some(kingPos) =>
        val kingMoves = Rule.generateMovablePoints(board, kingPos, playerKingPiece, playerTurn, false).toList
        if (kingMoves.size < MIN_KING_ESCAPES_THRESHOLD)
          safetyScore += KING_FEW_ESCAPES_PENALTY

        val pawnRankY = if (playerTurn == PlayerA) kingPos.y - 1 else kingPos.y + 1
        if (pawnRankY >= 0 && pawnRankY <= 8) {
          for (dx <- -1 to 1) {
            val px = kingPos.x + dx
            if (px >= 0 && px <= 8) {
              val pieceOn = board.squares.get(Point(pawnRankY, px))
              if (Piece.generalize(pieceOn) == Piece.◯.FU && Piece.▲△(pieceOn, playerTurn))
                safetyScore += PAWN_SHIELD_BONUS_PER_PAWN
            }
          }
        }

        val opponentTurn = playerTurn.change
        var attackingPieces: Set[Point] = Set.empty
        for (y <- 0 to 8; x <- 0 to 8) {
          val pos = Point(y, x)
          val piece = board.squares.get(pos)
          if (piece != Piece.❏ && Piece.▲△(piece, opponentTurn)) {
            val moves = Rule.generateMovablePoints(board, pos, piece, opponentTurn, false)
            var attacksAdj = false
            for (m <- moves if !attacksAdj) {
              val mp = m._1
              val dist = Math.abs(mp.y - kingPos.y) + Math.abs(mp.x - kingPos.x)
              if (dist <= 1) attacksAdj = true
            }
            if (attacksAdj) attackingPieces += pos
          }
        }
        safetyScore += attackingPieces.size * KING_ADJACENT_ATTACK_PENALTY

        var lineThreatPieces: Set[Point] = Set.empty
        for (oy <- 0 to 8; ox <- 0 to 8) {
          val oppPos = Point(oy, ox)
          val oppPiece = board.squares.get(oppPos)
          if (oppPiece != Piece.❏ && Piece.▲△(oppPiece, opponentTurn) && !lineThreatPieces.contains(oppPos)) {
            val gen = Piece.generalize(oppPiece)
            val isMajor = gen == Piece.◯.HI || gen == Piece.◯.KA || Piece.isPromoted(oppPiece)
            if (isMajor) {
              val rookDirs = List((0,1),(0,-1),(1,0),(-1,0))
              val bishopDirs = List((1,1),(1,-1),(-1,1),(-1,-1))
              val dirs = gen match {
                case Piece.◯.HI => if (Piece.isPromoted(oppPiece)) rookDirs ++ bishopDirs else rookDirs
                case Piece.◯.KA => if (Piece.isPromoted(oppPiece)) bishopDirs ++ rookDirs else bishopDirs
                case _ => Nil
              }
              var kingFound = false
              for (d <- dirs if !kingFound) {
                var cy = oy + d._1
                var cx = ox + d._2
                var clear = true
                while (cy >=0 && cy <=8 && cx >=0 && cx <=8 && clear && !kingFound) {
                  if (cy == kingPos.y && cx == kingPos.x) {
                    kingFound = true
                    val penalty = if (Piece.isPromoted(oppPiece)) LINE_OF_SIGHT_THREAT_PENALTY_MAJOR else LINE_OF_SIGHT_THREAT_PENALTY_MINOR
                    safetyScore += penalty
                    lineThreatPieces += oppPos
                  } else if (board.squares.get(Point(cy,cx)) != Piece.❏) {
                    clear = false
                  } else {
                    cy += d._1
                    cx += d._2
                  }
                }
              }
            }
          }
        }

        var friendlyGeneralCount = 0
        for (y <- 0 to 8; x <- 0 to 8) {
          val pos = Point(y, x)
          val piece = board.squares.get(pos)
          if (Piece.▲△(piece, playerTurn)) {
            Piece.generalize(piece) match {
              case Piece.◯.KI | Piece.◯.GI =>
                val dist = Math.abs(pos.y - kingPos.y) + Math.abs(pos.x - kingPos.x)
                if (dist <= GENERAL_NEAR_KING_DISTANCE) friendlyGeneralCount += 1
              case _ =>
            }
          }
        }
        if (friendlyGeneralCount >= 2) safetyScore += GOOD_CASTLE_BONUS

        val (mkY, mkX, g1Y, g1X, g2Y, g2X, sY, sX) =
          if (playerTurn == PlayerA) (7,1,7,2,7,3,6,2) else (1,7,1,6,1,5,2,6)

        if (kingPos.y == mkY && kingPos.x == mkX) {
          val gold = Piece.convert(Piece.◯.KI, playerTurn)
          val silver = Piece.convert(Piece.◯.GI, playerTurn)
          val g1p = board.squares.get(Point(g1Y,g1X)) == gold
          val g2p = board.squares.get(Point(g2Y,g2X)) == gold
          val sp  = board.squares.get(Point(sY,sX)) == silver
          if (g1p && g2p && sp) safetyScore += MINO_CASTLE_BONUS
        }

      case None =>
        safetyScore -= 10000
    }
    safetyScore
  }

  private def calculateCenterControlScore(board: Board, playerTurn: Turn): Int = {
    var score = 0
    for (y <- 0 to 8; x <- 0 to 8) {
      val pos = Point(y, x)
      val piece = board.squares.get(pos)
      if (piece != Piece.❏) {
        if (CENTER_FILES.contains(pos.x) && CENTER_RANKS.contains(pos.y)) {
          if (Piece.▲△(piece, playerTurn)) score += CENTER_SQUARE_BONUS
        }
      }
    }
    score
  }

  private def calculatePromotionPotentialScore(board: Board, playerTurn: Turn): Int = {
    var score = 0
    val inZone = if (playerTurn == PlayerA) (y: Int) => y <= 2 else (y: Int) => y >= 6

    for (y <- 0 to 8; x <- 0 to 8) {
      val pos = Point(y, x)
      val piece = board.squares.get(pos)
      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn) && !Piece.isPromoted(piece)) {
        if (inZone(pos.y)) {
          Piece.generalize(piece) match {
            case Piece.◯.FU | Piece.◯.KY | Piece.◯.KE | Piece.◯.GI => score += PROMOTION_POTENTIAL_MINOR
            case Piece.◯.KA | Piece.◯.HI => score += PROMOTION_POTENTIAL_MAJOR
            case _ =>
          }
        }
      }
    }
    score
  }

  private def calculatePieceSquareTableScore(board: Board, playerTurn: Turn): Int = {
    var pstScore = 0
    for (y <- 0 to 8; x <- 0 to 8) {
      val pos = Point(y, x)
      val piece = board.squares.get(pos)
      if (piece != Piece.❏ && Piece.▲△(piece, playerTurn)) {
        val gen = Piece.generalize(piece)
        gen match {
          case Piece.◯.HI =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = x
            pstScore += ROOK_PST_SENTE(yy)(xx)
          case Piece.◯.KA =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += BISHOP_PST_SENTE(yy)(xx)
          case Piece.◯.KE =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += KNIGHT_PST_SENTE(yy)(xx)
          case Piece.◯.KI =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += GOLD_PST_SENTE(yy)(xx)
          case Piece.◯.GI =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += SILVER_PST_SENTE(yy)(xx)
          case Piece.◯.KY =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += LANCE_PST_SENTE(yy)(xx)
          case Piece.◯.FU =>
            val yy = if (playerTurn == PlayerA) y else 8 - y
            val xx = if (playerTurn == PlayerA) x else 8 - x
            pstScore += PAWN_PST_SENTE(yy)(xx)
          case _ =>
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
        val legalMoves = Rule.generateMovablePoints(board, attackerPos, attackerPiece, playerTurn, true)
        for (moveTuple <- legalMoves) {
          val attackedPos = moveTuple._1
          val attackedPiece = board.squares.get(attackedPos)
          if (attackedPiece != Piece.❏ && Piece.▲△(attackedPiece, opponentTurn)) {
            attackingScore += ATTACKING_PIECE_BONUS
            if (getNominalPieceValue(attackedPiece) > getNominalPieceValue(attackerPiece))
              attackingScore += ATTACKING_MORE_VALUABLE_PIECE_BONUS
          }
        }
      }
    }
    attackingScore
  }

  private def calculatePieceCoordinationScore(board: Board, playerTurn: Turn): Int = {
    var coordinationScore = 0

    var rooks: List[(Point, Piece)] = Nil
    for (y <- 0 to 8; x <- 0 to 8) {
      val pos = Point(y, x)
      val piece = board.squares.get(pos)
      if (Piece.▲△(piece, playerTurn)) {
        val gen = Piece.generalize(piece)
        if (gen == Piece.◯.HI) {
          rooks = (pos, piece) :: rooks
        }
      }
    }

    if (rooks.size >= 2) {
      for (i <- rooks.indices; j <- i + 1 until rooks.size) {
        val (p1, _) = rooks(i)
        val (p2, _) = rooks(j)
        var connected = false
        var clear = true
        if (p1.y == p2.y) {
          connected = true
          val startX = Math.min(p1.x, p2.x)
          val endX = Math.max(p1.x, p2.x)
          for (x <- startX + 1 until endX if clear) {
            if (board.squares.get(Point(p1.y, x)) != Piece.❏) clear = false
          }
        } else if (p1.x == p2.x) {
          connected = true
          val startY = Math.min(p1.y, p2.y)
          val endY = Math.max(p1.y, p2.y)
          for (y <- startY + 1 until endY if clear) {
            if (board.squares.get(Point(y, p1.x)) != Piece.❏) clear = false
          }
        }
        if (connected && clear) coordinationScore += CONNECTED_ROOKS_BONUS
      }
    }

    for (y <- 0 to 8; x <- 0 to 8) {
      val defenderPos = Point(y, x)
      val defenderPiece = board.squares.get(defenderPos)
      if (defenderPiece != Piece.❏ && Piece.▲△(defenderPiece, playerTurn)) {
        val legalMoves = Rule.generateMovablePoints(board, defenderPos, defenderPiece, playerTurn, true)
        for (moveTuple <- legalMoves) {
          val defendedPos = moveTuple._1
          val pieceOnDefended = board.squares.get(defendedPos)
          if (pieceOnDefended != Piece.❏ && Piece.▲△(pieceOnDefended, playerTurn) && defenderPos != defendedPos) {
            coordinationScore += DEFENDING_PIECE_BONUS
            if (getNominalPieceValue(pieceOnDefended) - getNominalPieceValue(defenderPiece) >= VALUABLE_PIECE_DIFFERENCE_THRESHOLD)
              coordinationScore += DEFENDING_MORE_VALUABLE_PIECE_BONUS
          }
        }
      }
    }
    coordinationScore
  }

  def evaluate(board: Board, turn: Turn): Int = {
    val materialScore = EvaluationV1.evaluate(board, turn)

    val myMobility = calculatePieceMobilityScore(board, turn)
    val myKingSafety = calculateKingSafetyScore(board, turn)
    val myPromotionPotential = calculatePromotionPotentialScore(board, turn)
    val myCenterControl = calculateCenterControlScore(board, turn)
    val myPstScore = calculatePieceSquareTableScore(board, turn)
    val myAttackingScore = calculateAttackingPiecesScore(board, turn)
    val myCoordinationScore = calculatePieceCoordinationScore(board, turn)

    val opponent = turn.change
    val oppMobility = calculatePieceMobilityScore(board, opponent)
    val oppKingSafety = calculateKingSafetyScore(board, opponent)
    val oppPromotionPotential = calculatePromotionPotentialScore(board, opponent)
    val oppCenterControl = calculateCenterControlScore(board, opponent)
    val oppPstScore = calculatePieceSquareTableScore(board, opponent)
    val oppAttackingScore = calculateAttackingPiecesScore(board, opponent)
    val oppCoordinationScore = calculatePieceCoordinationScore(board, opponent)

    val positionalDiff =
      (myMobility - oppMobility) +
      (myKingSafety - oppKingSafety) +
      (myPromotionPotential - oppPromotionPotential) +
      (myCenterControl - oppCenterControl) +
      (myPstScore - oppPstScore) +
      (myAttackingScore - oppAttackingScore) +
      (myCoordinationScore - oppCoordinationScore)

    materialScore + positionalDiff
  }
}
