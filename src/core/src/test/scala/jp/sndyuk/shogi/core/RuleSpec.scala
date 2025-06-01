package jp.sndyuk.shogi.core

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Piece._

class RuleSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {

  "36FU" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 6)
    val piece = ▲.FU
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(3, 5), false))
  }

  "39GI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 9)
    val piece = ▲.GI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(3, 8), false))
  }

  "28HI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(2, 8)
    val piece = ▲.HI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only ((Board.humanReadableToPoint(1, 8), false), (Board.humanReadableToPoint(3, 8), false), (Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(5, 8), false), (Board.humanReadableToPoint(6, 8), false), (Board.humanReadableToPoint(7, 8), false))
  }

  "88KA" should "not be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(8, 8)
    val piece = ▲.KA
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream shouldBe empty
  }

  // --- Tests for Rule.movableScopes ---

  "▲.FU (Sente Pawn)" should "have correct movable scopes" in {
    val piece = ▲.FU
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-1, 0, false))
  }

  "▲.KY (Sente Lance)" should "have correct movable scopes" in {
    val piece = ▲.KY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-1, 0, Rule.∞))
  }

  "▲.KE (Sente Knight)" should "have correct movable scopes" in {
    val piece = ▲.KE
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List((-2, -1, false), (-2, 1, false))
  }

  "▲.GI (Sente Silver)" should "have correct movable scopes" in {
    val piece = ▲.GI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (1, -1, false), (1, 1, false)
    )
  }

  "▲.KI (Sente Gold)" should "have correct movable scopes" in {
    val piece = ▲.KI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.KA (Sente Bishop)" should "have correct movable scopes" in {
    val piece = ▲.KA
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "▲.HI (Sente Rook)" should "have correct movable scopes" in {
    val piece = ▲.HI
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
  }

  "▲.OU (Sente King)" should "have correct movable scopes" in {
    val piece = ▲.OU
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false),
      (0, -1, false), (0, 1, false),
      (1, 0, false), (1, -1, false), (1, 1, false)
    )
  }

  // Promoted Sente Pieces
  "▲.TO (Promoted Sente Pawn)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.TO
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NY (Promoted Sente Lance)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NK (Promoted Sente Knight)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NK
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.NG (Promoted Sente Silver)" should "have correct movable scopes (same as Gold)" in {
    val piece = ▲.NG
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      (-1, 0, false), (-1, -1, false), (-1, 1, false), (0, -1, false), (0, 1, false), (1, 0, false)
    )
  }

  "▲.UM (Promoted Sente Bishop)" should "have correct movable scopes" in {
    val piece = ▲.UM
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from ▲.OU)
      (-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false),
      (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false),
      // Bishop moves (from ▲.KA)
      (-1, -1, Rule.∞), (-1, 1, Rule.∞), (1, -1, Rule.∞), (1, 1, Rule.∞)
    )
  }

  "▲.RY (Promoted Sente Rook)" should "have correct movable scopes" in {
    val piece = ▲.RY
    val scopes = Rule.movableScopes(piece)
    scopes should contain theSameElementsAs List(
      // King moves (from ▲.OU)
      (-1, 0, false), (-1, 1, false), (0, 1, false), (1, 1, false),
      (1, 0, false), (1, -1, false), (0, -1, false), (-1, -1, false),
      // Rook moves (from ▲.HI)
      (-1, 0, Rule.∞), (1, 0, Rule.∞), (0, -1, Rule.∞), (0, 1, Rule.∞)
    )
  }
}
