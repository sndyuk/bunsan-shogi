package jp.sndyuk.shogi.core

import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Piece._

class RuleSpec extends FlatSpec with Matchers with BeforeAndAfter {

  "36FU" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 6)
    val piece = ▲.FU
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only((Board.humanReadableToPoint(3, 5), false))
  }

  "39GI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(3, 9)
    val piece = ▲.GI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only((Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(3, 8), false))
  }

  "28HI" should "be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(2, 8)
    val piece = ▲.HI
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream should contain only((Board.humanReadableToPoint(1, 8), false), (Board.humanReadableToPoint(3, 8), false), (Board.humanReadableToPoint(4, 8), false), (Board.humanReadableToPoint(5, 8), false), (Board.humanReadableToPoint(6, 8), false), (Board.humanReadableToPoint(7, 8), false))
  }

  "88KA" should "not be able to move" in {

    val board = Board()
    val oldPos = Board.humanReadableToPoint(8, 8)
    val piece = ▲.KA
    val moves = Rule.generateMovablePoints(board, oldPos, piece, PlayerA, true)

    moves.toStream shouldBe empty
  }
}
