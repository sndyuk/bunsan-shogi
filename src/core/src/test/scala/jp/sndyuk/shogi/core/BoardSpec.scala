package jp.sndyuk.shogi.core

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Piece.▲
import Piece.△

class BoardSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {

  // Note: Points like (9,2) are used in some tests below to refer to specific types/slots of pieces in hand,
  // likely based on an internal convention within the Board class for representing captured pieces.

  "Sente Pawn at 1,7" should "move one step forward to 1,6 on an empty board state" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 6)

    val result = Board().move(state, oldPos, newPos, true, false)

    result shouldBe State(List(Transition(oldPos, newPos, false, None)), PlayerB)
  }

  "Sente Pawn at 1,7" should "not be able to move two steps forward to 1,5 on an empty board state" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 5)

    val result = Board().moveOpt(state, oldPos, newPos, true, false)

    result shouldBe None
  }

  // Helper to create a history of Sente pawn moving 17->16->15->14 and Gote pawn 93->94->95->96
  private def move17FUTo14FU_93FUTo96FU = {
    List(
      Transition(Board.humanReadableToPoint(1, 7), Board.humanReadableToPoint(1, 6), false, None),
      Transition(Board.humanReadableToPoint(9, 3), Board.humanReadableToPoint(9, 4), false, None),
      Transition(Board.humanReadableToPoint(1, 6), Board.humanReadableToPoint(1, 5), false, None),
      Transition(Board.humanReadableToPoint(9, 4), Board.humanReadableToPoint(9, 5), false, None),
      Transition(Board.humanReadableToPoint(1, 5), Board.humanReadableToPoint(1, 4), false, None),
      Transition(Board.humanReadableToPoint(9, 5), Board.humanReadableToPoint(9, 6), false, None)).reverse
  }

  "Sente Pawn at 1,4" should "be promoted when moving to 1,3 after a sequence of moves" in {

    val transtions = move17FUTo14FU_93FUTo96FU
    val state = State(transtions, PlayerA)

    val oldPos = Board.humanReadableToPoint(1, 4)
    val newPos = Board.humanReadableToPoint(1, 3)
    val result = Board().newBoard(state).move(state, oldPos, newPos, true, true)

    result shouldBe State(Transition(oldPos, newPos, true, Some(△.FU)) :: transtions, PlayerB)
  }

  "Sente Pawn at 1,7" should "not be allowed to promote when moving to 1,6 (move becomes invalid)" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 6)

    val result = Board().moveOpt(state, oldPos, newPos, true, true)

    result shouldBe None
  }

  "Sente's promoted Pawn capturing at 1,3" should "add a FU to Sente's hand" in {

    val transtions = move17FUTo14FU_93FUTo96FU
    val state = State(transtions, PlayerA)

    val oldPos = Board.humanReadableToPoint(1, 4)
    val newPos = Board.humanReadableToPoint(1, 3)

    val board = Board().newBoard(state)
    board.move(state, oldPos, newPos, true, true)

    val result = board.piece((9, 2), PlayerA) // 9: 持駒, 2: 歩

    result shouldBe ▲.FU
  }

  // Helper extending move17FUTo14FU_93FUTo96FU with Sente promoting and moving 14->13(promote)->12, and Gote moving 96->97
  private def move17FUTo12TO_93FUTo97FU = {
    List(Transition(Board.humanReadableToPoint(1, 4), Board.humanReadableToPoint(1, 3), true, None),
      Transition(Board.humanReadableToPoint(9, 6), Board.humanReadableToPoint(9, 7), false, None),
      Transition(Board.humanReadableToPoint(1, 3), Board.humanReadableToPoint(1, 2), false, None)).reverse ::: move17FUTo14FU_93FUTo96FU
  }

  "Player B" should "be able to drop a captured FU from hand to 1,3" in {

    val transtions = move17FUTo12TO_93FUTo97FU
    val state = State(transtions, PlayerB)

    val oldPos = Board.humanReadableToPoint(2, 0) // 歩, 持駒
    val newPos = Board.humanReadableToPoint(1, 3)

    val board = Board().newBoard(state)
    val result = board.move(state, oldPos, newPos, true, false)
    result shouldBe State(Transition(oldPos, newPos, false, None) :: transtions, PlayerA)

    val capturedPiece = board.capturedPieces.get((9, 2), PlayerB) // 9: 持駒, 2: 歩
    capturedPiece shouldBe None
  }

  "Player B" should "get an IllegalStateException when attempting to drop a FU that results in Nifu" in {

    val transtions = move17FUTo12TO_93FUTo97FU
    val state = State(transtions, PlayerB)

    val oldPos = Board.humanReadableToPoint(2, 0) // 歩, 持駒
    val newPos = Board.humanReadableToPoint(9, 5)

    val board = Board().newBoard(state)
    assertThrows[IllegalStateException] {
      board.move(state, oldPos, newPos, true, false)
    }

    val capturedPiece = board.piece((9, 2), PlayerB) // 9: 持駒, 2: 歩
    capturedPiece shouldBe △.FU
  }
}
