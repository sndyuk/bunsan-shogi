package jp.sndyuk.shogi.core

import org.scalatest._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader
import Piece._

class BoardSpec extends FlatSpec with Matchers with BeforeAndAfter {

  "A piece" should "move to a point" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 6)

    val result = Board().move(state, oldPos, newPos, true, false)

    result should be(State(List(Transition(oldPos, newPos, false, None)), PlayerB))
  }

  "A piece" should "not move to a point" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 5)

    val result = Board().moveOpt(state, oldPos, newPos, true, false)

    result should be(None)
  }

  private def move17FUTo14FU_93FUTo96FU = {
    List(
      Transition(Board.humanReadableToPoint(1, 7), Board.humanReadableToPoint(1, 6), false, None),
      Transition(Board.humanReadableToPoint(9, 3), Board.humanReadableToPoint(9, 4), false, None),
      Transition(Board.humanReadableToPoint(1, 6), Board.humanReadableToPoint(1, 5), false, None),
      Transition(Board.humanReadableToPoint(9, 4), Board.humanReadableToPoint(9, 5), false, None),
      Transition(Board.humanReadableToPoint(1, 5), Board.humanReadableToPoint(1, 4), false, None),
      Transition(Board.humanReadableToPoint(9, 5), Board.humanReadableToPoint(9, 6), false, None)).reverse
  }

  "A piece" should "be promoted" in {

    val transtions = move17FUTo14FU_93FUTo96FU
    val state = State(transtions, PlayerA)

    val oldPos = Board.humanReadableToPoint(1, 4)
    val newPos = Board.humanReadableToPoint(1, 3)
    val board = Board().newBoard(state)
    val result = Board().newBoard(state).move(state, oldPos, newPos, true, true)

    result should be(State(Transition(oldPos, newPos, true, Some(△.FU)) :: transtions, PlayerB))
  }

  "A piece" should "not be promoted" in {

    val state = State(Nil, PlayerA)
    val oldPos = Board.humanReadableToPoint(1, 7)
    val newPos = Board.humanReadableToPoint(1, 6)

    val result = Board().moveOpt(state, oldPos, newPos, true, true)

    result should be(None)
  }

  "A piece" should "capture an player B's piece" in {

    val transtions = move17FUTo14FU_93FUTo96FU
    val state = State(transtions, PlayerA)

    val oldPos = Board.humanReadableToPoint(1, 4)
    val newPos = Board.humanReadableToPoint(1, 3)

    val board = Board().newBoard(state)
    board.move(state, oldPos, newPos, true, true)

    val result = board.piece((9, 2), PlayerA) // 9: 持駒, 2: 歩

    result should be(▲.FU)
  }

  private def move17FUTo12TO_93FUTo97FU = {
    List(Transition(Board.humanReadableToPoint(1, 4), Board.humanReadableToPoint(1, 3), true, None),
      Transition(Board.humanReadableToPoint(9, 6), Board.humanReadableToPoint(9, 7), false, None),
      Transition(Board.humanReadableToPoint(1, 3), Board.humanReadableToPoint(1, 2), false, None)).reverse ::: move17FUTo14FU_93FUTo96FU
  }

  "Player B" should "put a captured piece" in {

    val transtions = move17FUTo12TO_93FUTo97FU
    val state = State(transtions, PlayerB)

    val oldPos = Board.humanReadableToPoint(2, 0) // 歩, 持駒
    val newPos = Board.humanReadableToPoint(1, 3)

    val board = Board().newBoard(state)
    val result = board.move(state, oldPos, newPos, true, false)
    result should be(State(Transition(oldPos, newPos, false, None) :: transtions, PlayerA))

    val capturedPiece = board.capturedPieces.get((9, 2), PlayerB) // 9: 持駒, 2: 歩
    capturedPiece should be(None)
  }

  "Player B" should "not put a captured piece becase of 2 FU" in {

    val transtions = move17FUTo12TO_93FUTo97FU
    val state = State(transtions, PlayerB)

    val oldPos = Board.humanReadableToPoint(2, 0) // 歩, 持駒
    val newPos = Board.humanReadableToPoint(9, 5)

    val board = Board().newBoard(state)
    assertThrows[IllegalStateException] {
      board.move(state, oldPos, newPos, true, false)
    }

    val capturedPiece = board.piece((9, 2), PlayerB) // 9: 持駒, 2: 歩
    capturedPiece should be(△.FU)
  }
}
