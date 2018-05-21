package jp.sndyuk.shogi.ai.learner

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece._
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.ai.simulator.Utils
import jp.sndyuk.shogi.core.State
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object IDFinder extends App {

  var prevBoard = Board()
  var prevState = State()
  var prevId = ID(prevBoard)

  val idMap = HashMap[ID, Board]()
  val hashMap = HashMap[Int, Board]()
  idMap += prevId -> prevBoard
  hashMap += prevId.hashCode -> prevBoard

  val plans = Utils.plans(prevBoard, prevState, false)

  def onErr(msg: String, errBoard1: Board, errBoard2: Board): Unit = {
    println("Error----------------------")
    println(errBoard1)
    println(errBoard2)
    println(msg)
    println("---------------------------")
  }

  val originalBoard = prevBoard
  val originalState = prevState
  var max = prevId.hashCode
  var min = prevId.hashCode
  for (plan <- plans) {
    println(plan)
    val board = originalBoard.copy()
    val state = board.move(originalState, plan.oldPos, plan.newPos, false, plan.nari)
    val id = ID(board)
    println(s"Hash: ${"%19d".format(id.hashCode)}")

    if (idMap.contains(id)) {
      onErr(s"Conflict: $id", idMap.get(id).get, board)
      sys.exit
    }
    if (hashMap.contains(id.hashCode)) {
      onErr(s"Conflict: $id", hashMap.get(id.hashCode).get, board)
      sys.exit
    }

    prevBoard = board
    prevState = state
    prevId = id
    idMap += id -> board
    hashMap += id.hashCode -> board

    max = Math.max(max, id.hashCode)
    min = Math.min(min, id.hashCode)
  }
  println(s"max: $max, min: $min, diff: ${max - min}")
}
