package jp.sndyuk.shogi.core

import scala.collection.mutable.HashMap

import jp.sndyuk.shogi.player.Utils

object IDFinder extends App {

  var prevBoard = Board()
  var prevState = State()
  var prevId = ID(prevBoard)

  val idMap = HashMap[ID, Board]()
  val hashMap = HashMap[Long, Board]()
  idMap += prevId -> prevBoard
  hashMap += prevId.hashLong -> prevBoard

  val plans = Utils.plans(prevBoard, prevState)

  def onErr(msg: String, errBoard1: Board, errBoard2: Board): Unit = {
    println("Error----------------------")
    println(errBoard1)
    println(errBoard2)
    println(msg)
    println("---------------------------")
  }

  val originalBoard = prevBoard
  val originalState = prevState
  var max = prevId.hashLong
  var min = prevId.hashLong
  for (plan <- plans) {
    println(plan)
    val board = originalBoard.copy()
    val state = board.move(originalState, plan.oldPos, plan.newPos, false, plan.nari)
    val id = ID(board)
    println(s"Hash: ${"%19d".format(id.hashLong)}")

    if (idMap.contains(id)) {
      onErr(s"Conflict: $id", idMap.get(id).get, board)
      sys.exit
    }
    if (hashMap.contains(id.hashLong)) {
      onErr(s"Conflict: $id", hashMap.get(id.hashLong).get, board)
      sys.exit
    }

    prevBoard = board
    prevState = state
    prevId = id
    idMap += id -> board
    hashMap += id.hashLong -> board

    max = Math.max(max, id.hashLong)
    min = Math.min(min, id.hashLong)
  }
  println(s"max: $max, min: $min, diff: ${max - min}")
}
