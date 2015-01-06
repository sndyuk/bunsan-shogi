package jp.sndyuk.shogi

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.algorithm.core._
import jp.sndyuk.shogi.ai.Utils

object Main extends App {

  val board = Board()

  Seq(
    ((6, 0), (5, 0)),
    ((8, 3), (7, 4)))
    .foldLeft(Option(State(Nil, PlayerA))) { (stateOpt, move) =>
      stateOpt match {
        case Some(state) => {
          val newState = board.moveOpt(state, move._1, move._2, false, false)
          println(board.toString)
          println()
          newState
        }
        case _ => {
          println("Can not move.")
          None
        }
      }
    }
  if (false) {
    // Orion DB
    HashStorage.start
    val storage = new HashStorage[String, Number]()
    println()

    {
      val start = System.currentTimeMillis()
      for (i <- 0 until 10000) {
        storage += 1.toString -> 1
      }
      println((System.currentTimeMillis() - start))
    }
    {
      val start = System.currentTimeMillis()
      for (i <- 0 until 100) {
        storage(i.toString)
      }
      println((System.currentTimeMillis() - start))
    }
  }

  if (true) {
    val board = Board()
    val start = System.currentTimeMillis()
    var count = 0
    for (i <- 0 to 1000000) {
      val plans = Utils.plans(board, State())
      count += plans.length
    }
    val elapsed = System.currentTimeMillis() - start
    println(s"count: $count, ${elapsed}, ${count / (elapsed / 1000d)}/sec")
  }
}
