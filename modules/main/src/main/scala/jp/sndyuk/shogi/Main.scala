package jp.sndyuk.shogi

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.algorithm.core._

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
