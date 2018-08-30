package jp.sndyuk.shogi

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.core.Piece._
import jp.sndyuk.shogi.algorithm.core._
import jp.sndyuk.shogi.ai.simulator.Utils
import jp.sndyuk.shogi.core.Transition
import scala.collection.immutable.Queue

object Main extends App {

  val board2 = Board()

  Seq(
    ((7, 7), (7, 6)), ((3, 3), (3, 4)),
    ((8, 8), (2, 2)), ((8, 3), (8, 4)),
    ((6, 9), (7, 8)), ((7, 1), (7, 2)),
    ((2, 2), (7, 7)), ((8, 4), (8, 5)),
    ((6, 7), (6, 6)), ((8, 5), (8, 6)),
    ((7, 8), (6, 7)), ((8, 6), (8, 7)),
    ((2, 7), (2, 6)), ((3, 1), (3, 2)),
    ((7, 9), (8, 8)), ((8, 2), (8, 5)),
    ((8, 8), (8, 7)))
    .foldLeft(Option(State(Nil, PlayerA))) { (stateOpt, move) =>
      stateOpt match {
        case Some(state) => {
          val oldPos = Board.humanReadableToPoint(move._1._1, move._1._2)
          val newPos = Board.humanReadableToPoint(move._2._1, move._2._2)

          val newState = board2.moveOpt(state, oldPos, newPos, false, false)
          println(board2.toString)
          println()
          newState
        }
        case _ => {
          println("Can not move.")
          None
        }
      }
    }

  if (true) {
    // Orion DB
    HashStorage.start
    val storage = new HashDocument[Int, String]("TestDoc17")
    val p = Utils.plans(board2, State()).toList
    println()

    {
      val start = System.currentTimeMillis()
      for (i <- 0 until 10000) {
        storage.put(i % p.length, p(i % p.length).toString)
      }
      println((System.currentTimeMillis() - start))
    }
    {
      val start = System.currentTimeMillis()
      for (i <- 0 until 10000) {
        assert(storage.get(i % p.length).isDefined)
      }
      println((System.currentTimeMillis() - start))
    }
  }

  if (false) {
    // Warm up
    for (i <- 0 to 50000) {
      val plans = Utils.plans(board2, State())
    }

    // 198手
    //　9　　8　　7　　6　　5　　4　　3　　2　　1
    //△香|　　|　　|　　|　　|　　|　　|△桂|△香|一
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //　　|　　|　　|　　|　　|▲と|　　|△金|△玉|二
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //　　|　　|△桂|△歩|　　|▲銀|　　|　　|　　|三
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //△歩|　　|△歩|　　|　　|　　|　　|▲歩|△歩|四
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //　　|　　|　　|▲歩|　　|　　|▲銀|△歩|　　|五
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //　　|▲歩|▲歩|△角|　　|　　|▲歩|　　|▲歩|六
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //▲歩|　　|　　|　　|　　|　　|▲金|▲銀|　　|七
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //▲飛|　　|　　|　　|　　|　　|　　|　　|　　|八
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //▲香|▲桂|　　|　　|　　|　　|△角|▲玉|▲香|九
    //―-―-―-―-―-―-―-―-―-―-―-―-―-―-―
    //△ 持駒[玉0, 歩5, 金1, 銀1, 飛0, 角0, 桂1, 香0]
    //▲ 持駒[玉0, 歩0, 金1, 銀0, 飛1, 角0, 桂0, 香0]
    val board = new Board()
    board.init2(Seq(
      Seq(△.KY, ❏, ❏, ❏, ❏, ❏, ❏, △.KE, △.KY),
      Seq(❏, ❏, ❏, ❏, ❏, ▲.TO, ❏, △.KI, △.OU),
      Seq(❏, ❏, △.KE, △.FU, ❏, ▲.GI, ❏, ❏, ❏),
      Seq(△.FU, ❏, △.FU, ❏, ❏, ❏, ❏, ▲.FU, △.FU),
      Seq(❏, ❏, ❏, ▲.FU, ❏, ❏, ▲.GI, △.FU, ❏),
      Seq(❏, ▲.FU, ▲.FU, △.KA, ❏, ❏, ▲.FU, ❏, ▲.FU),
      Seq(▲.FU, ❏, ❏, ❏, ❏, ❏, ▲.KI, ▲.GI, ❏),
      Seq(▲.HI, ❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏),
      Seq(▲.KY, ▲.KE, ❏, ❏, ❏, ❏, △.KA, ▲.OU, ▲.KY)),
      Seq((△.KI, 1), (△.HI, 1),
        (▲.FU, 5), (▲.KE, 1), (▲.GI, 1), (▲.KI, 1)))
    println("----------------------------")
    println(board.toString())

    // 初期
    val defaultBoard = Board()
    val start = System.currentTimeMillis()
    var count = 0
    val state = State(Nil, PlayerB)
    for (i <- 0 until 100000) {
      val plans = Utils.plans(defaultBoard, state)
      plans.foreach { p =>
        count += 1
        //        val s = board.move(state, p.oldPos, p.newPos, false, p.nari)
        //        board.rollback(s)
      }
    }

    val elapsed = System.currentTimeMillis() - start

    // 移動後
    val start2 = System.currentTimeMillis()
    var count2 = 0
    for (i <- 0 until 100000) {
      val plans = Utils.plans(board, state)
      plans.foreach { p =>
        count2 += 1
        //        val s = board.move(state, p.oldPos, p.newPos, false, p.nari)
        //        board.rollback(s)
      }
      //      if (i == 0) {
      //        plans.filter { t => !Point.isCaptured(t.oldPos) }.foreach { println(_) }
      //      }
    }
    val elapsed2 = System.currentTimeMillis() - start2

    // Sandbox
    val start3 = System.currentTimeMillis()
    var count3 = 0
    var q = Queue[Board]()

    for (i <- 0 until 1) {
      val plans = Utils.plans(board, state)
      plans.foreach { p =>
        count3 += 1
        println(p)
        val s = board.move(state, p.oldPos, p.newPos, false, p.nari)
        //        val p2 = Utils.plans(board, s, true)
        board.rollback(s)
      }
    }

    val elapsed3 = System.currentTimeMillis() - start3

    println(s"初期盤面: count: $count, ${elapsed}, ${(count.toDouble / elapsed * 1000).toInt}/sec")
    println(s"中盤盤面: count: $count2, ${elapsed2}, ${(count2.toDouble / elapsed2 * 1000).toInt}/sec")
    println(s"中盤枝刈: count: $count3, ${elapsed3}, ${(count3.toDouble / elapsed3 * 1000).toInt}/sec")

    // Loop: 300,000
    // 初期盤面: count: 9000000, 2031, 4431314/sec
    // 中盤盤面: count: 62400000, 7137, 8743169/sec

    // Loop: 300,000 移動込
    // 初期盤面: count: 9000000, 2859, 3147953/sec
    // 中盤盤面: count: 54600000, 10324, 5288647/sec
  }
}
