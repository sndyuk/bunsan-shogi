package jp.sndyuk.shogi.ui

import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.player.HumanPlayer
import jp.sndyuk.shogi.player.AIPlayer
import jp.sndyuk.shogi.player.Player
import akka.actor.Actor
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.player.CommandReader

object Console extends App with Shogi {

  override val board = new Board()

  val commandReader = new CommandReader {

    // (筋,段)(筋,段)[+]  (+: 成り)
    private[this] val routeRegex = """\((\d+),(\d+)\)\((\d+),(\d+)\)([\+])?""".r

    def read(state: State): Transition = {
      val routeRegex(a, b, c, d, e) = scala.Console.in.readLine
      val oldPos = Board.humanReadableToPoint(a.toInt, b.toInt)
      val newPos = Board.humanReadableToPoint(c.toInt, d.toInt)
      println(e)
      Transition(oldPos, newPos, e == "+", board.pieceOnBoardNotEmpty(newPos))
    }
  }

  //   val playerA = new AIPlayer(board)
  override val playerA = new HumanPlayer("playerA", board, commandReader, true)
  //  val playerB = new HumanPlayer(board)
  override val playerB = new AIPlayer()

  println(board.toString)
  println("(筋,段)(筋,段)")
  println("持駒([0 = 玉, 1 = 歩, 2 = 金, 3 = 銀, 4 = 飛, 5 = 角, 6 = 桂, 7 = 香], 0)")

  override def afterMove(player: Player, oldPos: Point, newPos: Point) {
    println(board.toString)
  }

  override def beforeMove(player: Player) {
    println(s"Turn: $player")
  }

  override def done(player: Player, oldPos: Point, newPos: Point, winner: Player) {
    println(s"Done. Winner: $winner")
  }

  override def failToMove(player: Player, oldPos: Point, newPos: Point) {
    println(s"Could not move: $oldPos -> $newPos")
  }

  override def onError(e: Exception) {
    println("Could not parse your command. Retry?(Y|n) or show the error and exit (x)")
    readLine.toUpperCase() match {
      case "N" => sys.exit
      case "X" => {
        e.printStackTrace()
        sys.exit
      }
    }
  }

  start()
}