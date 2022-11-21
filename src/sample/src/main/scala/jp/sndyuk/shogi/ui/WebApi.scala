package jp.sndyuk.shogi.ui

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.player.CommandReader
import jp.sndyuk.shogi.player.HumanPlayer
import jp.sndyuk.shogi.player.Player

class WebApi extends App with Shogi {

  override val board = Board()

  val commandReader = new CommandReader {

    // {
    //   old: 67
    //   new: 66
    //   promote: false
    // }
    private[this] val routeRegex = """\((\d+),(\d+)\)\((\d+),(\d+)\)([\+])?""".r

    def read(state: State): Transition = {
      val routeRegex(a, b, c, d, e) = scala.Console.in.readLine
      val oldPos = Board.humanReadableToPoint(a.toInt, b.toInt)
      val newPos = Board.humanReadableToPoint(c.toInt, d.toInt)
      println(e)
      Transition(oldPos, newPos, e == "+", board.pieceOnBoardNotEmpty(newPos))
    }
  }

  override val playerA = new HumanPlayer("playerA", board, commandReader, true)
  override val playerB = new HumanPlayer("playerA", board, commandReader, true)

  override def afterMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    println(board.toString)
  }

  override def beforeMove(player: Player): Unit = {
    println(s"Turn: $player")
  }

  override def done(player: Player, oldPos: Point, newPos: Point, winner: Player): Unit = {
    println(s"Done. Winner: $winner")
  }

  override def failToMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    println(s"Could not move: $oldPos -> $newPos")
  }

  override def onError(e: Exception): Unit = {
    println("Could not parse your command. Retry?(Y|n) or show the error and exit (x)")
    scala.io.StdIn.readLine().toUpperCase() match {
      case "N" => sys.exit
      case "X" => {
        e.printStackTrace()
        sys.exit
      }
    }
  }

  start()
}
