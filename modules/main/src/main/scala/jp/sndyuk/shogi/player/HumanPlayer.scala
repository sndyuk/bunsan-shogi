package jp.sndyuk.shogi.player

import java.io.BufferedReader
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import akka.actor.Actor
import scala.concurrent.duration._
import akka.util.Timeout
import akka.pattern.ask
import akka.actor.ActorRef

trait CommandReader {
  
  def read(state: State): Transition
}

class HumanPlayer(name: String, board: Board, reder: CommandReader, validation: Boolean) extends Player {

  implicit val timeout = Timeout(300 second)

  def next(board: Board, state: State): Transition = { 
    reder.read(state)
  }
  
  override def needValidation = validation

  override def toString(): String = name
}