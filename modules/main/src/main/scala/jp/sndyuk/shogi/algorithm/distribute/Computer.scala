package jp.sndyuk.shogi.algorithm.distribute

import akka.actor._
import jp.sndyuk.shogi.core._
import Protocol._

class Computer(board: Board, master: ActorRef) extends Actor with ActorLogging {
  
  override def receive = {
    case ToComputer.Compute(state) =>
    case unexpected => log.error("{}", unexpected)
  }
}