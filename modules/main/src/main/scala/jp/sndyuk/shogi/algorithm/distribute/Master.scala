package jp.sndyuk.shogi.algorithm.distribute

import jp.sndyuk.shogi.algorithm.Algorithm
import akka.actor._
import Protocol._

class Master(resultHandler: ActorRef) extends Actor with ActorLogging {

  override def receive = {
    case ToMaster.Ready(worker) =>
    case unexpected => log.error("{}", unexpected)
  }
}