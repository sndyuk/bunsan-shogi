package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core._
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor._
import scala.util.{ Success, Failure }
import scala.collection.parallel.mutable.ParTrieMap
import scala.collection.mutable.Queue
import scala.annotation.tailrec
import com.twitter.concurrent.Offer
import com.twitter.concurrent.Broker
import scala.util.Try
import scala.collection.parallel.ParSeq
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait AI {
  val logger = Logger(LoggerFactory.getLogger(this.getClass().getName()))

  def next(board: Board, state: State): Transition
}
