package jp.sndyuk.shogi.ai.simulator

import com.typesafe.scalalogging.Logger

import jp.sndyuk.shogi.core.{ Board, State, Transition }
import org.slf4j.LoggerFactory

trait AI {
  val logger = Logger(LoggerFactory.getLogger(this.getClass().getName()))

  def next(board: Board, state: State): Transition
}
