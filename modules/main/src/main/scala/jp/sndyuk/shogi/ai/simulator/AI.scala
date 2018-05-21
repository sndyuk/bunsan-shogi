package jp.sndyuk.shogi.ai.simulator

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import jp.sndyuk.shogi.core.{ Board, State, Transition }

trait AI {
  val logger = Logger(LoggerFactory.getLogger(this.getClass().getName()))

  def next(board: Board, state: State): Transition
}
