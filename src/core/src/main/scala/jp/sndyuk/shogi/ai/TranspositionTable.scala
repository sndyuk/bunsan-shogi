package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.Transition

import scala.collection.mutable

/** Simple transposition table used by AlphaBetaAI_V4 to cache evaluated states. */
object TranspositionTable {
  case class Entry(value: Int, depth: Int, bestMove: Option[Transition])

  private val table = mutable.HashMap.empty[Long, Entry]

  def get(hash: Long, depth: Int): Option[Entry] = {
    table.get(hash).filter(_.depth >= depth)
  }

  def put(hash: Long, value: Int, depth: Int, bestMove: Option[Transition]): Unit = {
    table.get(hash) match {
      case Some(e) if e.depth > depth => // Keep deeper entry
      case _ => table.update(hash, Entry(value, depth, bestMove))
    }
  }

  def clear(): Unit = table.clear()
}
