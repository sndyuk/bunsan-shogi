package jp.sndyuk.shogi.player

import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.ai.ShogiAI // Added import

class AIPlayer(
  val name: String,
  val turn: Turn,
  private val ai: ShogiAI,
  private val defaultSearchDepth: Int = 3
) extends Player { // Player trait does not have name/turn in constructor

  override def next(board: Board, state: State): Transition = {
    // Ensure AI plays for its assigned turn, even if state.turn might be different (e.g. for analysis)
    // However, for actual gameplay, state.turn should be equal to this.turn.
    if (this.turn != state.turn) {
        println(s"AIPlayer Warning: AI ($name) playing as ${this.turn} was asked to move for ${state.turn}'s turn.")
    }

    println(s"AIPlayer ($name, $turn) is thinking using ${ai.getClass.getSimpleName} with depth $defaultSearchDepth for turn ${state.turn}...")

    ai.findBestMove(state, board, this.turn, defaultSearchDepth) match {
      case (Some(move), _) => // Destructure tuple, ignore nodesVisited for return
        move
      case (None, _) => // Destructure tuple, ignore nodesVisited for return
        // This situation (AI returns None) should ideally be handled by the game logic
        // if it means no legal moves (checkmate/stalemate).
        // If it's an unexpected AI failure, throwing an exception is reasonable.
        throw new IllegalStateException(s"AI ${ai.getClass.getSimpleName} for player $name ($turn) could not find a move.")
    }
  }

  // Optional: override if AI does its own validation. Default is true in Player trait.
  // override def needValidation: Boolean = false

  override def toString(): String = s"AI($name, ${ai.getClass.getSimpleName})"
}
