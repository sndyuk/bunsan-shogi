package jp.sndyuk.shogi.ai

import jp.sndyuk.shogi.core.Block
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Rule
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Piece._
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.Point
import java.util.concurrent.ConcurrentHashMap
import jp.sndyuk.shogi.core.PlayerA

object Utils {

//  private val planCache = new ConcurrentHashMap[String, List[Transition]]()

  def plans(board: Board, state: State): List[Transition] = {
//    val currId = board.id() + (if (state.turn == PlayerA) "0" else "1")
//    val cache = planCache.get(currId)
//    if (cache != null) {
//      cache
//    } else {
      val pieces = board.allMovablePieces(state.turn)

      val plans = pieces.flatMap {
        case Block(point, piece) =>
          val nari = if (Piece.isPromoted(piece)) board.pieceOnBoard(point).map { !Piece.isPromoted(_) }.getOrElse(false) else false
          Rule.generateMovablePointsWithPromote(board, point, piece, state.turn).map { case (a, b) => Transition(point, a, b, None) }
      }
//      planCache.put(currId, plans)
      plans
//    }
  }

  def isEffectiveMove(transition: Transition, state: State, board: Board): Boolean = {
    if (Rule.isThreefoldRepetition(board, state)) {
      return false
    }

    val piece = board.piece(transition.oldPos, state.turn)
    val gpiece = generalize(piece)
    val oldPos = transition.oldPos
    val newPos = transition.newPos

    // 成れるのに成らない歩、角、飛、香は不要
    if (!transition.nari && (gpiece == ◯.FU || gpiece == ◯.KA || gpiece == ◯.HI || gpiece == ◯.KY) && Rule.canBePromoted(board, oldPos, newPos, piece)) {
      return false
    }

    val around = for {
      x <- newPos.x - 1 to newPos.x + 1
      y <- newPos.y - 1 to newPos.y + 1
      if x <= 8 && y <= 8 && x >= 0 && y >= 0 && !(x == newPos.x && y == newPos.y)
      p <- board.pieceOnBoardNotEmpty(Point(y, x))
    } yield p

    // 飛ばない駒(歩、銀、金)の周り8マスに駒があること
    if ((gpiece == ◯.FU || gpiece == ◯.GI || gpiece == ◯.KI) && around.isEmpty) {
      return false
    }

    // 王の周り8マスに自駒があること
    if ((gpiece == ◯.OU) && !around.exists(▲△(_, state.turn))) {
      return false
    }

    // 王が取られる位置に移動しないこと
    //    if (isCaptureOuAtNextTurn(transition, state, board)) {
    //      return false
    //    }

    true
  }

  private def isCaptureOuAtNextTurn(transition: Transition, state: State, board: Board): Boolean = {
    val nextState = board.move(state, transition.oldPos, transition.newPos, false, transition.nari)
    val nextTransitions = plans(board, nextState)
    val result = findTransitionCaputuringOu(nextTransitions, nextState, board).isDefined
    board.rollback(nextState)
    result
  }

  def findTransitionCaputuringOu(xs: Seq[Transition], state: State, board: Board): Option[Transition] = xs.find(isCaptureOu(_, state, board))
  def isCaptureOu(transition: Transition, state: State, board: Board): Boolean = {
    val nextState = board.move(state, transition.oldPos, transition.newPos, false, transition.nari)
    // Get OU if the transition can capture it.
    if (board.isFinish(state.turn)) {
      board.rollback(nextState)
      true
    } else {
      board.rollback(nextState)
      false
    }
  }
}
