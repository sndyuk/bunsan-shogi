package jp.sndyuk.shogi.ai.learner

import java.io.File
import scala.io.Source
import scala.language.reflectiveCalls
import jp.sndyuk.shogi.algorithm.core.HashStorage
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.ID
import jp.sndyuk.shogi.core.KifuStrage
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.kifu.KI2Parser
import jp.sndyuk.shogi.kifu.Move
import scala.collection.mutable.HashMap
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import jp.sndyuk.shogi.kifu.Kifu
import jp.sndyuk.shogi.kifu.Kifu

object TransitionMaster extends App {

  HashStorage.start

  val files = new File("/Users/sndyuk/kifu/for_ai").listFiles.filter(_.getName.endsWith(".ki2"))
  for (file <- files) {
    println(s"Parse kifu: ${file.getAbsolutePath()}")
    val parseResult = KI2Parser.parse(Source.fromFile(file, "windows-31j").getLines)

    val kifu = Try(parseResult.get) match {
      case Success(kifu) => {
        val board = Board()
        var state = State()
        for (move <- kifu.moves) {
          move match {
            case Move(player, oldPos, newPos, piece, _) =>
              val id = ID(board)
              state = board.move(state, oldPos, newPos, true, !board.pieceOnBoard(oldPos).exists(Piece.isPromoted(_)) && Piece.isPromoted(piece))
              KifuStrage.nextTransitions.update(id, state.history.head, kifu.winner == state.turn.change)
          }
        }
      }
      case Failure(ex) => println(s"No result when parseing!!! ${file.getAbsolutePath()}", ex)
    }
  }
}
