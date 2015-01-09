package jp.sndyuk.shogi

import com.typesafe.config.ConfigFactory
import scala.language.implicitConversions
import java.io.File

package object core {

  implicit def toPoint(p: (Int, Int)) = Point(p._1, p._2)
  type Piece = Int

  type Move = (Point, Boolean)

  trait Turn {
    def change: Turn
  }

  // 先手
  case object PlayerA extends Turn {
    def change = PlayerB
    override def toString() = "▲"
  }

  // 後手
  case object PlayerB extends Turn {
    def change = PlayerA
    override def toString() = "△"
  }
  case class Transition(oldPos: Point, newPos: Point, nari: Boolean, captured: Option[Piece]) extends Serializable {
    override def toString(): String = {
      s"$oldPos->$newPos${if (nari) " 成" else ""}"
    }
  }

  val config = ConfigFactory.parseResources("application.conf").resolve()
}