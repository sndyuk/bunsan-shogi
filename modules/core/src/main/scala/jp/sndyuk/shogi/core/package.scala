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

  object Transition {
    def apply(v: Int): Transition = {
      Transition(Point((v & 960) >>> 6, (v & 15360) >>> 10), Point((v & 245760) >>> 14, (v & 3932160) >>> 18), ((v & 32) >>> 5) == 1, if ((v & 31) > 0) Some(v & 31) else None)
    }
  }

  case class Transition(oldPos: Point, newPos: Point, nari: Boolean, captured: Option[Piece]) extends Serializable {
    override def toString(): String = {
      s"$oldPos->$newPos${if (nari) " 成" else ""}"
    }

    // 22bit
    def toInt(): Int = {
      // 1111 1111 1111 1111 1 11111
      newPos.x << 18 | newPos.y << 14 | oldPos.x << 10 | oldPos.y << 6 | (if (nari) 1 << 5 else 0) | captured.getOrElse(0)
    }
  }

  val config = ConfigFactory.parseResources("application.conf").resolve()
}