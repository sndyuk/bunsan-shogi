package jp.sndyuk.shogi

import scala.language.implicitConversions

import com.typesafe.config.ConfigFactory

package object core {

  implicit def toPoint(p: (Int, Int)) = Point(p._1, p._2)
  type Piece = Int

  type Move = (Point, Boolean)

  val config = ConfigFactory.parseResources("application.conf").resolve()
}
