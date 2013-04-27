package jp.sndyuk.shogi

import com.typesafe.config.ConfigFactory
import scala.language.implicitConversions
import java.io.File

package object core {

  implicit def toPoint(p: (Int, Int)) = Point(p._1, p._2)
  type Piece = Int

  val config = ConfigFactory.parseResources("application.conf").resolve()
}