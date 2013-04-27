package jp.sndyuk.shogi.algorithm.distribute

import jp.sndyuk.shogi.core._
import jp.sndyuk.shogi.algorithm.Algorithm

object Protocol {

  object ToMaster {
    case class Ready(computer: Computer)
  }

  object ToComputer {
    case class Compute(state: State)
  }
}