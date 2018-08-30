package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.algorithm.core.HashDocument

object KifuStrage {

  val nextTransitions = new HashDocument[String, Array[Long]]("Transition") {
    def update(id: ID, transition: Transition, win: Boolean): Unit = {

      val currOpt = super.get(id.toString)
      super.put(id.toString, newValue(transition, win, currOpt))
    }

    def getEffectiveTransitions(id: ID, size: Int): List[Transition] = {
      val vOpt = super.get(id.toString)
      vOpt.map { v =>
        (v.toList.sortBy { l =>
          var winCount = (l & 268419072) >>> 14
          var looseCount = l & 16383
          looseCount.toDouble / (winCount + looseCount)
        }).take(size).map { l => Transition((l >>> 28).toInt) }
      }.getOrElse(Nil)
    }
  }

  private def newValue(transition: Transition, win: Boolean, currOpt: Option[Array[Long]]): Array[Long] = {
    val t = transition.toInt.toLong
    def newValue(): Long = (t << 28) | (if (win) (1 << 14) else 1)
    if (currOpt.isDefined) {
      val curr = currOpt.get
      for (i <- 0 until curr.length) {
        val l = curr(i)
        if ((l >>> 28) == t) {
          // Transition             Win            Loose
          // 1111111111111111111111 11111111111111 11111111111111
          // ---------------------- -------------- 	--------------
          var winCount = (l & 268419072) >>> 14
          var looseCount = l & 16383
          if (win) {
            winCount += 1
          } else {
            looseCount += 1
          }
          curr(i) = ((t << 28) | (winCount << 14) | looseCount)
          return curr
        }
      }
      Array(newValue(), curr: _*)
    } else {
      Array(newValue())
    }
  }
}
