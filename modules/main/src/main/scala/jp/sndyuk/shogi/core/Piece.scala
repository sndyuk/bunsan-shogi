package jp.sndyuk.shogi.core

object Piece {

  val maxPieceSize = 28
  val maxGeneralizedPieceSize = 8
  val ❏ = 0

  val pieceMask = (1 << maxPieceSize + 1) - 1

  def pieceIndex(piece: Piece): Int = {
    piece match {
      case ❏ => 0
      case ▲.OU => 0
      case ▲.FU => 1
      case ▲.KI => 2
      case ▲.GI => 3
      case ▲.HI => 4
      case ▲.KA => 5
      case ▲.KE => 6
      case ▲.KY => 7
      case ▲.TO => 8
      case ▲.NG => 9
      case ▲.RY => 10
      case ▲.UM => 11
      case ▲.NK => 12
      case ▲.NY => 13

      case △.OU => 14
      case △.FU => 15
      case △.KI => 16
      case △.GI => 17
      case △.HI => 18
      case △.KA => 19
      case △.KE => 20
      case △.KY => 21
      case △.TO => 22
      case △.NG => 23
      case △.RY => 24
      case △.UM => 25
      case △.NK => 26
      case △.NY => 27
    }
  }

  def generalize(piece: Piece): Int = {
    piece match {
      case ❏ => ❏
      case ▲.OU => ◯.OU
      case ▲.FU => ◯.FU
      case ▲.KI => ◯.KI
      case ▲.GI => ◯.GI
      case ▲.HI => ◯.HI
      case ▲.KA => ◯.KA
      case ▲.KE => ◯.KE
      case ▲.KY => ◯.KY
      case ▲.TO => ◯.FU
      case ▲.NG => ◯.GI
      case ▲.RY => ◯.HI
      case ▲.UM => ◯.KA
      case ▲.NK => ◯.KE
      case ▲.NY => ◯.KY

      case △.OU => ◯.OU
      case △.FU => ◯.FU
      case △.KI => ◯.KI
      case △.GI => ◯.GI
      case △.HI => ◯.HI
      case △.KA => ◯.KA
      case △.KE => ◯.KE
      case △.KY => ◯.KY
      case △.TO => ◯.FU
      case △.NG => ◯.GI
      case △.RY => ◯.HI
      case △.UM => ◯.KA
      case △.NK => ◯.KE
      case △.NY => ◯.KY

      case _ => piece
    }
  }

  def reverseIfPromoted(piece: Piece): Piece = {
    piece match {
      case ❏ => ❏
      case ▲.TO => ▲.FU
      case ▲.NG => ▲.GI
      case ▲.RY => ▲.HI
      case ▲.UM => ▲.KA
      case ▲.NK => ▲.KE
      case ▲.NY => ▲.KY

      case △.TO => △.FU
      case △.NG => △.GI
      case △.RY => △.HI
      case △.UM => △.KA
      case △.NK => △.KE
      case △.NY => △.KY

      case _ => piece
    }
  }

  /**
   * 成る。成れなかったらそのまま返す。
   */
  def toBePromoted(piece: Piece): Int = {
    piece match {
      case ❏ => ❏
      case ▲.FU => ▲.TO
      case ▲.GI => ▲.NG
      case ▲.HI => ▲.RY
      case ▲.KA => ▲.UM
      case ▲.KE => ▲.NK
      case ▲.KY => ▲.NY

      case △.FU => △.TO
      case △.GI => △.NG
      case △.HI => △.RY
      case △.KA => △.UM
      case △.KE => △.NK
      case △.KY => △.NY

      case _ => piece
    }
  }

  def isPromoted(piece: Piece): Boolean = {
    piece match {
      case ▲.TO |
        ▲.NG |
        ▲.RY |
        ▲.UM |
        ▲.NK |
        ▲.NY |
        △.TO |
        △.NG |
        △.RY |
        △.UM |
        △.NK |
        △.NY => true
      case _ => false
    }
  }

  // 先手
  def ▲(piece: Piece): Boolean = {
    piece match {
      case ▲.OU => true
      case ▲.FU => true
      case ▲.KI => true
      case ▲.GI => true
      case ▲.HI => true
      case ▲.KA => true
      case ▲.KE => true
      case ▲.KY => true
      case ▲.TO => true
      case ▲.NG => true
      case ▲.RY => true
      case ▲.UM => true
      case ▲.NK => true
      case ▲.NY => true
      case _ => false
    }
  }
  // 後手
  def △(piece: Piece): Boolean = {
    piece match {
      case △.OU => true
      case △.FU => true
      case △.KI => true
      case △.GI => true
      case △.HI => true
      case △.KA => true
      case △.KE => true
      case △.KY => true
      case △.TO => true
      case △.NG => true
      case △.RY => true
      case △.UM => true
      case △.NK => true
      case △.NY => true
      case _ => false
    }
  }

  // 後手
  def ▲△(piece: Piece, turn: Turn): Boolean = {
    if (turn == PlayerA)  ▲(piece) else △(piece)
  }

  def pieceString(piece: Piece): String = {
    if (piece == ❏) {
      "　　"
    } else if (▲(piece)) {
      s"▲${name(piece)}"
    } else {
      s"△${name(piece)}"
    }
  }

  def name(piece: Piece): String = {
    piece match {
      case ❏ => ""
      case ▲.OU | △.OU => "玉"
      case ▲.FU | △.FU => "歩"
      case ▲.KI | △.KI => "金"
      case ▲.GI | △.GI => "銀"
      case ▲.HI | △.HI => "飛"
      case ▲.KA | △.KA => "角"
      case ▲.KE | △.KE => "桂"
      case ▲.KY | △.KY => "香"
      case ▲.TO | △.TO => "と"
      case ▲.NG | △.NG => "全"
      case ▲.RY | △.RY => "龍"
      case ▲.UM | △.UM => "馬"
      case ▲.NK | △.NK => "圭" // 成桂
      case ▲.NY | △.NY => "杏" // 成香
    }
  }

  object ◯ {
    val OU = 1
    val FU = 1 << 1 // 2
    val KI = 1 << 2 // 4
    val GI = 1 << 3 // 8
    val HI = 1 << 4 // 16
    val KA = 1 << 5 // 32
    val KE = 1 << 6 // 64
    val KY = 1 << 7 // 128

    val all = Seq(OU, FU, KI, GI, HI, KA, KE, KY)
  }

  object ▲ {
    val OU = 1
    val FU = 1 << 1 // 2
    val KI = 1 << 2 // 4
    val GI = 1 << 3 // 8
    val HI = 1 << 4 // 16
    val KA = 1 << 5 // 32
    val KE = 1 << 6 // 64
    val KY = 1 << 7 // 128

    val TO = 1 << 8 // 256
    val NG = 1 << 9 // 512
    val RY = 1 << 10 // 1024
    val UM = 1 << 11 // 2048
    val NK = 1 << 12 // 4096
    val NY = 1 << 13 // 8192
  }

  object △ {
    val OU = 1 << 14 // 16384
    val FU = 1 << 15 // 32768
    val KI = 1 << 16 // 65536
    val GI = 1 << 17 // 131072
    val HI = 1 << 18 // 262144
    val KA = 1 << 19 // 524288
    val KE = 1 << 20 // 1048576
    val KY = 1 << 21 // 2097152

    val TO = 1 << 22 // 4104304
    val NG = 1 << 23 // 8388608
    val RY = 1 << 24 // 16777216
    val UM = 1 << 25 // 33554432
    val NK = 1 << 26 // 67108864
    val NY = 1 << 27 // 134217728
  }

  def invert(piece: Piece, turn: Turn): Piece = {
    if (turn == PlayerA) {
      piece match {
        case ❏ => ❏
        case ◯.OU => ▲.OU
        case ◯.FU => ▲.FU
        case ◯.KI => ▲.KI
        case ◯.GI => ▲.GI
        case ◯.HI => ▲.HI
        case ◯.KA => ▲.KA
        case ◯.KE => ▲.KE
        case ◯.KY => ▲.KY
      }
    } else {
      piece match {
        case ❏ => ❏
        case ◯.OU => △.OU
        case ◯.FU => △.FU
        case ◯.KI => △.KI
        case ◯.GI => △.GI
        case ◯.HI => △.HI
        case ◯.KA => △.KA
        case ◯.KE => △.KE
        case ◯.KY => △.KY
      }
    }
  }

  def turned(piece: Piece): Piece = {
    val gpiece = generalize(piece)
    if (▲(piece)) invert(gpiece, PlayerB)
    else invert(gpiece, PlayerA)
  }
}
