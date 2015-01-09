package jp.sndyuk.shogi.core

object Piece {

  val maxPieceSize = 28
  val maxGeneralizedPieceSize = 8
  val ❏ = 0

  def generalize(piece: Piece): Piece = piece match {
    case ❏ => piece
    case ▲.OU | △.OU => ◯.OU
    case _ => piece & 7 | 32
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
    if ((piece >= ▲.FU && piece <= ▲.KY) || (piece >= △.FU && piece <= △.KY)) piece | 8
    else piece
  }

  @inline def isPromoted(piece: Piece): Boolean = (piece & 8) == 8

  // 先手
  @inline def ▲(piece: Piece): Boolean = (piece & 16) == 0

  // 後手
  @inline def △(piece: Piece): Boolean = (piece & 16) == 16

  // 後手
  @inline def ▲△(piece: Piece, turn: Turn): Boolean = piece != ❏ && turn == PlayerA ^ △(piece)

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
      case ▲.OU | △.OU | ◯.OU => "玉"
      case ▲.FU | △.FU | ◯.FU => "歩"
      case ▲.KI | △.KI | ◯.KI => "金"
      case ▲.GI | △.GI | ◯.GI => "銀"
      case ▲.HI | △.HI | ◯.HI => "飛"
      case ▲.KA | △.KA | ◯.KA => "角"
      case ▲.KE | △.KE | ◯.KE => "桂"
      case ▲.KY | △.KY | ◯.KY => "香"
      case ▲.TO | △.TO => "と"
      case ▲.NG | △.NG => "全"
      case ▲.RY | △.RY => "龍"
      case ▲.UM | △.UM => "馬"
      case ▲.NK | △.NK => "圭" // 成桂
      case ▲.NY | △.NY => "杏" // 成香
    }
  }

  // 左から6bit目がtrue
  object ◯ {
    @inline val OU = Integer.parseInt("101000", 2)
    @inline val KI = Integer.parseInt("100001", 2)
    @inline val FU = Integer.parseInt("100010", 2)
    @inline val GI = Integer.parseInt("100011", 2)
    @inline val HI = Integer.parseInt("100100", 2)
    @inline val KA = Integer.parseInt("100101", 2)
    @inline val KE = Integer.parseInt("100110", 2)
    @inline val KY = Integer.parseInt("100111", 2)
    val all = Seq(OU, FU, KI, GI, HI, KA, KE, KY)
  }

  // 左から5bit目がfalse
  object ▲ {
    @inline val OU = Integer.parseInt("1000", 2)
    @inline val KI = Integer.parseInt("0001", 2)
    @inline val FU = Integer.parseInt("0010", 2)
    @inline val GI = Integer.parseInt("0011", 2)
    @inline val HI = Integer.parseInt("0100", 2)
    @inline val KA = Integer.parseInt("0101", 2)
    @inline val KE = Integer.parseInt("0110", 2)
    @inline val KY = Integer.parseInt("0111", 2)

    @inline val TO = FU | Integer.parseInt("1000", 2)
    @inline val NG = GI | Integer.parseInt("1000", 2)
    @inline val RY = HI | Integer.parseInt("1000", 2)
    @inline val UM = KA | Integer.parseInt("1000", 2)
    @inline val NK = KE | Integer.parseInt("1000", 2)
    @inline val NY = KY | Integer.parseInt("1000", 2)
  }

  // 左から5bit目がtrue
  object △ {
    @inline val OU = Integer.parseInt("11000", 2)
    @inline val KI = Integer.parseInt("10001", 2)
    @inline val FU = Integer.parseInt("10010", 2)
    @inline val GI = Integer.parseInt("10011", 2)
    @inline val HI = Integer.parseInt("10100", 2)
    @inline val KA = Integer.parseInt("10101", 2)
    @inline val KE = Integer.parseInt("10110", 2)
    @inline val KY = Integer.parseInt("10111", 2)

    @inline val TO = FU | Integer.parseInt("1000", 2)
    @inline val NG = GI | Integer.parseInt("1000", 2)
    @inline val RY = HI | Integer.parseInt("1000", 2)
    @inline val UM = KA | Integer.parseInt("1000", 2)
    @inline val NK = KE | Integer.parseInt("1000", 2)
    @inline val NY = KY | Integer.parseInt("1000", 2)
  }

  def invert(piece: Piece, turn: Turn): Piece = {
    if (piece == ❏) piece
    else if (turn == PlayerA) {
      piece & 15 // & 1111
    } else {
      piece & 15 | 16 // & 1111 | 10000
    }
  }

  def turned(piece: Piece): Piece = {
    val gpiece = generalize(piece)
    if (▲(piece)) invert(gpiece, PlayerB)
    else invert(gpiece, PlayerA)
  }
}
