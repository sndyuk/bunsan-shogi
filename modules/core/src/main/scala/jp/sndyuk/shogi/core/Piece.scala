package jp.sndyuk.shogi.core

object Piece {

  @inline val maxPieceSize = 28
  @inline val maxGeneralizedPieceSize = 8
  @inline val ❏ = 0
  @inline val bitsPiece = Integer.parseInt("1111", 2);
  @inline val bitsPromotedPiece = Integer.parseInt("1000", 2);
  @inline val bitsPieceWithoutPromoted = bitsPiece ^ bitsPromotedPiece;
  @inline val bitsPlayerBPiece = Integer.parseInt("10000", 2);
  @inline val bitsGeneralPiece = Integer.parseInt("100000", 2);

  @inline def generalize(piece: Piece): Piece = piece match {
    case ❏ => piece
    case ▲.OU | △.OU => ◯.OU
    case _ => piece & bitsPieceWithoutPromoted | bitsGeneralPiece
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

  // Promote it. If it can't be promoted, returns the original piece.
  @inline def promote(piece: Piece): Int = {
    if ((piece >= ▲.FU && piece <= ▲.KY) || (piece >= △.FU && piece <= △.KY)) piece | bitsPromotedPiece
    else piece
  }

  @inline def isPromoted(piece: Piece): Boolean = (piece & bitsPromotedPiece) != 0

  // 先手
  @inline def ▲(piece: Piece): Boolean = ! △(piece)

  // 後手
  @inline def △(piece: Piece): Boolean = (piece & bitsPlayerBPiece) != 0

  @inline def ▲△(piece: Piece, turn: Turn): Boolean = piece != ❏ && turn == PlayerA ^ △(piece)

  def pieceString(piece: Piece): String = {
    if (piece == ❏) {
      "　　"
    } else if (▲(piece)) {
      s"△${name(piece)}"
    } else {
      s"▽${name(piece)}"
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

  object ▲ {
    @inline val OU = Integer.parseInt("1000", 2)
    @inline val KI = Integer.parseInt("0001", 2)
    @inline val FU = Integer.parseInt("0010", 2)
    @inline val GI = Integer.parseInt("0011", 2)
    @inline val HI = Integer.parseInt("0100", 2)
    @inline val KA = Integer.parseInt("0101", 2)
    @inline val KE = Integer.parseInt("0110", 2)
    @inline val KY = Integer.parseInt("0111", 2)

    @inline val TO = FU | bitsPromotedPiece
    @inline val NG = GI | bitsPromotedPiece
    @inline val RY = HI | bitsPromotedPiece
    @inline val UM = KA | bitsPromotedPiece
    @inline val NK = KE | bitsPromotedPiece
    @inline val NY = KY | bitsPromotedPiece
  }

  object △ {
    @inline val OU = ▲.OU | bitsPlayerBPiece
    @inline val KI = ▲.KI | bitsPlayerBPiece
    @inline val FU = ▲.FU | bitsPlayerBPiece
    @inline val GI = ▲.GI | bitsPlayerBPiece
    @inline val HI = ▲.HI | bitsPlayerBPiece
    @inline val KA = ▲.KA | bitsPlayerBPiece
    @inline val KE = ▲.KE | bitsPlayerBPiece
    @inline val KY = ▲.KY | bitsPlayerBPiece

    @inline val TO = FU | bitsPromotedPiece
    @inline val NG = GI | bitsPromotedPiece
    @inline val RY = HI | bitsPromotedPiece
    @inline val UM = KA | bitsPromotedPiece
    @inline val NK = KE | bitsPromotedPiece
    @inline val NY = KY | bitsPromotedPiece
  }

  object ◯ {
    @inline val OU = ▲.OU | bitsGeneralPiece
    @inline val KI = ▲.KI | bitsGeneralPiece
    @inline val FU = ▲.FU | bitsGeneralPiece
    @inline val GI = ▲.GI | bitsGeneralPiece
    @inline val HI = ▲.HI | bitsGeneralPiece
    @inline val KA = ▲.KA | bitsGeneralPiece
    @inline val KE = ▲.KE | bitsGeneralPiece
    @inline val KY = ▲.KY | bitsGeneralPiece
    val all = Seq(OU, FU, KI, GI, HI, KA, KE, KY)
  }

  def convert(piece: Piece, turn: Turn): Piece = {
    if (piece == ❏) piece
    else if (turn == PlayerA) {
      piece & bitsPiece
    } else {
      piece & bitsPiece | bitsPlayerBPiece
    }
  }

  def turned(piece: Piece): Piece = {
    if (▲(piece)) convert(piece, PlayerB)
    else convert(piece, PlayerA)
  }
}
