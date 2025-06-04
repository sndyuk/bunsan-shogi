package jp.sndyuk.shogi.core

object ID {
  def apply(board: Board): ID = {
    val bits = board.squares.bits.longArray
    val capture = board.capturedPieces.longValue
    // Instantiate the case class
    ID(bits(0), bits(1), bits(2), bits(3), bits(4), bits(5), bits(6), capture)
  }
}

// v1〜v7は後半4bit未使用
// v8は後半16bit未使用
// 全 468bit = 512 - ((4 * 7) + 16)
case class ID(
  v1: Long, // squares.bits(0)
  v2: Long, // squares.bits(1)
  v3: Long, // ...
  v4: Long, // ...
  v5: Long, // ...
  v6: Long, // ...
  v7: Long, // squares.bits(6)
  v8: Long // capturedPieces
) {

  def hashLong: Long = v1 ^ v2 ^ v3 ^ v4 ^ v5 ^ v6 ^ v7 ^ v8

  override def toString: String = hashLong.toHexString
}
