package jp.sndyuk.shogi.core

object ID {
  def apply(board: Board): ID = {
    val bits = board.squares.bits.longArray
    val capture = board.capturedPieces.longValue
    new ID(bits(0), bits(1), bits(2), bits(3), bits(4), bits(5), bits(6), capture)
  }
}

class ID(
  // v1〜v7は後半4bit未使用
  // v8は後半16bit未使用
  // 全 468bit = 512 - ((4 * 7) + 16)
  private val v1: Long, // squares.bits(0)
  private val v2: Long, // squares.bits(1)
  private val v3: Long, // ...
  private val v4: Long, // ...
  private val v5: Long, // ...
  private val v6: Long, // ...
  private val v7: Long, // squares.bits(6)
  private val v8: Long // capturedPieces
) {

  lazy val hashLong: Long = v1 ^ v2 ^ v3 ^ v4 ^ v5 ^ v6 ^ v7 ^ v8

  override lazy val hashCode: Int = (hashLong ^ (hashLong >>> 32)).toInt

  override def equals(o: Any): Boolean = {
    val oid = o.asInstanceOf[ID]
    oid.v8 == v8 &&
      oid.v1 == v1 &&
      oid.v2 == v2 &&
      oid.v3 == v3 &&
      oid.v4 == v4 &&
      oid.v5 == v5 &&
      oid.v6 == v6 &&
      oid.v7 == v7
  }

  override lazy val toString = hashLong.toHexString
}
