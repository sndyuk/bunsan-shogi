package jp.sndyuk.shogi.core

object BitSet {
  def apply(nbits: Int): BitSet = new BitSet(nbits)()
}
case class BitSet(nbits: Int)(private val bits: Array[Long] = Array.fill(nbits / 64 + 1)(0L)) {
  val length = nbits

  // value: [0 | 1 ]
  @inline private def set(index: Int, value: Int): Unit = {
    //     println(s"$index, ${(1L << 64 - (index % 64)).toBinaryString}")
    if (value == 0) {
      bits(index / 64) &= ~(1L << (64 - (index % 64)))
    } else {
      bits(index / 64) |= 1L << (64 - (index % 64))
    }
  }

  private def updateBits(l: Long, num: Long, i: Int, span: Int): Long = {
    val mask = if (i == 0) {
      (1L << 64 - (i + span)) - 1
    } else {
      val left = ~0L << (64 - i)
      val right = (1L << 64 - (i + span)) - 1
      left | right
    }
    val cleared = l & mask
    val shifted = num << (64 - (i + span))

    cleared | shifted
  }

  def setInt(updates: Int, index: Int, span: Int): Unit = {
    assert((index % 64 + span) < 64, index) // 複数のLongに跨がらないこと
    bits(index / 64) = updateBits(bits(index / 64), updates, index % 64, span)
  }

  def value(index: Int): Int = {
    (bits(index / 64) >> (64 - (index % 64)) & 1).toInt
  }

  def intValue(i: Int, span: Int): Int = {
    assert(span <= 31)
    assert((i % 64 + span) < 64) // 複数のLongに跨がらないこと
    val l = bits(i / 64)
    ((l >>> (64 - ((i % 64) + span))) & ((1 << span) - 1)).toInt
  }

  def clear(index: Int, span: Int): Unit = {
    assert((index % 64 + span) < 64) // 複数のLongに跨がらないこと
    setInt(0, index, span)
  }

  def copy(): BitSet = {
    new BitSet(nbits)(bits.clone())
  }

  override def toString(): String = {
    bits.foldLeft(new StringBuilder()) { (sb, c) => sb.append(f"${BigInt(c.toBinaryString)}%064d") }.toString
  }

  def id(): String = {
    bits.foldLeft(new StringBuilder()) { (sb, c) => sb.append(java.lang.Long.toString(c, 36)) }.toString
  }
}
