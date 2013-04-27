package jp.sndyuk.shogi.core

object BitSet {
  def apply(nbits: Int): BitSet = new BitSet(nbits)()
}
case class BitSet(nbits: Int)(private val bits: Array[Long] = Array.fill(nbits / 64 + 1)(0L)) {
  val length = nbits
  def set(index: Int, value: Boolean): Unit = {
    //     println(s"$index, ${(1L << 64 - (index % 64)).toBinaryString}")
    if (value) {
      bits(index / 64) |= (1L << 64 - (index % 64))
    } else {
      bits(index / 64) &= ~(1L << 64 - (index % 64))
    }
  }

  def value(index: Int): Boolean = {
    (bits(index / 64) >> (64 - (index % 64)) & 1) == 1
  }

  def intValue(pos: Int, span: Int): Int = {
    assert(span <= 32)
    (0 to span - 1).foldLeft(0) { (v, i) => v | ((if (value(pos + i)) 1 else 0) << i) }
  }

  def clear(pos: Int, span: Int): Unit = (0 to span - 1).foreach(i => set(pos + i, false))

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
