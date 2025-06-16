package jp.sndyuk.shogi.core

object BitSet {
  def apply(nbits: Int): BitSet = new BitSet(nbits)()

  @inline val span = 5
}
case class BitSet(val length: Int)(private val bits: Array[Long] = Array.fill(length / 64 + 1)(0L)) {
  import BitSet._

  // (!) Do not modify value.
  private[core] def longArray: Array[Long] = bits

  @inline private def masks(i: Int): Long = ~((-1L << 64 - span) >>> i)

  @inline private def updateBits(l: Long, num: Long, i: Int): Long = {
    val cleared = l & masks(i)
    val shifted = num << (64 - (i + span))

    cleared | shifted
  }

  private val capacity = bits.length * 64

  @inline private def validateIndex(i: Int): Unit = {
    require(i >= 0 && i + span <= capacity,
      s"BitSet index $i out of bounds for capacity $capacity")
    assert((i % 64) + BitSet.span <= 64,
      s"BitSet index $i (local ${i % 64}) too high for span ${BitSet.span}")
  }

  def setInt(updates: Int, index: Int): Unit = {
    validateIndex(index)
    bits(index / 64) = updateBits(bits(index / 64), updates, index % 64)
  }

  def value(index: Int): Int = {
    validateIndex(index)
    (bits(index / 64) >>> (64 - (index % 64)) & 1).toInt
  }

  def intValue(i: Int): Int = {
    validateIndex(i)
    val l = bits(i / 64)
    ((l >>> (64 - ((i % 64) + span))) & ((1 << span) - 1)).toInt
  }

  def replaceIntValue(updates: Long, i: Int): Int = {
    validateIndex(i)
    val l = bits(i / 64)
    val index = i % 64
    val mask = masks(index)
    // mask   : 11111000001111...
    // l      : 11011011011101...
    val cleared = l & mask
    // cleared: 11010000011101...
    //              -----
    val old = ((~mask & l) >>> (64 - (index + span))).toInt
    if (updates == 0) {
      bits(i / 64) = cleared
      old
    } else {
      val shifted = updates << (64 - (index + span))
      // updates: ..000000000111
      // shifted: 00000011100000...
      // --------------------------
      // result : 11010011111101...
      bits(i / 64) = cleared | shifted
      old
    }
  }

  def clear(index: Int): Unit = {
    setInt(0, index)
  }

  def copy(): BitSet = {
  val newBitsArray = this.bits.clone()
    new BitSet(this.length)(newBitsArray)
  }

  override def toString(): String = {
    bits.foldLeft(new StringBuilder()) { (sb, c) => sb.append(f"${BigInt(c.toBinaryString)}%064d") }.toString
  }

  def id(): String = {
    bits.foldLeft(new StringBuilder()) { (sb, c) => sb.append(java.lang.Long.toString(c, 36)) }.toString
  }
}
