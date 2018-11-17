package jp.sndyuk.shogi.core

import java.util.HashSet
import scala.reflect.ClassTag

case class FixedFifoArray[T: ClassTag](size: Int) {
  private var index = 0
  private val indexLimit = size - 2
  private val arr = Array.ofDim[T](size)
  private val set = new HashSet[T](size, 1)

  def add(elem: T) = {
    if (index < indexLimit) {
      index += 1
    } else {
      index = 0
      set.remove(elem)
    }
    arr.update(index, elem)
    set.add(elem)
  }

  def contains(elem: T): Boolean = set.contains(elem)
}
