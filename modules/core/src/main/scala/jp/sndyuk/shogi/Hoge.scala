package jp.sndyuk.shogi

object Hoge extends App {

  def isPrime(i: Int): Boolean = {
    val b = Math.floor(Math.pow(i, 0.5)).toInt
    for (j <- 2 to b) {
      if (i % j == 0) {
        return false
      }
    }
    true
  }

  for (i <- 0 to 100) {
    println(s"$i: ${isPrime(i)}")
  }
}
