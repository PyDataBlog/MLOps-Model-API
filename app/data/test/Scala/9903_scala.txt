/* https://www.hackerrank.com/challenges/a-very-big-sum */
package com.negrisoli.algorithms.warmup

object AVeryBigSum {
  
  def main(args: Array[String]) {
    val it = io.Source.stdin.getLines()
    val size = it.next().toInt
    println(it.next().split(" ").take(size).map(BigInt(_)).sum)
  }
  
}