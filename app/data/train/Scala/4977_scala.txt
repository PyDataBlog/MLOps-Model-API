package com.codechef.problems.atm

object Main {
  def main(args: Array[String]): Unit = {
    import java.io._
    val reader = new BufferedReader(new InputStreamReader(System.in))
    val strs = reader.readLine.split("\\s+")
    val amount = strs(0).toInt
    val total = strs(1).toDouble
    if(amount == 0) {
      printf("%0.2f\n", total)
    }else {
      if(amount % 5 == 0) {
        if(amount + 0.5 <= total) {
          printf("%.2f\n", total - amount - 0.5)
        }else {
          printf("%.2f\n", total)
        }
      }else {
        printf("%.2f\n", total)
      } 
    }
  }
}