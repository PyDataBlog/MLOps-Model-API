package chapter05

object BitwiseOperations {
  def main(args: Array[String]): Unit = {
    println("Bitwise AND Of 1&2 : " + (1 & 2))
    println("Bitwise OR Of 1&2 : " + (1 | 2))
    println("Bitwise XOR Of 1&2 : " + (1 ^ 2))

    //>>>31 has to be added as the no is always expressed in 32 bits
    println("Unary Bitwise complement of 6 : " + (~6 >>> 31))
  }
}
