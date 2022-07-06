package chapter05

object RichOperations {
  def main(args: Array[String]): Unit = {
    println("Max in a range 0 - 10 : " + (0 max 10))
    println("Min in a range 0 - 10 : " + (0 min 10))
    println("Absolute value of -2.7 : " + (-2.7 abs))
    println("Rounded value of 2.7 : " + (2.7 round))
    println("Capitalize bob : " + ("bob" capitalize))
    println("Dropping 2 letters in robert : " + ("robert" drop 2))
  }
}
