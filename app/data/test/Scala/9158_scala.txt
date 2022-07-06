/**
  * Created by akorovin on 21.12.2016.
  */
object TestFunc {
  def main(args: Array[String]): Unit = {
    // typical structure of function
    /**
      * def funcName (param1:dataType, param2:dataType) : returnType {
      *    function body
      *    return valueToReturn
      * }
      */

    /**
      * function
      * @param num1 parameter with type int, default value 1
      * @param num2
      * @return return type is int
      */
    def getSum(num1: Int = 1, num2: Int = 2) : Int = {
      // NOTE: keyword return is redundant
      // return num1 + num2
      num1 + num2
    }

    println("5 + 10: " + getSum(5, 10))

    // void function example. should return Unit
    def sayHi(): Unit = {
      println("hello")
    }

    sayHi()

    // multiple of integers as parameter
    def getSumMultipleParams(args: Int*): Int = {
      var sum : Int = 0
      for (num <- args)
        sum += num
      // return value sum
      sum
    }

    println("1 + 2 + 3 + 4 + 5: " + getSumMultipleParams(1,2,3,4,5))

    // recursion
    def factorial(num : BigInt): BigInt = {
      if (num <= 1)
        1
      else
        num * factorial(num - 1)
    }

    println("factorial of 4: " + factorial(4))
  }
}
