package org.precompiler.scala101.ch05

/**
  * Created by RL on 8/22/17.
  */
object StringUtils {

  implicit class StringMe(val target: String) {
    def x(t: Int): String = {
      var ret = ""
      for (i <- 0 to t - 1) {
        ret = ret + target
      }
      ret
    }
  }

}


