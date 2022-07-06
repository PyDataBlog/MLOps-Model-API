package com.chyuck.algorithms.scala

object SelectionSort {

  def sort(array: Array[Int]) : Array[Int] = {
    if (array.length <= 1) array
    else {

      for (i <- 0 until array.length) {

        var minValueIndex = i

        for (j <- i + 1 until array.length)
          if (array(j) < array(minValueIndex))
            minValueIndex = j

        val temp = array(i)
        array(i) = array(minValueIndex)
        array(minValueIndex) = temp
      }

      array
    }
  }

}
