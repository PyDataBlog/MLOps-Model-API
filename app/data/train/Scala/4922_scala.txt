package com.codingkapoor.codingbat

import org.scalatest.{FlatSpec, Matchers}

class FunctionalIISpec extends FlatSpec with Matchers {

  """Given a list of integers, method "noNeg"""" should """return a list of the integers, omitting any that are less than 0.""" in {
    FunctionalII.noNeg(List(1, -2)) should equal(List(1))
    FunctionalII.noNeg(List(-3, -3, 3, 3)) should equal(List(3, 3))
    FunctionalII.noNeg(List(-1, -1, -1)) should equal(List())
  }

  """Given a list of non-negative integers, method "no9"""" should """return a list of those numbers except omitting any that end with 9.""" in {
    FunctionalII.no9(List(1, 2, 19)) should equal(List(1, 2))
    FunctionalII.no9(List(9, 19, 29, 3)) should equal(List(3))
    FunctionalII.no9(List(1, 2, 3)) should equal(List(1, 2, 3))
  }

  """Given a list of integers, method "noTeen"""" should """return a list of those numbers, omitting any that are between 13 and 19 inclusive.""" in {
    FunctionalII.noTeen(List(12, 13, 19, 20)) should equal(List(12, 20))
    FunctionalII.noTeen(List(1, 14, 1)) should equal(List(1, 1))
    FunctionalII.noTeen(List(15)) should equal(List())
  }

  """Given a list of strings, method "noLong"""" should """return a list of the strings, omitting any string length 4 or more.""" in {
    FunctionalII.noLong(List("this", "not", "too", "long")) should equal(List("not", "too"))
    FunctionalII.noLong(List("a", "bbb", "cccc")) should equal(List("a", "bbb"))
    FunctionalII.noLong(List("cccc", "cccc", "cccc")) should equal(List())
  }

  """Given a list of strings, method "noZ"""" should """return a list of the strings, omitting any string that contains a "z".""" in {
    FunctionalII.noZ(List("aaa", "bbb", "aza")) should equal(List("aaa", "bbb"))
    FunctionalII.noZ(List("hziz", "hzello", "hi")) should equal(List("hi"))
    FunctionalII.noZ(List("hello", "howz", "are", "youz")) should equal(List("hello", "are"))
  }

  """Given a list of strings, method "no34"""" should """return a list of the strings, omitting any string length 3 or 4.""" in {
    FunctionalII.no34(List("a", "bb", "ccc")) should equal(List("a", "bb"))
    FunctionalII.no34(List("a", "bb", "ccc", "dddd")) should equal(List("a", "bb"))
    FunctionalII.no34(List("ccc", "dddd", "apple")) should equal(List("apple"))
  }

  """Given a list of strings, method "noYY"""" should """return a list where each string has "y" added at its end, omitting any resulting strings that contain "yy" as a substring anywhere.""" in {
    FunctionalII.noYY(List("a", "b", "c")) should equal(List("ay", "by", "cy"))
    FunctionalII.noYY(List("a", "b", "cy")) should equal(List("ay", "by"))
    FunctionalII.noYY(List("xx", "ya", "zz")) should equal(List("xxy", "yay", "zzy"))
  }

  """Given a list of non-negative integers, method "two2"""" should """return a list of those numbers multiplied by 2, omitting any of the resulting numbers that end in 2.""" in {
    FunctionalII.two2(List(1, 2, 3)) should equal(List(4, 6))
    FunctionalII.two2(List(2, 6, 11)) should equal(List(4))
    FunctionalII.two2(List(0)) should equal(List(0))
  }

  """Given a list of integers, method "square56""" should """return a list of those numbers squared and the product added to 10, omitting any of the resulting numbers that end in 5 or 6.""" in {
    FunctionalII.square56(List(3, 1, 4)) should equal(List(19, 11))
    FunctionalII.square56(List(1)) should equal(List(11))
    FunctionalII.square56(List(2)) should equal(List(14))
  }

}
