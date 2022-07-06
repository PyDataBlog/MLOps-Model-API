package org.au9ustine.puzzles.s99

import org.scalatest.{Matchers, FlatSpec}

/**
 * Problem 16 Unit Tesitng
 *
 * Created by au9ustine on 10/17/2015.
 */
class P16$Test extends FlatSpec with Matchers {

  "P16.duplicateN" should "duplicate the elements of a list a given number of times"  in {
    val (n, lst) = (3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val expected = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    P16.drop(n, lst) should be (expected)
  }

}
