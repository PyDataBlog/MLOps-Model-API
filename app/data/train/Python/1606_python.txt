#!/usr/bin/env python3

import unittest
import greatest_common_divisor as gcd


class TestGreatestCommonDivisor(unittest.TestCase):

    def setUp(self):
        # use tuple of tuples instead of list of tuples because data won't change
        # https://en.wikipedia.org/wiki/Algorithm
        # a, b, expected
        self.test_data = ((12, 8, 4),
                          (9, 12, 3),
                          (54, 24, 6),
                          (3009, 884, 17),
                          (40902, 24140, 34),
                          (14157, 5950, 1)
                          )

    def test_greatest_common_divisor_zero(self):
        actual = gcd.GreatestCommonDivisor.greatest_common_divisor(12, 0)
        self.assertEqual(0, actual)
        actual = gcd.GreatestCommonDivisor.greatest_common_divisor(0, 13)
        self.assertEqual(0, actual)
        actual = gcd.GreatestCommonDivisor.greatest_common_divisor(-5, 13)
        self.assertEqual(0, actual)

    def test_greatest_common_divisor(self):
        for test_case in self.test_data:
            expected = test_case[2]
            actual = gcd.GreatestCommonDivisor.greatest_common_divisor(test_case[0], test_case[1])
            fail_message = str.format("expected {0} but got {1}", expected, actual)
            self.assertEqual(expected, actual, fail_message)

    def test_next_smaller_divisor(self):
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(8, 8)
        self.assertEqual(4, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 12)
        self.assertEqual(6, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 6)
        self.assertEqual(4, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 4)
        self.assertEqual(3, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 3)
        self.assertEqual(2, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 2)
        self.assertEqual(1, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(12, 1)
        self.assertEqual(1, actual)
        actual = gcd.GreatestCommonDivisor.next_smaller_divisor(54, 18)
        self.assertEqual(9, actual)


if __name__ == "__main__":
    unittest.main()
