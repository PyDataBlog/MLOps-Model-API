#!/usr/bin/env python

################################################################################
#
# Project Euler - Problem 6
#
# The sum of the squares of the first ten natural numbers is,
#
# 1^2 + 2^2 + ... + 10^2 = 385
# The square of the sum of the first ten natural numbers is,
#
# (1 + 2 + ... + 10)^2 = 552 = 3025
# Hence the difference between the sum of the squares of the first ten natural
# numbers and the square of the sum is 3025 - 385 = 2640
#
# Find the difference between the sum of the squares of the first one hundred
# natural numbers and the square of the sum.
#
# Joaquin Derrac - carrdelling@gmail.com
#
################################################################################


if __name__ == "__main__":

    sum_one_hundred = sum([x for x in range(1, 101)])
    sum_one_hundred_squared = sum_one_hundred * sum_one_hundred

    sum_squared = sum([x ** 2 for x in range(1, 101)])

    solution = sum_one_hundred_squared - sum_squared

    print(solution)
