# -*- coding: utf-8 -*-
"""
Given an array of integers, every element appears twice except for one. Find
that single one.

Note:
Your algorithm should have a linear runtime complexity. Could you implement it
without using extra memory?
"""

__author__ = 'yelongyu1024@gmail.com'


class Solution(object):
    def __init__(self, nums):
        self.nums = nums

    def single_number(self):
        num_count = {}
        for num in self.nums:
            num_count.setdefault(num, 0)
            num_count[num] = num_count.get(num) + 1
        print num_count
        for key, value in num_count.items():
            if value == 1:
                return key


def main():
    solution = Solution([2, 3, 5, 7, 9, 3, 5, 7, 9])
    print solution.single_number()


if __name__ == '__main__':
    main()
