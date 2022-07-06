
class Solution(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        inspected_dict = {}

        for i, num in enumerate(nums):
            try:
                j = inspected_dict[num]
                return j+1, i+1
            except KeyError:
                inspected_dict[target-num] = i