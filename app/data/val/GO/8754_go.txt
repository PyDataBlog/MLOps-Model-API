package main

import "fmt"

func main() {
	nums := []int{1, 2}
	nextPermutation(nums)
	fmt.Println(nums)

}

func nextPermutation(nums []int) {

	if len(nums) < 2 {
		return
	}
	flagPosition := 0
	for i := len(nums) - 1; i > 0; i-- {
		if nums[i-1] < nums[i] {
			flagPosition = i
			break
		}
	}

	if flagPosition != 0 {
		flagPosition2 := len(nums) - 1
		for i := flagPosition; i < len(nums); i++ {
			if nums[i] <= nums[flagPosition-1] {
				flagPosition2 = i - 1
				break
			}
		}
		nums[flagPosition-1], nums[flagPosition2] = nums[flagPosition2], nums[flagPosition-1]
	}

	for i, j := flagPosition, len(nums)-1; i < j; {
		nums[i], nums[j] = nums[j], nums[i]
		i++
		j--
	}
}
