package main

import "fmt"

func main() {
	x, y := 10, 4
	fmt.Println("Original x, y: ", x, y)

	swap(&x, &y)
	fmt.Println("After swap x, y: ", x, y)
}

func swap(x, y *int) {
	temp := *x
	*x = *y
	*y = temp
}
