package main

import "fmt"

// Calculate the n^th Fibonacci number.
func fib(n int) int {
	if n < 2 {
		return n
	} else {
		return fib(n-1) + fib(n-2)
	}
}

func main() {
	res := fib(20)
	fmt.Println(res)
}
