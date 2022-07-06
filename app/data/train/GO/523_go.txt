package main

import (
	"fmt"
)

type myType int

func (t myType) println() {
	fmt.Println(t)
}

func main() {
	var z myType = 123

	z.println()
}
