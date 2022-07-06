package closure

import (
	"fmt"
)

func wrapper() func() int {
	x := 0

	increment := func() int {
		x++ //x is enclosed in inner block scope to reduce the scope of x being used at block level only. Hence the closure
		return x
	}
	return increment
}

func bye() {
	fmt.Println("good bye")
}

func IncrementTest() {
	defer bye() //defer keyword is used to defer(delay till function exit) when this func ends. Handy when file needs to be closed e.g : defer.fileclose
	incrementer := wrapper()
	fmt.Println(incrementer())
	fmt.Println(incrementer())
}
