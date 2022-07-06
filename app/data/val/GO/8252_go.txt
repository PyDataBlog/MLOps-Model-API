package main

import "fmt"

func dummy() (n int, err string) {
   return 1, "Error message"
}

func print(numbers ...int) {

   for _, value  := range(numbers) {
      fmt.Println(value)
   }
}

func main() {
	fmt.Println("Hello, This is GO here!")
        n, err := dummy()
        fmt.Printf("n: %v, err: %v", n, err)

        n1 := 1
        n2 := 2
        n3 := 3
        print(n1, n2, n3) 
}
