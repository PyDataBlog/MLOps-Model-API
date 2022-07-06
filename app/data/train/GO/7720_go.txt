package main

import (
    "fmt"
    "time"
    "sync"
)


func workerNoRetrieve(ch chan int) {
    go func(ch chan int) {
        ch <- 23
    }(ch)

    //not best retrieve after goroutine self invocation then return
}


func workerAct(ch chan int, x int, y int) int {
    go func(c chan int) {
         sum := x+y
         ch <- sum
    }(ch)

    value := <-ch
    return value
}


func main() {
    channel := make(chan int)
    fmt.Println("Captured val is: ",workerAct(channel, 10, 10))
    workerNoRetrieve(channel)
    fmt.Println(worker2(channel, 23, 43))

    time.Sleep(2000) //ensure the goroutines run
}


func worker2(ch chan int, x int, y int) int {
    go func(c chan int) {
        sum := x+y
        ch <- sum
    }(ch)

    value := <-ch
    return value
}
