// The MIT License (MIT)
// Copyright (c) 2014 Philipp Neugebauer
package main

import (
    "bufio"
    "fmt"
    "os"
    "math/rand"
    "strconv"
)

func computer(inputChannel chan int, resultChannel chan string){
    for human_choice := range inputChannel {
    	computer_choice := rand.Intn(3)
    	evaluation(computer_choice, human_choice, resultChannel)
    }
}

func evaluation(computer_choice int, human_choice int, resultChannel chan string){
	switch human_choice {
	case 0:
		switch computer_choice {
		case 0:
			resultChannel <- "draw"
		case 1:
			resultChannel <- "loss"
		case 2:
			resultChannel <- "win"
		}
	case 1:
		switch computer_choice {
		case 0:
			resultChannel <- "win"
		case 1:
			resultChannel <- "draw"
		case 2:
			resultChannel <- "loss"
		}
	case 2:
		switch computer_choice {
		case 0:
			resultChannel <- "loss"
		case 1:
			resultChannel <- "win"
		case 2:
			resultChannel <- "draw"
		}
	default:
		resultChannel <- "Only numbers between 0 and 2 are valid!"
	}
	close(resultChannel)
}

func main() {
	computerChannel := make(chan int)
	resultChannel := make(chan string)
	go computer(computerChannel, resultChannel)
	reader := bufio.NewReader(os.Stdin)
	fmt.Println("Choose: \n 0 = rock\n 1 = paper\n 2 = scissors")
	text, _ := reader.ReadString('\n')
	choice, _ := strconv.Atoi(text)
	computerChannel <- choice
	close(computerChannel)
	for message := range resultChannel {
        fmt.Println("Result:", message)
    }
}

