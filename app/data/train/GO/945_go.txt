package main

// list all of the multipels of m below max and returns them on the ret channel
func listMultiples(m, max int, ret chan int) {
	if m == 0 {
		return
	}
	count := 0
	tmp := 0
	for {
		tmp = count * m
		if tmp >= max {
			break
		}

		count++

		ret <- tmp
	}

	close(ret)

}

// range over each channel until they have been closed. and return the sum of all values returned
func collect(c []chan int) int {
	r := make(chan int)
	intSet := make(map[int]struct{})

	// fan in the recieved values
	for i := range c {
		go func(f chan int) {
			tmp := 0
			for tmp = range f {
				r <- tmp
			}
			r <- -1
		}(c[i])
	}

	i := 0
	tmp := 0

	// sum values recieved on r until the done signal (-1) has been recieved by all
	for i < len(c) {
		tmp = <-r
		if tmp == -1 {
			i++
		} else {
			intSet[tmp] = struct{}{}
		}
	}

	// add up all of the keys in the set
	sum := 0
	for k, _ := range intSet {
		sum += k
	}

	return sum
}

func main() {
	r := []chan int{
		make(chan int),
		make(chan int),
	}

	go listMultiples(3, 1000, r[0])
	go listMultiples(5, 1000, r[1])

	print(collect(r))

}
