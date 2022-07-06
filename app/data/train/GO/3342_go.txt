package main

func main() {

	fn := func() { println("Hello, World!") }

	fn()

	fns := [](func(x int) int){
		func(x int) int { return x + 1 },
		func(x int) int { return x + 2 },
	}

	println(fns[1](100))

	d := struct {
		fn func() string
	}{
		fn: func() string { return "Hello, World!" },
	}

	println(d.fn())

	fc := make(chan func() string, 2)
	fc <- func() string { return "Hello, World!" }
	println((<-fc)())
}
