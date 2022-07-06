fn fib(n: i64) -> i64 {
    if n == 0 {
        0
    } else if n == 1 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    let argument = 10;

    println!("fib({}) = {}", argument, fib(argument));
}
