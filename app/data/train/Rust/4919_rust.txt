extern crate FizzBuzz;

fn main() {
    for r in (1..101).map(FizzBuzz::fizz_buzz) {
        println!("{}", r);
    }
}
