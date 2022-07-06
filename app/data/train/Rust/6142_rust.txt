#![feature(test)]

mod lib;

fn main() {
    //let sum = summ_iter();
    let sum = lib::summ_while();
    println!("Sum: {}", sum);
}
