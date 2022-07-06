/*
    recu.rs
    demonstrates enums, recursive datatypes, and methdos
    vanilla
*/

fn main() {
    let list = ~Node(1, ~Node(2, ~Node(3, ~Empty)));
    let list2 = ~Node(2, ~Node(3, ~Node(5, ~Empty)));
    println!("Sum of all values in the list: {:i}.", list.sum());
    println!("Product of all values in the list {:i}.",list2.prod());
}

enum IntList {
    Node(int, ~IntList),
    Empty
}

impl IntList {
    fn sum(~self) -> int {
        // As in C and C++, pointers are dereferenced with the asterisk `*` operator.
        match *self {
            Node(value, next) => value + next.sum(),
            Empty => 0
        }
    }
    fn prod(~self) -> int {
        match *self {
            Node(value,next) => value * next.prod(),
            Empty => 1
        }
    }
}
