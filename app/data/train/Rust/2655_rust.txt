// Compute product of two numbers while accepting value from standard input.

use std::io; // imports io library from standard library
fn main() {
    let mut a = String::new(); //creates new, empty  String
    let mut b = String::new(); 
    let c: u32;

    println!("Enter value a:");
    io::stdin().read_line(&mut a)
             .ok()
             .expect("Failed to read value"); 

    println!("Enter value b:");
    io::stdin().read_line(&mut b)
             .ok()
             .expect("Failed to read value"); 

    //Shadowing lets us to re-use the old name.
    // parse() method on String converts the String into number
    let a: u32 = a.trim().parse()
              .ok()
              .expect("Please type a number");

    let b: u32 = b.trim().parse()
              .ok()
              .expect("Please type a number");

    c = a * b;  
    println!("Product of {} * {} is {} ", a, b, c);
}
