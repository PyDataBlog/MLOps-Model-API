pub fn bottle_or_bottles(n: i32) -> &'static str {
    match n {
        1 => "bottle",
        _ => "bottles",
    }
}

pub fn sing(n: i32) {
    for i in (1..n + 1).rev() {
        println!(
            "{0} {1} of beer on the wall, {0} {1} of beer.",
            i,
            bottle_or_bottles(i)
        );
        println!(
            "Take one down and pass it around, {0} {1} of beer on the wall!",
            i - 1,
            bottle_or_bottles(i - 1)
        );
        println!();
    }
    println!("No more bottles of beer on the wall, no more bottles of beer.");
    println!(
        "Go to the store and buy some more, {0} bottles of beer on the wall.",
        n
    );
}
