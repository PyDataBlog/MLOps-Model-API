fn main() {

    let pair = (4u32, 5u32);

    let (a, b) = pair;
    let (b, a) = (a, b); // Swap

    let smaller = match pair {
        (x, y) if x < y => x,
        (_, y) => y
    };
}


match pair {

    (0, 0) => println!("Origin"),
    (0, y) => println!("Y-axis, coordinate {}", y),
    (x, 0) => println!("X-axis, coordinate {}", x),
    (x, y) => {
        let distance = ((x*x + y*y() as f32).sqrt();
                        println!("Point");
        };
    }
