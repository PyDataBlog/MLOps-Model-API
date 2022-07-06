fn main() {
    let x = &5;

    println!("Hello, world!");

    match x {
        &y => println!("1. {:?}", y),
    }

    match x {
        ref y => println!("2. {:p} -> {:?}", y, y),
    }

    struct Point {
        x : i32,
        y : i32,
    }

    let origin = Point { x: 0, y: 10  };

    match origin {
        Point { x: a, y : b, .. } => println!("3. Point of {}, {}", a, b),
    }
}
