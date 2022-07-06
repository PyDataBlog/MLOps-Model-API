fn main() {

    fn plus_one(x: &int) -> int {
        *x + 1
    }


    let y = Box::new(10i);

    println!("{}", plus_one(y));
}
