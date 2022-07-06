

fn main() {
    let num = 1000;

    for b in 1..num {
        let a = (1000000 - 2000*b) / (2000-2*b);
        let c = 1000 - a - b;

        if a*a + b*b == c*c {
            println!("The numbers are {}, {}, {}", a, b, c);
            println!("The product abc = {}", a*b*c);
            break;
        }
    }
}
