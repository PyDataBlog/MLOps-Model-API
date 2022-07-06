// main.rs

// include external crates
extern crate color;
extern crate time;
extern crate cmq;

// use these functions
use color::{Rgb, ToHsv};

// main
fn main() {

    println!("Converting RGB to HSV!");
    let red = Rgb::new(255u8, 0, 0);
    println!("HSV: {:?}", red.to_hsv::<f32>());
    
    let m = "2";
    cmq::pretty_print(m);
}
