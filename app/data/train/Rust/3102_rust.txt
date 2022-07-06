fn main() {
    // Addition with unsigned integer
    println!("1 + 3 = {}", 1u32 + 3);

    // Subtraction with signed integer
    // Rust refuses to compile if there is an overflow
    println!("1 - 3 = {}", 1i32 - 3);

    // Boolean logic
    println!("true AND false is {}", true && false);
    println!("true OR false is {}", true || false);
    println!("NOT false is {}", !true);

    // Bitwise operations
    println!("0110 AND 0011 is {:04b}", 0b0110u32 & 0b0011);
    println!("0110 OR 0011 is {:04b}", 0b0110u32 | 0b0011);
    println!("0110 XOR 0011 is {:04b}", 0b0110u32 ^ 0b0011);
    println!("1 << 5 is {}", 1u32 << 5);
    println!("0x80 >> 2 is 0x{:x}", 0x80u32 >> 2);

    // Use underscores to improve readibility!
    println!("One million is written as {}", 1_000_000u32);
}
