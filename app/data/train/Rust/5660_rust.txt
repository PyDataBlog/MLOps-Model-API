// Ler um conjunto de quatro valores i,a,b,c onde i é um inteiro
// e positivo e a,b,c são quaisquer valores reais distintos e os
// escreva. A seguir
/*
Se i = 1 escrever os valores a,b,c em ordem crescente
Se i = 2 escrever os valores a,b,c em ordem decrescente
Se i = 3 escrever os valores a,b,c de forma que o maior valor vai
ficar entre os outros dois
*/



#[macro_use]
extern crate text_io;

fn main() {
    loop {
        println!(".:Entre com o valor para I inteiro no intervalor [1,3].:");
        let i: u8 = read!();
        if !(i > 0 && i < 4) {
            continue;
        }
        println!(".:Entre com qualquer valor real para A.:");
        let a: f32 = read!();
        println!(".:Entre com qualquer valor real para B.:");
        let b: f32 = read!();
        println!(".:Entre com qualquer valor real para C.:");
        let c: f32 = read!();
        match i {
            1 => {
                if a > b && b > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, b, a);
                } else if a > b && a > c && c > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, c, a);
                } else if b > a && a > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, a, b);
                } else if b > a && b > c && c > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, c, b);
                } else if c > a && a > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, a, c);
                } else if c > a && c > b && b > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, b, c);
                }
            }
            2 => {
                if a > b && b > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, b, c);
                } else if a > b && a > c && c > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, c, b);
                } else if b > a && a > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, a, c);
                } else if b > a && b > c && c > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, c, a);
                } else if c > a && a > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, a, b);
                } else if c > a && c > b && b > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, b, a);
                }
            }
            _ => {
                if a > b && b > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, a, c);
                } else if a > b && a > c && c > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, a, b);
                } else if b > a && a > c {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, b, c);
                } else if b > a && b > c && c > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, c, b, a);
                } else if c > a && a > b {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, a, c, b);
                } else if c > a && c > b && b > a {
                    println!("Para I = {:?}, temos {:?}, {:?}, {:?}", i, b, c, a);
                }
            }
        }
        break;
    }
}
