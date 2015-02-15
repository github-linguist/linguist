extern crate num;

use num::complex::Cmplx;

fn main() {
    let a = Cmplx::new(-4.0, 5.0);
    let b = Cmplx::new(1.0, 1.0);

    println!("a = {}", a);
    println!("b = {}", b);
    println!("a + b = {}", a + b);
    println!("a * b = {}", a * b);
    println!("1 / a = {}", Cmplx::new(1.0, 0.0) / a);
    println!("-a = {}", -a);
    println!("conj a = {}", a.conj());
}
