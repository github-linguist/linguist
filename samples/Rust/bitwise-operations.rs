fn main() {
    let a: u8 = 105;
    let b: u8 = 91;
    println!("a      = {:0>8t}", a);
    println!("b      = {:0>8t}", b);
    println!("a | b  = {:0>8t}", a | b);
    println!("a & b  = {:0>8t}", a & b);
    println!("a ^ b  = {:0>8t}", a ^ b);
    println!("!a     = {:0>8t}", !a);
    println!("a << 3 = {:0>8t}", a >> 3);
    println!("a >> 3 = {:0>8t}", a << 3);
}
