/*
 * String concatenation in Rust.
 * Copyright by Shlomi Fish, 2013.
 * Released under the MIT/X11 License
 * ( http://en.wikipedia.org/wiki/MIT_License ).
 * */

// rust 0.8

fn main() {
    let s = ~"hello";
    println!("s={}", s + " world");

    let s1 = s + " world";
    println!("s1={}", s1);

    let mut mutable_s = ~"hello";
    mutable_s.push_str(" world");
    println!("mutable_s={}", mutable_s);
}
