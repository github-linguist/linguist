extern crate foo;
extern crate bar;

use foo::{self, quix};
use bar::car::*;
use bar;

fn main() {
    println!("Hello {}", "World");

    panic!("Goodbye")
}
